# randomization.R
library(dplyr)
library(tibble)
library(DBI)

D_PER_PERSON <- 4
METHODS      <- c("Talk", "Highlight", "Text")

PERMS <- list(
  c("Talk","Highlight","Text"),
  c("Talk","Text","Highlight"),
  c("Highlight","Talk","Text"),
  c("Highlight","Text","Talk"),
  c("Text","Talk","Highlight"),
  c("Text","Highlight","Talk")
)

# Helper: choose k IDs from the least-used datasets
choose_least_used_ids <- function(con, ids, plot_type, k) {
  ids <- sort(unique(as.integer(ids)))
  
  used <- dbGetQuery(con, "
    SELECT dataset_id, COUNT(*) AS n_used
    FROM participant_lineup_set
    WHERE plot_type = ?
    GROUP BY dataset_id
  ", params = list(plot_type))
  
  usage <- tibble(dataset_id = ids) %>%
    left_join(as_tibble(used), by = "dataset_id") %>%
    mutate(n_used = ifelse(is.na(n_used), 0L, as.integer(n_used))) %>%
    arrange(n_used, dataset_id)
  
  # smallest usage levels first
  levs <- sort(unique(usage$n_used))
  
  pool_ids <- integer(0)
  for (lv in levs) {
    pool_ids <- usage$dataset_id[usage$n_used <= lv]
    if (length(pool_ids) >= k) break
  }
  
  sample(pool_ids, size = k, replace = FALSE)
}


# Assign 3 box + 1 scatter
assign_datasets_balanced <- function(con, participant_id, session_id,
                                     box_ids_all, scatter_ids_all,
                                     D = D_PER_PERSON) {
  
  # reuse if already fully assigned
  existing <- dbGetQuery(con, "
    SELECT slot, dataset_id, plot_type
    FROM participant_lineup_set
    WHERE participant_id = ? AND session_id = ?
    ORDER BY slot
  ", params = list(participant_id, session_id))
  
  if (nrow(existing) == D) {
    return(as_tibble(existing))
  }
  
  dbExecute(con, "BEGIN IMMEDIATE;")
  committed <- FALSE
  on.exit({
    if (!committed) {
      try(dbExecute(con, "ROLLBACK;"), silent = TRUE)
    }
  }, add = TRUE)
  
  # recheck inside transaction
  existing2 <- dbGetQuery(con, "
    SELECT slot, dataset_id, plot_type
    FROM participant_lineup_set
    WHERE participant_id = ? AND session_id = ?
    ORDER BY slot
  ", params = list(participant_id, session_id))
  
  if (nrow(existing2) == D) {
    dbExecute(con, "COMMIT;")
    committed <- TRUE
    return(as_tibble(existing2))
  }
  
  # clear partial assignment if any
  if (nrow(existing2) > 0 && nrow(existing2) < D) {
    dbExecute(con, "
      DELETE FROM participant_lineup_set
      WHERE participant_id = ? AND session_id = ?
    ", params = list(participant_id, session_id))
  }
  
  # choose least-used datasets
  chosen_box <- choose_least_used_ids(
    con = con,
    ids = box_ids_all,
    plot_type = "box",
    k = 3
  )
  
  chosen_sc <- choose_least_used_ids(
    con = con,
    ids = scatter_ids_all,
    plot_type = "scatter",
    k = 1
  )
  
  chosen_id   <- c(chosen_box, chosen_sc)
  chosen_type <- c(rep("box", length(chosen_box)), rep("scatter", length(chosen_sc)))
  
  # shuffle slot order of the 4 chosen datasets
  ord <- sample.int(length(chosen_id))
  chosen_id   <- chosen_id[ord]
  chosen_type <- chosen_type[ord]
  
  now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S%z")
  
  df <- tibble(
    participant_id = participant_id,
    session_id     = session_id,
    slot           = seq_along(chosen_id),
    dataset_id     = chosen_id,
    plot_type      = chosen_type,
    assigned_at    = now
  )
  
  dbWriteTable(con, "participant_lineup_set", df, append = TRUE, row.names = FALSE)
  
  dbExecute(con, "COMMIT;")
  committed <- TRUE
  
  df
}


# Build 12-trial schedule: 4 datasets × 3 methods
# Each dataset gets all 3 methods exactly once
make_schedule <- function(pid_index, assigned_df) {
  assigned_df <- as_tibble(assigned_df)
  
  ds_vec4 <- assigned_df$dataset_id
  pt_vec4 <- assigned_df$plot_type
  
  # rotate method order across datasets / participants
  perm_id <- ((pid_index + seq_along(ds_vec4) - 2) %% length(PERMS)) + 1
  meth_for_dataset <- lapply(perm_id, function(k) PERMS[[k]])
  
  # randomize the 4 datasets once
  ord <- sample(seq_along(ds_vec4))
  
  out <- tibble()
  k <- 1
  
  for (r in seq_along(METHODS)) {
    for (j in ord) {
      out <- bind_rows(out, tibble(
        trial_n    = k,
        dataset_id = ds_vec4[j],
        plot_type  = pt_vec4[j],
        method     = meth_for_dataset[[j]][r]
      ))
      k <- k + 1
    }
  }
  
  out
}