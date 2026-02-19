# randomization.R
library(dplyr)
library(tibble)
library(DBI)

N_BOX     <- 140
N_SCATTER <- 18

D_PER_PERSON <- 4
METHODS      <- c("Talk", "Highlight", "Summary")

PERMS <- list(
  c("Talk","Highlight","Summary"),
  c("Talk","Summary","Highlight"),
  c("Highlight","Talk","Summary"),
  c("Highlight","Summary","Talk"),
  c("Summary","Talk","Highlight"),
  c("Summary","Highlight","Talk")
)


# Assign 3 box + 1 scatter randomly
assign_datasets_random <- function(con, participant_id, session_id,
                                   n_box = N_BOX, n_scatter = N_SCATTER,
                                   D = D_PER_PERSON) {
  
  # If already fully assigned, reuse
  existing <- dbGetQuery(con, "
    SELECT slot, dataset_id, plot_type
    FROM participant_lineup_set
    WHERE participant_id = ? AND session_id = ?
    ORDER BY slot
  ", params = list(participant_id, session_id))
  
  if (nrow(existing) == D) {
    return(as_tibble(existing))
  }
  
  # Transaction protects against concurrent writes / refresh mid-assign
  dbExecute(con, "BEGIN IMMEDIATE;")
  on.exit(try(dbExecute(con, "COMMIT;"), silent = TRUE), add = TRUE)
  
  # Re-check inside transaction
  existing2 <- dbGetQuery(con, "
    SELECT slot, dataset_id, plot_type
    FROM participant_lineup_set
    WHERE participant_id = ? AND session_id = ?
    ORDER BY slot
  ", params = list(participant_id, session_id))
  
  if (nrow(existing2) == D) {
    return(as_tibble(existing2))
  }
  
  # If partial rows exist, delete them and rebuild cleanly
  if (nrow(existing2) > 0 && nrow(existing2) < D) {
    dbExecute(con, "
      DELETE FROM participant_lineup_set
      WHERE participant_id = ? AND session_id = ?;
    ", params = list(participant_id, session_id))
  }
  
  # Pick 3 distinct box IDs and 1 distinct scatter ID
  box_ids <- sample.int(n_box, size = 3, replace = FALSE)
  sc_id   <- sample.int(n_scatter, size = 1, replace = FALSE)
  
  # Store scatter as unique ID > n_box
  chosen_id   <- c(box_ids, n_box + sc_id)
  chosen_type <- c(rep("box", 3), "scatter")
  
  # Shuffle the 4 stimuli order
  ord <- sample.int(D)
  chosen_id   <- chosen_id[ord]
  chosen_type <- chosen_type[ord]
  
  now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S%z")
  
  df <- tibble(
    participant_id = participant_id,
    session_id     = session_id,
    slot           = 1:D,
    dataset_id     = chosen_id,
    plot_type      = chosen_type,
    assigned_at    = now
  )
  
  dbWriteTable(con, "participant_lineup_set", df, append = TRUE, row.names = FALSE)
  
  df
}

# Build 12-trial schedule (4 stimuli × 3 methods)
make_schedule <- function(pid_index, assigned_df) {
  
  assigned_df <- as_tibble(assigned_df)
  
  ds_vec4 <- assigned_df$dataset_id
  pt_vec4 <- assigned_df$plot_type
  
  perm_id <- ((pid_index + seq_along(ds_vec4) - 2) %% length(PERMS)) + 1
  meth_for_dataset <- lapply(perm_id, function(k) PERMS[[k]])
  
  # Randomize stimulus order once - no adjacent repeats
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
