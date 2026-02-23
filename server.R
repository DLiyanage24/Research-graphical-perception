library(shiny)
library(r2d3)
library(shinyjs)
library(tibble)
library(dplyr)
library(DBI)
library(RSQLite)
library(jsonlite)
# install.packages("devtools")
# devtools::install_github("abbey-thomas/speechcollectr")
library(speechcollectr)

# randomization helpers
source("randomization.R")

# Paths
DATA_DIR <- file.path("data")
REC_DIR <- file.path(DATA_DIR, "recordings")
DB_PATH <- file.path(DATA_DIR, "app_data.sqlite")

`%||%` <- function(a, b) if (!is.null(a)) a else b

# DB setup
init_db <- function() {
  con <- dbConnect(RSQLite::SQLite(), DB_PATH)

  # SQLite settings for better concurrency
  dbExecute(con, "PRAGMA journal_mode = WAL;")
  dbExecute(con, "PRAGMA busy_timeout = 5000;")

  # recordings
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS recordings (
      id             INTEGER PRIMARY KEY AUTOINCREMENT,
      participant_id TEXT    NOT NULL,
      session_id     TEXT    NOT NULL,
      trial_n        INTEGER NOT NULL,
      saved_at       TEXT    NOT NULL,
      filename       TEXT    NOT NULL,
      filepath       TEXT    NOT NULL,
      lineup_file    TEXT    NOT NULL,
      sel1           INTEGER,
      sel2           INTEGER
    )
  ")

  # highlight data
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS highlighted_regions (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      participant_id TEXT,
      session_id     TEXT,
      timestamp      TEXT,
      trial_n        INTEGER,
      lineup_file    TEXT,
      plotIndex      INTEGER,
      selected_json  TEXT,
      n_selected     INTEGER,
      draw_started_at_ms REAL,
      draw_ended_at_ms   REAL,
      draw_duration_ms   REAL
    )
  ")

  # text summary
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS text_summary (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      participant_id TEXT,
      session_id     TEXT,
      timestamp      TEXT,
      trial_n        INTEGER,
      lineup_file    TEXT,
      plotIndex      INTEGER,
      reasoning      TEXT
    )
  ")

  # clicks - logs all panel click interactions
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS clicks (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      participant_id TEXT,
      session_id     TEXT,
      timestamp      TEXT,
      trial_n        INTEGER,
      lineup_file    TEXT,
      plotIndex      INTEGER,
      selection_rank INTEGER,
      action         TEXT,
      time_from_start_sec REAL
    )
  ")

  # demographics
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS demographics (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      participant_id TEXT,
      session_id     TEXT,
      timestamp      TEXT,
      age_range      TEXT,
      education_level TEXT,
      gender_identity  TEXT,
      color_blind      TEXT
    )
  ")

  # participants- one row per participant session
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS participants (
      pid_index      INTEGER PRIMARY KEY AUTOINCREMENT,
      participant_id TEXT NOT NULL,
      session_id     TEXT NOT NULL,
      started_at     TEXT NOT NULL,
      ended_at       TEXT,
      completed      INTEGER DEFAULT 0
    )
  ")

  # ensure uniqueness: one participants row per (participant_id, session_id)
  dbExecute(con, "
    CREATE UNIQUE INDEX IF NOT EXISTS idx_participants_unique
    ON participants(participant_id, session_id);
  ")

  # datasets assignment
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS participant_lineup_set (
      participant_id TEXT NOT NULL,
      session_id     TEXT NOT NULL,
      slot           INTEGER NOT NULL,
      dataset_id     INTEGER NOT NULL,
      plot_type      TEXT NOT NULL,
      assigned_at    TEXT NOT NULL,
      PRIMARY KEY (participant_id, session_id, slot)
    )
  ")

  # full full schedule
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS trial_plan (
      participant_id TEXT NOT NULL,
      session_id     TEXT NOT NULL,
      trial_n        INTEGER NOT NULL,
      dataset_id     INTEGER NOT NULL,
      plot_type      TEXT NOT NULL,
      method         TEXT NOT NULL,
      lineup_file    TEXT NOT NULL,
      PRIMARY KEY (participant_id, session_id, trial_n)
    )
  ")

  # trial_timing
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS trial_timing (
      id             INTEGER PRIMARY KEY AUTOINCREMENT,
      participant_id TEXT NOT NULL,
      session_id     TEXT NOT NULL,
      trial_n        INTEGER NOT NULL,
      method         TEXT,
      started_at     TEXT,
      ended_at       TEXT,
      duration_sec   REAL,
      UNIQUE(participant_id, session_id, trial_n)
    )
  ")

  # helper - add columns if missing
  ensure_cols <- function(con, table, cols) {
    existing <- dbGetQuery(con, paste0("PRAGMA table_info(", table, ");"))$name
    for (nm in names(cols)) {
      if (!nm %in% existing) {
        dbExecute(con, sprintf("ALTER TABLE %s ADD COLUMN %s %s;", table, nm, cols[[nm]]))
      }
    }
  }

  # keep old DBs compatible (adds new columns if needed)
  ensure_cols(con, "text_summary", list(trial_n = "INTEGER", lineup_file = "TEXT", reasoning = "TEXT"))
  ensure_cols(con, "clicks", list(lineup_file = "TEXT"))
  ensure_cols(con, "highlighted_regions", list(lineup_file = "TEXT"))
  ensure_cols(con, "trial_timing", list(method = "TEXT", started_at = "TEXT", ended_at = "TEXT", duration_sec = "REAL"))
  ensure_cols(con, "participant_lineup_set", list(plot_type = "TEXT"))
  ensure_cols(con, "trial_plan", list(plot_type = "TEXT"))
  ensure_cols(con, "participants", list(ended_at = "TEXT", completed = "INTEGER DEFAULT 0"))
  ensure_cols(con, "demographics", list(timestamp = "TEXT", age_range = "TEXT", education_level = "TEXT", gender_identity = "TEXT", color_blind = "TEXT"))
  ensure_cols(con, "recordings", list(sel1 = "INTEGER", sel2 = "INTEGER"))

  invisible(con)
}

# helper - append a tibble/data.frame into a table (type cleanup)
append_df <- function(con, table, df) {
  if (!nrow(df)) {
    return(invisible(TRUE))
  }
  for (nm in names(df)) {
    if (inherits(df[[nm]], "POSIXt")) {
      df[[nm]] <- format(df[[nm]], "%Y-%m-%d %H:%M:%S")
    } else if (is.logical(df[[nm]])) df[[nm]] <- as.integer(df[[nm]])
  }
  dbWriteTable(con, table, df, append = TRUE, row.names = FALSE)
  invisible(TRUE)
}

# server logic
server <- function(input, output, session) {
  # ensure folders exist
  dir.create(DATA_DIR, showWarnings = FALSE, recursive = TRUE)
  dir.create(REC_DIR, showWarnings = FALSE, recursive = TRUE)

  # DB connection
  con <- init_db()

  # generate unique IDs for this session
  participant_id <- paste0("P-", format(Sys.time(), "%Y%m%d%H%M%S"), "-", substr(session$token, 1, 6))
  session_id <- paste0("S-", format(Sys.time(), "%Y%m%dT%H%M%S"), "-", substr(session$token, 1, 6))

  # session-level start time
  sessionStart <- reactiveVal(Sys.time())

  # or end-of-session cleanup
  session$userData$participant_id <- participant_id
  session$userData$session_id <- session_id
  session$userData$has_participants_row <- FALSE

  session$onSessionEnded(function() {
    pid <- session$userData$participant_id
    sid <- session$userData$session_id
    now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S%z")

    # only update if we inserted a participants row
    if (isTRUE(session$userData$has_participants_row) && DBI::dbIsValid(con)) {
      try(DBI::dbExecute(con, "
        UPDATE participants
        SET ended_at = COALESCE(ended_at, ?)
        WHERE participant_id = ? AND session_id = ?;
      ", params = list(now, pid, sid)), silent = TRUE)
    }

    if (DBI::dbIsValid(con)) {
      try(DBI::dbDisconnect(con), silent = TRUE)
    }
  })

  # Lineup files + experiment settings
  JS_PATH <- "you-draw-it-svg.js"
  TOTAL_TRIALS <- 12
  D_PER_PERSON <- 4

  img_dir <- "Images"
  box_svgs <- sort(list.files(img_dir, pattern = "^(T5|T6)_boxplot_.*\\.svg$", full.names = FALSE))
  sc_svgs <- sort(list.files(img_dir, pattern = "^SC_scatter_.*\\.svg$", full.names = FALSE))

  # ensure required SVGs exist
  if (length(box_svgs) < 3) stop("Need at least 3 boxplot SVGs in Images/ matching ^(T5|T6)_boxplot_.*\\.svg$")
  if (length(sc_svgs) < 1) stop("Need at least 1 scatter SVGs in Images/ matching ^SC_scatter_.*\\.svg$")

  N_BOX <- length(box_svgs)
  N_SCATTER <- length(sc_svgs)

  #  map (dataset_id, plot_type) -> filename
  dataset_id_to_file <- function(dataset_id, plot_type) {
    if (plot_type == "box") {
      if (dataset_id < 1 || dataset_id > N_BOX) stop("Box dataset_id out of range: ", dataset_id)
      return(box_svgs[dataset_id])
    } else if (plot_type == "scatter") {
      sc_index <- dataset_id - N_BOX
      if (sc_index < 1 || sc_index > N_SCATTER) stop("Scatter dataset_id out of range: ", dataset_id)
      return(sc_svgs[sc_index])
    } else {
      stop("Unknown plot_type: ", plot_type)
    }
  }

  # trial plan stored in reactiveVal after start experiment
  trial_plan_rv <- reactiveVal(NULL)

  make_payload_from_file <- function(fn) list(svg = paste0("/lineups/", fn), n_panels = 20, filename = fn)

  get_trial_payload <- function(trial_n) {
    tp <- trial_plan_rv()
    req(!is.null(tp), nrow(tp) == TOTAL_TRIALS)
    fn <- tp$lineup_file[tp$trial_n == trial_n][1]
    make_payload_from_file(fn)
  }

  # fetch lineup payload for a given trial number
  get_trial_method <- function(trial_n) {
    tp <- trial_plan_rv()
    req(!is.null(tp), nrow(tp) == TOTAL_TRIALS)
    tp$method[tp$trial_n == trial_n][1]
  }

  # Reactive state containers
  rvs <- reactiveValues(trial_n = 0)

  rv_click <- reactiveValues(event_rank = 0)
  last_clicked <- reactiveVal(NULL)
  buf <- reactiveValues(lineup = NULL, regions = list())

  talk_lineup <- reactiveVal(NULL)
  summary_lineup <- reactiveVal(NULL)

  trial_started_at <- reactiveValues()

  # Talk recording gating
  talk_is_recording <- reactiveVal(FALSE)
  talk_rec_started_at <- reactiveVal(NULL)
  talk_has_recorded <- reactiveVal(FALSE) # TRUE only after rec_done
  talk_rec_row_id <- reactiveVal(NA_integer_) # inserted recordings.id
  rec_status_rv <- reactiveVal("")

  # Outputs: recording status + timer
  output$rec_status <- renderText(rec_status_rv())

  output$rec_timer <- renderText({
    if (!isTRUE(talk_is_recording())) {
      return("")
    }
    start <- talk_rec_started_at()
    if (is.null(start)) {
      return("")
    }
    invalidateLater(250, session)
    sec <- as.integer(difftime(Sys.time(), start, units = "secs"))
    sprintf("Recording time: %02d:%02d", sec %/% 60, sec %% 60)
  })

  # Trial counter text
  trial_counter_text <- reactive({
    if (rvs$trial_n <= 0) {
      paste0("Trial 0 of ", TOTAL_TRIALS)
    } else {
      paste0("Trial ", rvs$trial_n, " of ", TOTAL_TRIALS)
    }
  })

  output$trial_counter_talk <- renderText(trial_counter_text())
  output$trial_counter_highlight <- renderText(trial_counter_text())
  output$trial_counter_summary <- renderText(trial_counter_text())

  # Save timing
  save_trial_timing <- function(trial_n, ended_at = Sys.time()) {
    m <- get_trial_method(trial_n)
    started <- trial_started_at[[as.character(trial_n)]] %||% ended_at
    dur <- as.numeric(difftime(ended_at, started, units = "secs"))

    dbExecute(con, "
      INSERT INTO trial_timing (participant_id, session_id, trial_n, method, started_at, ended_at, duration_sec)
      VALUES (?, ?, ?, ?, ?, ?, ?)
      ON CONFLICT(participant_id, session_id, trial_n)
      DO UPDATE SET
        method = excluded.method,
        started_at = excluded.started_at,
        ended_at = excluded.ended_at,
        duration_sec = excluded.duration_sec;
    ", params = list(
      participant_id, session_id, trial_n, m,
      format(started, "%Y-%m-%d %H:%M:%S"),
      format(ended_at, "%Y-%m-%d %H:%M:%S"),
      dur
    ))
  }

  # load the correct tab for the current trial
  go_to_current_trial <- function() {
    req(rvs$trial_n >= 1, rvs$trial_n <= TOTAL_TRIALS)
    m <- get_trial_method(rvs$trial_n)

    # mark trial start time
    trial_started_at[[as.character(rvs$trial_n)]] <- Sys.time()

    if (m == "Talk") {
      showTab("topnav", "Talk")
      updateTabsetPanel(session, "topnav", selected = "Talk")

      talk_lineup(get_trial_payload(rvs$trial_n))

      # reset talk gating
      talk_is_recording(FALSE)
      talk_rec_started_at(NULL)
      talk_has_recorded(FALSE)
      talk_rec_row_id(NA_integer_)
      rec_status_rv("Click “Start recording”. The lineup will appear after recording begins.")

      # reset dropdowns
      updateSelectInput(session, "talk_sel1", selected = "-")
      updateSelectInput(session, "talk_sel2", selected = "-")

      # reset record UI
      shinyjs::enable("rec_start")
      shinyjs::show("rec_start")
      shinyjs::hide("rec_stop")
      shinyjs::enable("rec_stop")

      # show placeholder until recording starts
      shinyjs::show("talk_placeholder")
      shinyjs::hide("talk_lineup_wrap")
    } else if (m == "Highlight") {
      showTab("topnav", "Highlight")
      updateTabsetPanel(session, "topnav", selected = "Highlight")

      # reset highlight buffers
      buf$lineup <- get_trial_payload(rvs$trial_n)
      buf$regions <- list()
      last_clicked(NULL)
      rv_click$event_rank <- 0

      # render D3 in highlight mode
      output$lineup_highlight <- renderD3({
        r2d3(
          data = buf$lineup,
          script = JS_PATH,
          options = list(mode = "highlight", annotation = FALSE)
        )
      })
    } else if (m == "Summary") {
      showTab("topnav", "Summary")
      updateTabsetPanel(session, "topnav", selected = "Summary")

      # set lineup payload
      summary_lineup(get_trial_payload(rvs$trial_n))

      # render D3 in plain mode
      output$lineup_summary <- renderD3({
        r2d3(
          data = summary_lineup(),
          script = JS_PATH,
          options = list(mode = "plain", annotation = FALSE)
        )
      })
      # reset Summary inputs
      updateSelectInput(session, "sum_sel1", selected = "-")
      updateSelectInput(session, "sum_sel2", selected = "-")
      updateTextAreaInput(session, "sum_why1", value = "")
      updateTextAreaInput(session, "sum_why2", value = "")
    }
  }

  # Advance to next trial (or to Demographics after final trial)
  advance_trial <- function() {
    if (rvs$trial_n < TOTAL_TRIALS) {
      rvs$trial_n <- rvs$trial_n + 1
      go_to_current_trial()
    } else {
      updateTabsetPanel(session, "topnav", selected = "Demographics")
    }
  }
  # Consent gating: must check “I agree” checkbox
  observeEvent(input$consent_continue, {
    if (isTRUE(input$consent_choice)) {
      updateTabsetPanel(session, "topnav", selected = "Lineup")
    } else {
      showModal(modalDialog("Please check “I agree” to continue.", easyClose = TRUE))
    }
  })

  # Start experiment: create participant row + assign datasets + build trial_plan
  observeEvent(input$start_experiment, {
    started_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S%z")

    # insert participant session row
    dbExecute(con, "
      INSERT INTO participants (participant_id, session_id, started_at)
      VALUES (?, ?, ?)
      ON CONFLICT(participant_id, session_id) DO NOTHING;
    ", params = list(participant_id, session_id, started_at))

    session$userData$has_participants_row <- TRUE

    pid_index <- dbGetQuery(con, "
      SELECT pid_index
      FROM participants
      WHERE participant_id = ? AND session_id = ?
      LIMIT 1;
    ", params = list(participant_id, session_id))$pid_index[1]

    # assign datasets
    assigned_df <- assign_datasets_random(
      con = con,
      participant_id = participant_id,
      session_id = session_id,
      n_box = N_BOX,
      n_scatter = N_SCATTER,
      D = D_PER_PERSON
    )


    # only create trial_plan once
    existing_n <- dbGetQuery(con, "
      SELECT COUNT(*) AS n
      FROM trial_plan
      WHERE participant_id = ? AND session_id = ?;
    ", params = list(participant_id, session_id))$n[1]

    if (existing_n == 0) {
      sched0 <- make_schedule(pid_index, assigned_df)

      sched <- sched0 %>%
        mutate(
          participant_id = participant_id,
          session_id     = session_id,
          lineup_file    = mapply(dataset_id_to_file, dataset_id, plot_type)
        ) %>%
        select(participant_id, session_id, trial_n, dataset_id, plot_type, method, lineup_file)

      append_df(con, "trial_plan", sched)
    }

    # load trial_plan into memory for fast access during the run
    tp <- dbGetQuery(con, "
      SELECT trial_n, dataset_id, plot_type, method, lineup_file
      FROM trial_plan
      WHERE participant_id = ? AND session_id = ?
      ORDER BY trial_n;
    ", params = list(participant_id, session_id))

    trial_plan_rv(as_tibble(tp))

    if (rvs$trial_n <= 0) rvs$trial_n <- 1
    go_to_current_trial()
  })


  # Talk aloud: Start recording (shows lineup + starts speechcollectr)
  observeEvent(input$rec_start, {
    req(rvs$trial_n >= 1, get_trial_method(rvs$trial_n) == "Talk")

    # render lineup in record mode
    output$lineup_talk <- renderD3({
      r2d3(
        data = talk_lineup(),
        script = JS_PATH,
        options = list(mode = "record", annotation = FALSE)
      )
    })

    # start recorder (speechcollectr)
    msg <- tryCatch(
      {
        startRec()
        NULL
      },
      error = function(e) as.character(e)
    )

    if (is.null(msg)) {
      talk_is_recording(TRUE)
      talk_rec_started_at(Sys.time())
      talk_has_recorded(FALSE)
      talk_rec_row_id(NA_integer_)

      shinyjs::disable("rec_start")
      shinyjs::show("rec_stop")
      shinyjs::enable("rec_stop")

      shinyjs::hide("talk_placeholder")
      shinyjs::show("talk_lineup_wrap")

      rec_status_rv("Recording… When done, click “Stop recording”, then select panel number(s) and click “Save & proceed”.")
    } else {
      talk_is_recording(FALSE)
      talk_rec_started_at(NULL)
      rec_status_rv(paste("Could not start recorder:", msg))
    }
  })


  # Stop recording
  observeEvent(input$rec_stop, {
    req(rvs$trial_n >= 1, get_trial_method(rvs$trial_n) == "Talk")

    if (!isTRUE(talk_is_recording()) || is.null(talk_rec_started_at())) {
      showModal(modalDialog(
        title = "Recording required",
        "Please click “Start recording” first.",
        easyClose = TRUE
      ))
      return()
    }

    rec_status_rv("Stopping and saving…")
    shinyjs::disable("rec_stop")

    ts <- format(Sys.time(), "%Y%m%d-%H%M%S")
    fname <- sprintf("%s_T%02d_%s.wav", participant_id, rvs$trial_n, ts)

    msg <- tryCatch(
      {
        stopRec(filename = fname, finishedId = "rec_done")
        NULL
      },
      error = function(e) as.character(e)
    )

    if (!is.null(msg)) {
      shinyjs::enable("rec_stop")
      rec_status_rv(paste("Could not stop/save recorder:", msg))
    }
  })


  # Recording finished (insert recording row)
  observeEvent(input$rec_done, {
    req(rvs$trial_n >= 1, get_trial_method(rvs$trial_n) == "Talk")

    wav_path <- normalizePath(as.character(input$rec_done), winslash = "/", mustWork = FALSE)
    if (!nzchar(wav_path) || !file.exists(wav_path)) {
      showModal(modalDialog("Recorder finished, but no file was found.", easyClose = TRUE))
      shinyjs::enable("rec_stop")
      return()
    }

    ts <- format(Sys.time(), "%Y%m%d-%H%M%S")
    dest_fname <- sprintf("%s_T%02d_%s.wav", participant_id, rvs$trial_n, ts)
    dest_path <- normalizePath(file.path(REC_DIR, dest_fname), winslash = "/", mustWork = FALSE)

    if (!file.rename(wav_path, dest_path)) {
      file.copy(wav_path, dest_path, overwrite = TRUE)
      unlink(wav_path)
    }

    saved_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S%z")

    dbExecute(
      con,
      "INSERT INTO recordings (participant_id, session_id, trial_n, saved_at, filename, filepath, lineup_file, sel1, sel2)
       VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?);",
      params = list(
        participant_id, session_id, rvs$trial_n, saved_at,
        dest_fname, dest_path,
        talk_lineup()$filename %||% NA_character_,
        NA_integer_, NA_integer_
      )
    )

    rid <- dbGetQuery(con, "SELECT last_insert_rowid() AS id;")$id[1]
    talk_rec_row_id(rid)
    talk_has_recorded(TRUE)

    # reset recording flags
    talk_is_recording(FALSE)
    talk_rec_started_at(NULL)

    shinyjs::hide("rec_stop")
    shinyjs::enable("rec_start")
    rec_status_rv("Recording saved. Now select panel number(s), then click “Save & proceed”.")
  })


  # TALK: Save & proceed (gate both recording + selection)
  observeEvent(input$talk_next, {
    req(rvs$trial_n >= 1, get_trial_method(rvs$trial_n) == "Talk")

    sel1 <- if (is.null(input$talk_sel1) || input$talk_sel1 == "-") NA_integer_ else as.integer(input$talk_sel1)
    sel2 <- if (is.null(input$talk_sel2) || input$talk_sel2 == "-") NA_integer_ else as.integer(input$talk_sel2)

    has_rec <- isTRUE(talk_has_recorded())
    has_sel <- !is.na(sel1)

    if (!has_rec && !has_sel) {
      showModal(modalDialog(
        title = "Complete recording and selection",
        "Please complete the recording AND select a panel number before continuing.",
        easyClose = TRUE
      ))
      return()
    }

    if (!has_rec && has_sel) {
      showModal(modalDialog(
        title = "Recording required",
        "Please complete the recording (click “Stop recording” and wait for it to save) before continuing.",
        easyClose = TRUE
      ))
      return()
    }

    if (has_rec && !has_sel) {
      showModal(modalDialog(
        title = "Selection required",
        "Please select your first-choice panel number before continuing.",
        easyClose = TRUE
      ))
      return()
    }

    # update the recordings row with selections
    rid <- talk_rec_row_id()

    if (!is.na(rid)) {
      dbExecute(con, "
        UPDATE recordings
        SET sel1 = ?, sel2 = ?
        WHERE id = ?;
      ", params = list(sel1, sel2, rid))
    } else {
      # update most recent row for this talk trial
      dbExecute(con, "
        UPDATE recordings
        SET sel1 = ?, sel2 = ?
        WHERE participant_id = ? AND session_id = ? AND trial_n = ?
        ORDER BY id DESC
        LIMIT 1;
      ", params = list(sel1, sel2, participant_id, session_id, rvs$trial_n))
    }

    # save timing + advance
    save_trial_timing(rvs$trial_n, ended_at = Sys.time())

    rec_status_rv("Saved. Loading next trial…")
    advance_trial()
  })


  # text summary
  observeEvent(input$submit_summary, {
    req(rvs$trial_n >= 1, get_trial_method(rvs$trial_n) == "Summary")

    sel1 <- if (input$sum_sel1 == "-" || is.null(input$sum_sel1)) NA_integer_ else as.integer(input$sum_sel1)
    sel2 <- if (input$sum_sel2 == "-" || is.null(input$sum_sel2)) NA_integer_ else as.integer(input$sum_sel2)

    why1 <- trimws(input$sum_why1 %||% "")
    why2 <- trimws(input$sum_why2 %||% "")

    if (is.na(sel1) || !nzchar(why1)) {
      showModal(modalDialog("Please select at least one plot and provide a reason.", easyClose = TRUE))
      return()
    }
    if (!is.na(sel2) && !nzchar(why2)) {
      showModal(modalDialog("You selected a second plot—please provide a reason for it.", easyClose = TRUE))
      return()
    }

    now <- Sys.time()
    lineup_file <- summary_lineup()$filename %||% NA_character_

    rows <- list(
      tibble(
        participant_id = participant_id,
        session_id     = session_id,
        timestamp      = now,
        trial_n        = rvs$trial_n,
        lineup_file    = lineup_file,
        plotIndex      = sel1,
        reasoning      = why1
      )
    )

    if (!is.na(sel2) && nzchar(why2)) {
      rows <- append(rows, list(tibble(
        participant_id = participant_id,
        session_id     = session_id,
        timestamp      = now,
        trial_n        = rvs$trial_n,
        lineup_file    = lineup_file,
        plotIndex      = sel2,
        reasoning      = why2
      )))
    }

    append_df(con, "text_summary", bind_rows(rows))
    save_trial_timing(rvs$trial_n, ended_at = Sys.time())
    advance_trial()
  })


  # Highlight
  observeEvent(input$highlight_cleared,
    {
      req(rvs$trial_n >= 1, get_trial_method(rvs$trial_n) == "Highlight")
      info <- input$highlight_cleared
      if (is.null(info$plotIndex)) {
        return()
      }
      pidx <- as.integer(info$plotIndex)

      if (length(buf$regions)) {
        matches <- which(vapply(
          buf$regions,
          function(tb) tryCatch(as.integer(tb$plotIndex[1]) == pidx, error = function(...) FALSE),
          logical(1)
        ))
        if (length(matches)) buf$regions <- buf$regions[-tail(matches, 1)]
      }
    },
    ignoreInit = TRUE
  )

  # highlight - log plot clicks
  observeEvent(input$plot_clicked,
    {
      req(rvs$trial_n >= 1, get_trial_method(rvs$trial_n) == "Highlight")

      pc <- input$plot_clicked
      idx <- if (is.list(pc)) as.integer(pc$plotIndex) else as.integer(pc)
      act <- if (is.list(pc) && !is.null(pc$action)) as.character(pc$action) else "select"

      rv_click$event_rank <- rv_click$event_rank + 1
      ts_now <- Sys.time()

      dbExecute(
        con,
        "INSERT INTO clicks
       (participant_id, session_id, timestamp, trial_n, lineup_file, plotIndex, selection_rank, action, time_from_start_sec)
       VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?);",
        params = list(
          participant_id,
          session_id,
          format(ts_now, "%Y-%m-%d %H:%M:%S"),
          rvs$trial_n,
          buf$lineup$filename %||% NA_character_,
          idx,
          rv_click$event_rank,
          act,
          as.numeric(difftime(ts_now, sessionStart(), units = "secs"))
        )
      )
      # track the most recently selected plotIndex
      if (identical(act, "select")) last_clicked(idx) else last_clicked(NULL)
    },
    ignoreInit = TRUE
  )

  # receive highlight data from JS and buffer it until Save & proceed
  observeEvent(input$highlighted_region,
    {
      req(rvs$trial_n >= 1, get_trial_method(rvs$trial_n) == "Highlight")
      ann <- input$highlighted_region
      if (is.null(ann) || is.null(ann$polygon)) {
        return()
      }

      buf$regions <- append(buf$regions, list(
        tibble(
          participant_id     = participant_id,
          session_id         = session_id,
          timestamp          = Sys.time(),
          trial_n            = rvs$trial_n,
          lineup_file        = buf$lineup$filename %||% NA_character_,
          plotIndex          = ann$plotIndex,
          selected_json      = as.character(toJSON(ann$polygon, auto_unbox = TRUE)),
          n_selected         = length(ann$polygon),
          draw_started_at_ms = ann$drawStartedAt %||% NA_real_,
          draw_ended_at_ms   = ann$drawEndedAt %||% NA_real_,
          draw_duration_ms   = ann$durationMs %||% NA_real_
        )
      ))
    },
    ignoreInit = TRUE
  )

  # Save & proceed (must have at least one highlight, optionally enforce highlight on selected plot)
  observeEvent(input$save_highlight, {
    req(rvs$trial_n >= 1, get_trial_method(rvs$trial_n) == "Highlight")

    regions_df <- if (length(buf$regions)) bind_rows(buf$regions) else tibble()

    if (!nrow(regions_df)) {
      showModal(modalDialog(
        title = "Please highlight before continuing",
        "You must highlight the plot before clicking “Save & proceed”.",
        easyClose = TRUE
      ))
      return()
    }

    # if they selected a plot, ensure they highlighted that plot
    if (!is.null(last_clicked())) {
      if (!any(regions_df$plotIndex == last_clicked())) {
        showModal(modalDialog(
          title = "Highlight the selected plot",
          sprintf("Please highlight key features on plot %d (the one you selected) before continuing.", last_clicked()),
          easyClose = TRUE
        ))
        return()
      }
    }

    append_df(con, "highlighted_regions", regions_df)
    save_trial_timing(rvs$trial_n, ended_at = Sys.time())
    advance_trial()
  })


  # show video demo
  observeEvent(input$show_highlight_demo, {
    showModal(modalDialog(
      title = "How to mark key features on a panel",
      # size = "s",
      easyClose = TRUE,
      footer = modalButton("Close"),
      tags$video(
        src = "highlight-demo.mp4",
        type = "video/mp4",
        controls = NA,
        autoplay = NA,
        muted = NA,
        style = "width:100%; border-radius:12px; border:1px solid #e5e7eb;"
      ),
      tags$div(
        style = "margin-top:12px; font-size:14px; color:#475569;",
        tags$ul(
          tags$li("Click a panel first."),
          tags$li("Click the Highlight button."),
          tags$li("Draw around the feature you want to mark."),
          tags$li("You may clear and redraw.")
        )
      )
    ))
  })


  # demographics
  observeEvent(input$demo_continue, {
    if ((input$demo_exp %||% "") == "" ||
      (input$education %||% "") == "" ||
      (input$gender_identity %||% "") == "" ||
      (input$color_blind %||% "") == "") {
      showModal(modalDialog(
        "Please answer all demographic questions to continue.",
        easyClose = TRUE
      ))
      return()
    }

    demo_row <- tibble(
      participant_id   = participant_id,
      session_id       = session_id,
      timestamp        = format(Sys.time(), "%Y-%m-%d %H:%M:%S%z"),
      age_range        = input$demo_exp %||% NA_character_,
      education_level  = input$education %||% NA_character_,
      gender_identity  = input$gender_identity %||% NA_character_,
      color_blind      = input$color_blind %||% NA_character_
    )

    append_df(con, "demographics", demo_row)

    now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S%z")

    dbExecute(con, "
    UPDATE participants
    SET completed = 1,
        ended_at   = COALESCE(ended_at, ?)
    WHERE participant_id = ? AND session_id = ?;
  ", params = list(now, participant_id, session_id))

    # go to Done tab
    updateTabsetPanel(session, "topnav", selected = "Done")
  })
}
