#!/usr/bin/Rscript
# Sorry this isn't elegant but necessary for the cron tab to work
setwd("~/Documents/Students/Liyanage-Dinuwanthi/Research-graphical-perception/")

system2("source ./venv/bin/activate; ./whisper-transcribe", wait = F)



onedrive_running <- system("ps aufxw | grep onedrive", intern = T)
if (length(onedrive_running) < 2) {
  system2("onedrive", "--monitor", wait = F)
}

# Set up authentication via ssh
cred <- git2r::cred_ssh_key("~/.ssh/id_ed25519.pub", "~/.ssh/id_ed25519")
repo <- git2r::repository()
git2r::config(repo = repo, global = F, "Susan-auto", "srvanderplas@gmail.com")

# Log job start
httr::POST("https://hc-ping.com/99aa2306-4b9c-433a-a6d0-4107adc5e6c0/start")

# Check repo status
status <- git2r::status()

recordings <- list.files("data/recordings", full.names = F, recursive = T, include.dirs = T)
transcripts <- list.files("data/transcripts", full.names = F, recursive = T, include.dirs = T)
recordings_path <- file.path("/btrstorage", "OneDrive", "UNL", "Data", "2026-Liyanage-Dinuwanthi", "2026-Graphical-Perception", "data", "recordings")
recordings_data <- file.path("data", "recordings", recordings)

transcripts_path <- file.path("/btrstorage", "OneDrive", "UNL", "Data", "2026-Liyanage-Dinuwanthi", "2026-Graphical-Perception", "data", "transcripts")
transcripts_data <- file.path("data", "transcripts", transcripts)

tmp <- status$unstaged
modified <- names(tmp) == "modified"
modified <- unlist(tmp[modified])

# If db has been modified
if (any(stringr::str_detect(modified, ".*\\.sqlite")) | any(stringr::str_detect(modified, ".*\\.json"))) {

  # Copy database/codes to one drive
  file.copy(modified, file.path("/btrstorage", "OneDrive", "UNL", "Data", "2026-Liyanage-Dinuwanthi", "2026-Graphical-Perception"), overwrite = T)
  file.copy(recordings_data, recordings_path, overwrite = F)
  file.copy(transcripts_data, transcripts_path, overwrite = F)

  # Add changed db to commit and commit
  git2r::add(repo = ".", "*.sqlite")
  git2r::add(repo = ".", "*.json")
  try(git2r::commit(message = "Update data"))

  # Update
  git2r::pull(repo = ".", credentials = cred)
  git2r::push(getwd(), credentials = cred)

  if (length(git2r::status()$unstaged$conflicted) > 0) {
    # Log merge conflict, signal failure (Susan gets an email)
    httr::POST("https://hc-ping.com/99aa2306-4b9c-433a-a6d0-4107adc5e6c0/fail", body = "Merge conflict")
  } else {
    # Log success
    httr::POST("https://hc-ping.com/99aa2306-4b9c-433a-a6d0-4107adc5e6c0", body = "Changes pushed")
  }
} else {
  # Log no changes
  httr::POST("https://hc-ping.com/99aa2306-4b9c-433a-a6d0-4107adc5e6c0", body = "No changes")
}

git2r::config(repo = repo, global = F, "Susan Vanderplas", "srvanderplas@gmail.com")
