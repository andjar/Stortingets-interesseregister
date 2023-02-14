library("data.table")
library("pdftools")

list_of_files <- list.files(path = here::here("data", "raw", "unprocessed"), pattern = "*.pdf")
txt_start_page <- 5

commit_author_name <- "R Script Bot"
commit_author_email <- "ajarmund@gmail.com"
git_exec <- Sys.which("git")

for (sel_file in list_of_files) {
  txt <- pdf_text(here::here("data", "raw", "unprocessed", sel_file))
  file.rename(
    here::here("data", "raw", "unprocessed", sel_file),
    here::here("data", "raw",   "processed", sel_file)
  )
  
  df <- rbindlist(lapply(seq(txt_start_page, length(txt)), function(i) {
    data.table(
      page = i,
      date = as.Date(sel_file),
      text = scan(textConnection(txt[[i]]), what="character", sep = "\n")
    )
  }))
  df <- df[text != "Representanter"]
  df <- df[text != "Regjeringsmedlemmer"]
  df <- df[!grepl("Ajourf.rt pr\\.", text)]
  
  pattern <- "^[^,]+, [^(]+ \\([^,]+, [^)]+\\)$"
  df[, person_row := grepl(pattern, text)]
  df[grepl("3", text, fixed = TRUE), person_row := FALSE]
  df[grepl("forlag", text, fixed = TRUE), person_row := FALSE]
  
  current_person <- ""
  df[, person := current_person]
  for (i in 1:nrow(df)) {
    if (df[i, person_row] == TRUE) {
      current_person <- df[i, text]
    }
    df[i, person := current_person]
  }
  df <- df[person_row == FALSE]
  df[, person_row := NULL]
  
  df[text == "Har ingen registreringspliktige interesser", type := "None"]
  df[text == "Ingen registrerte opplysninger", type := "None"]
  
  df[, section_row := grepl("^\\xA7", text)]
  current_section <- ""
  for (i in 1:nrow(df)) {
    if (df[i, section_row] == TRUE) {
      current_section <- strsplit(df[i, text], " ", fixed = TRUE)[[1]][[1]]
    }
    if (is.na(df[i, type])) {
      df[i, type := current_section]
    }
  }
  df[, section_row := NULL]
  
  
  # Append to summary file
  fwrite(df, file = here::here("data", "data.csv"), append = TRUE)
  
  # Split by person and save
  dfs <- split(df, df$person)
  lapply(names(dfs), function(x) {
    fwrite(
      dfs[[x]][, .(text)],
      file = here::here("data", "processed", paste0(x, ".csv")),
      col.names = FALSE
    )
  })
  
  # Commit to git
  stage_args <- c("add", ".")
  stage_status <- system2(git_exec, args = stage_args, stdout = FALSE, stderr = TRUE, wait = TRUE)
  
  commit_time_str <- format(as.POSIXct(paste(substr(sel_file, 1, 10), "12:00:00"), format="%Y-%m-%d %H:%M:%S", tz="UTC"),
                            "%Y-%m-%dT%H:%M:%S %z")
  commit_msg <- paste("Update data as of", substr(sel_file, 1, 10))
  
  git_env_vars <- list(
    GIT_AUTHOR_NAME = commit_author_name,
    GIT_AUTHOR_EMAIL = commit_author_email,
    GIT_AUTHOR_DATE = commit_time_str,
    GIT_COMMITTER_NAME = commit_author_name,
    GIT_COMMITTER_EMAIL = commit_author_email,
    GIT_COMMITTER_DATE = commit_time_str
  )
  
  original_env_values <- list()
  for (var_name in names(git_env_vars)) {
    original_env_values[[var_name]] <- Sys.getenv(var_name, unset = NA)
    Sys.setenv(var_name = git_env_vars[[var_name]])
    args_setenv <- list(git_env_vars[[var_name]])
    names(args_setenv) <- var_name
    do.call(Sys.setenv, args_setenv)
  }
  
  commit_args <- c("commit", "--allow-empty", "-m", paste0('"', commit_msg, '"'))
  
  tryCatch({
    # Commit using system git - it will pick up the environment variables we just set
    # NO 'env' argument needed here in system2
    commit_output <- system2(git_exec, args = commit_args, stdout = TRUE, stderr = TRUE, wait = TRUE)
    commit_status <- attr(commit_output, "status") # Get status AFTER execution
    
  }, finally = {
    # --- Restore Original Environment Variables ---
    for (var_name in names(original_env_values)) {
      original_value <- original_env_values[[var_name]]
      if (is.na(original_value)) {
        Sys.unsetenv(var_name) # Unset if it wasn't set before
      } else {
        args_setenv <- list(original_value)
        names(args_setenv) <- var_name
        do.call(Sys.setenv, args_setenv)
      }
    }
  })
  
}
