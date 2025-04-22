# Load necessary libraries
# Install them if you haven't: install.packages(c("data.table", "diffobj", "here", "stringr", "lubridate", "yaml", "knitr"))
library(data.table)
library(diffobj)
library(here)
library(stringr)
library(lubridate)
library(yaml)
library(knitr)

# --- Configuration ---
output_dir_politicians <- "politicians"
output_dir_dates <- "dates"
data_file <- here("data", "data.csv")
quarto_yaml_file <- "_quarto.yml"
index_qmd_file <- "index.qmd"

# Create output directories if they don't exist
dir.create(output_dir_politicians, showWarnings = FALSE, recursive = TRUE)
dir.create(output_dir_dates, showWarnings = FALSE, recursive = TRUE)

# --- Data Loading and Preparation ---
message("Loading data from: ", data_file)
data <- fread(data_file)

# Ensure date is Date type
data[, date := as.IDate(date)]

# Combine text lines for each person/date, maintaining original order if possible
# Assuming data is pre-sorted correctly within each person/date group in the CSV
# If not, you might need an explicit order column (like line number)
message("Aggregating text data...")
summary_data <- data[, .(
  full_text = paste(text, collapse = "\n") # Concatenate lines
), by = .(person, date)]

# Order by person and then date
setorder(summary_data, person, date)

# Add previous date's text for comparison
summary_data[, prev_date := shift(date, type = "lag"), by = person]
summary_data[, prev_text := shift(full_text, type = "lag"), by = person]

# --- Helper function to sanitize filenames ---
sanitize_filename <- function(name, prefix = "") {
  name <- str_replace_all(name, "[^[:alnum:]_\\-]+", "_") # Replace non-alphanumeric (except _, -) with _
  name <- str_squish(name) # Remove extra whitespace/underscores
  name <- str_replace_all(name, "_+", "_") # Collapse multiple underscores
  name <- str_sub(name, 1, 100) # Limit length
  paste0(prefix, name, ".qmd")
}

# --- Generate Chapters by Politician ---
message("Generating chapters by politician...")
politician_files <- character()
all_persons <- unique(summary_data$person)

for (p in all_persons) {
  person_data <- summary_data[person == p]
  setorder(person_data, date)
  
  # Create filename
  filename <- file.path(output_dir_politicians, sanitize_filename(p))
  politician_files <- c(politician_files, filename)
  
  # Start writing QMD content
  qmd_content <- paste0("# ", p, "\n\n")
  
  for (i in 1:nrow(person_data)) {
    current_row <- person_data[i, ]
    qmd_content <- paste0(qmd_content, "## ", format(current_row$date, "%Y-%m-%d"), "\n\n")
    
    if (is.na(current_row$prev_text)) {
      # First entry for this person, just show the current text
      qmd_content <- paste0(
        qmd_content,
        "First registered entry:\n\n",
        "```text\n",
        current_row$full_text,
        "\n```\n\n"
      )
    } else {
      # Subsequent entry, show the diff
      # We need to pass the text variables to the R chunk
      # Using knitr's built-in diffobj support simplifies this
      qmd_content <- paste0(
        qmd_content,
        "Changes since ", format(current_row$prev_date, "%Y-%m-%d"), ":\n\n",
        # Pass variables explicitly to avoid environment issues in rendering
        "```{r echo=FALSE, results='asis', comment=NA}\n",
        "current_text_val <- ", deparse(current_row$full_text), "\n",
        "prev_text_val <- ", deparse(current_row$prev_text), "\n",
        "diffobj::diffChr(prev_text_val, current_text_val, format = 'html', mode = 'sidebyside', ignore.white.space = TRUE, line.numbers=FALSE, header = FALSE)\n",
        "```\n\n"
      )
    }
  }
  # Write the QMD file
  writeLines(qmd_content, filename, useBytes = TRUE) # Use UTF-8 encoding
}
message("Generated ", length(politician_files), " politician chapter files.")


# --- Generate Chapters by Date ---
message("Generating chapters by date...")
date_files <- character()
all_dates <- sort(unique(summary_data$date))

for (d in all_dates) {
  date_data <- summary_data[date == d]
  setorder(date_data, person) # Order by person within the date
  
  # Create filename
  filename <- file.path(output_dir_dates, sanitize_filename(format(d, "%Y_%m_%d"), prefix = "date_"))
  date_files <- c(date_files, filename)
  
  # Start writing QMD content
  qmd_content <- paste0("# ", format(d, "%Y-%m-%d"), "\n\n")
  
  for (p in date_data$person) {
    current_row <- date_data[person == p] # Should be only one row here per person/date
    
    # Need the previous state *for this person* (which might be from an earlier date)
    person_history <- summary_data[person == p & date <= d]
    setorder(person_history, date)
    prev_row <- if(nrow(person_history) > 1) person_history[nrow(person_history)-1] else NULL
    
    qmd_content <- paste0(qmd_content, "## ", p, "\n\n")
    
    if (is.null(prev_row)) {
      # First entry for this person up to this date
      qmd_content <- paste0(
        qmd_content,
        "First registered entry (as of this date):\n\n",
        "```text\n",
        current_row$full_text,
        "\n```\n\n"
      )
    } else {
      # Show diff compared to the last known state for this person
      qmd_content <- paste0(
        qmd_content,
        "Changes since ", format(prev_row$date, "%Y-%m-%d"), ":\n\n",
        "```{r echo=FALSE, results='asis', comment=NA}\n",
        "current_text_val <- ", deparse(current_row$full_text), "\n",
        "prev_text_val <- ", deparse(prev_row$full_text), "\n",
        "diffobj::diffChr(prev_text_val, current_text_val, format = 'html', mode = 'sidebyside', ignore.white.space = TRUE, line.numbers=TRUE, header = FALSE)\n",
        "```\n\n"
      )
    }
  }
  # Write the QMD file
  writeLines(qmd_content, filename, useBytes = TRUE)
}
message("Generated ", length(date_files), " date chapter files.")


# --- Generate _quarto.yml ---
message("Generating ", quarto_yaml_file, "...")
quarto_config <- list(
  project = list(
    type = "book",
    `output-dir` = "_book" # Standard output directory
  ),
  book = list(
    title = "Norwegian Politician Financial Interests",
    author = "Anders Hagen Jarmund",
    date = "today",
    chapters = list(
      list(part = "Introduction", chapters = list(index_qmd_file)),
      list(part = "Changes by Politician", chapters = as.list(politician_files)),
      list(part = "Changes by Date", chapters = as.list(date_files))
    ),
    `repo-url` = "https://github.com/andjar/Stortingets-interesseregister", # CHANGE THIS
    `repo-branch` = "main", # Or your default branch
    `repo-actions` = list("edit", "issue")
  ),
  format = list(
    html = list(
      theme = "cosmo", # Choose a theme: https://quarto.org/docs/output-formats/html-themes.html
      toc = TRUE,
      `toc-depth` = 2
    )
  ),
  # Optional: Add bibliography, etc.
  execute = list(
    freeze = "auto" # Re-render only if code/data changes
  )
)

write_yaml(quarto_config, quarto_yaml_file)
message(quarto_yaml_file, " generated successfully.")

# --- Generate a simple index.qmd ---
if (!file.exists(index_qmd_file)) {
  message("Generating basic ", index_qmd_file, "...")
  index_content <- '
# Introduction

This book summarizes the registered financial interests of Norwegian politicians based on data extracted from [Source - describe your PDF source].

The book is automatically generated and presents changes over time in two ways:

1.  **By Politician:** Each chapter focuses on one politician, showing the chronological changes in their registered interests.
2.  **By Date:** Each chapter focuses on a specific date, showing the status or changes for all relevant politicians on that date.

The diffs highlight additions (green) and deletions (red) compared to the previous entry, ignoring whitespace changes.

*This book is automatically generated. Please refer to the original source for official information.*
'
  writeLines(trimws(index_content), index_qmd_file)
  message(index_qmd_file, " created.")
} else {
  message(index_qmd_file, " already exists, skipping generation.")
}

message("Script finished.")