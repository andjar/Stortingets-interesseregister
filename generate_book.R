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
data <- fread(data_file, encoding = "UTF-8")

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

    if (is.na(current_row$prev_text)) {
      # First entry for this person, just show the current text
      qmd_content <- paste0(
        paste0(qmd_content, "## ", format(current_row$date, "%Y-%m-%d"), "\n\n"),
        "First registered entry:\n\n",
        "```text\n",
        current_row$full_text,
        "\n```\n\n"
      )
    } else {
      # Subsequent entry, show the diff
      any_change_from_previous <- all(gsub("[[:space:]]", "", current_row$full_text) == gsub("[[:space:]]", "", current_row$prev_text))
      if (any_change_from_previous) {
        # qmd_content <- paste0(
        #   paste0(qmd_content, "## ", format(current_row$date, "%Y-%m-%d"), "\n\n"),
        #   "No changes since ", format(current_row$prev_date, "%Y-%m-%d"), ".\n\n"
        # )
        qmd_content <- paste0(
          qmd_content,
          "No changes at ", format(current_row$date, "%Y-%m-%d"), ".\n\n"
        )
      } else {
        qmd_content <- paste0(
          paste0(qmd_content, "## ", format(current_row$date, "%Y-%m-%d"), "\n\n"),
          "Changes since ", format(current_row$prev_date, "%Y-%m-%d"), ":\n\n",
          # Pass variables explicitly to avoid environment issues in rendering
          "```{r echo=FALSE, results='asis', comment=NA}\n",
          "current_text_val <- ", deparse(current_row$full_text), "\n",
          "prev_text_val <- ", deparse(current_row$prev_text), "\n",
          "diffobj::diffChr(prev_text_val, current_text_val, format = 'html', mode = 'sidebyside', ignore.white.space = TRUE, style = list(html.output = 'diff.w.style'))\n",
          "```\n\n"
        )
      }
    }
  }

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
  filename <- file.path(output_dir_dates, sanitize_filename(format(as.Date(d), "%Y_%m_%d"), prefix = "date_"))
  date_files <- c(date_files, filename)
  
  # Start writing QMD content
  qmd_content <- paste0("# ", format(as.Date(d), "%Y-%m-%d"), "\n\n")
  
  for (p in date_data$person) {
    current_row <- date_data[person == p] # Should be only one row here per person/date
    
    # Need the previous state *for this person* (which might be from an earlier date)
    person_history <- summary_data[person == p & date <= d]
    setorder(person_history, date)
    prev_row <- if(nrow(person_history) > 1) person_history[nrow(person_history)-1] else NULL
    
    if (is.null(prev_row)) {
      # First entry for this person up to this date
      qmd_content <- paste0(
        paste0(qmd_content, "## ", p, "\n\n"),
        "First registered entry (as of this date):\n\n",
        "```text\n",
        current_row$full_text,
        "\n```\n\n"
      )
    } else {
      
      any_change_from_previous <- all(gsub("[[:space:]]", "", current_row$full_text) == gsub("[[:space:]]", "", prev_row$full_text))
      if (any_change_from_previous) {
        # qmd_content <- paste0(
        #   paste0(qmd_content, "## ", p, "\n\n"),
        #   "No changes since ", format(prev_row$date, "%Y-%m-%d"), ".\n\n"
        # )
      } else {
        qmd_content <- paste0(
          paste0(qmd_content, "## ", p, "\n\n"),
          "Changes since ", format(prev_row$date, "%Y-%m-%d"), ":\n\n",
          "```{r echo=FALSE, results='asis', comment=NA}\n",
          "current_text_val <- ", deparse(current_row$full_text), "\n",
          "prev_text_val <- ", deparse(prev_row$full_text), "\n",
          "diffobj::diffChr(prev_text_val, current_text_val, format = 'html', mode = 'sidebyside', ignore.white.space = TRUE, style = list(html.output = 'diff.w.style'))\n",
          "```\n\n"
        )
      }
    }
  }

  writeLines(qmd_content, filename, useBytes = TRUE)
}
message("Generated ", length(date_files), " date chapter files.")

# --- Copy diffobj CSS ---
message("Copying diffobj CSS file...")
diffobj_css_path <- diffobj::diffobj_css()

if (!is.null(diffobj_css_path) && file.exists(diffobj_css_path)) {
  # Define target path (e.g., in the root or a 'css' subdir)
  target_css_path <- "diffobj.css"
  # Uncomment next line if you prefer a css subdirectory
  # target_css_path <- file.path("css", "diffobj.css"); dir.create("css", showWarnings = FALSE)
  
  if (file.copy(diffobj_css_path, target_css_path, overwrite = TRUE)) {
    message("Copied diffobj CSS to: ", target_css_path)
  } else {
    warning("Failed to copy diffobj CSS to: ", target_css_path)
  }
} else {
  warning("Could not find diffobj CSS file using diffobj::diffStyle(css.mode = 'file').")
}

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
    `repo-actions` = list("edit", "issue"),
    `reader-mode` = TRUE,
    sidebar = list(
      search = TRUE,
      style = "docked"
    )
  ),
  format = list(
    html = list(
      theme = "cosmo", # Choose a theme: https://quarto.org/docs/output-formats/html-themes.html
      toc = TRUE,
      `toc-depth` = 2L,
      css = "diffobj.css"
    )
  ),
  # Optional: Add bibliography, etc.
  execute = list(
    freeze = "auto" # Re-render only if code/data changes
  )
)

write_yaml(quarto_config, quarto_yaml_file, handlers = list(logical = yaml::verbatim_logical))
message(quarto_yaml_file, " generated successfully.")
message("Script finished.")
