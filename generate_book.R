# Load necessary libraries
# Install them if you haven't: install.packages(c("data.table", "diffobj", "here", "stringr", "lubridate", "yaml"))
library(data.table)
library(diffobj)
library(here)
library(stringr)
library(lubridate)
library(yaml)

# --- Configuration ---
output_dir_politicians <- "politicians"
output_dir_dates <- "dates"
output_dir_indexes <- "indexes"
data_file <- here("data", "data.csv")
quarto_yaml_file <- "_quarto.yml"
index_qmd_file <- "index.qmd"

# Create output directories if they don't exist
dir.create(output_dir_politicians, showWarnings = FALSE, recursive = TRUE)
dir.create(output_dir_dates, showWarnings = FALSE, recursive = TRUE)
dir.create(output_dir_indexes, showWarnings = FALSE, recursive = TRUE)

# --- Helper function to parse politician info from name string ---
# Format: "Lastname, Firstname (Party, Region)"
parse_politician_info <- function(name_string) {
  # Extract parts using regex
  # Pattern: "Lastname, Firstname (Party, Region)"
  match <- str_match(name_string, "^([^,]+),\\s*([^(]+)\\s*\\(([^,]+),\\s*([^)]+)\\)")
  
  if (is.na(match[1])) {
    # Fallback if pattern doesn't match
    return(list(
      last_name = name_string,
      first_name = "",
      party = "Unknown",
      region = "Unknown",
      full_name = name_string
    ))
  }
  
  list(
    last_name = str_trim(match[2]),
    first_name = str_trim(match[3]),
    party = str_trim(match[4]),
    region = str_trim(match[5]),
    full_name = name_string
  )
}

# --- Party name mapping ---
party_names <- list(
  "A" = "Arbeiderpartiet",
  "FrP" = "Fremskrittspartiet", 
  "H" = "Høyre",
  "KrF" = "Kristelig Folkeparti",
  "MDG" = "Miljøpartiet De Grønne",
  "R" = "Rødt",
  "Sp" = "Senterpartiet",
  "SV" = "Sosialistisk Venstreparti",
  "V" = "Venstre",
  "PF" = "Pasientfokus",
  "Uavh" = "Uavhengig",
  "Unknown" = "Ukjent parti"
)

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

# --- Helper function to compute plain text diff (much smaller than HTML) ---
# Uses simple line-by-line comparison to show changes
compute_text_diff <- function(prev_text, current_text) {
  prev_lines <- strsplit(prev_text, "\n")[[1]]
  curr_lines <- strsplit(current_text, "\n")[[1]]
  
  # Use diffobj but capture as plain text (ANSI stripped)
  diff_output <- capture.output({
    diffobj::diffChr(
      prev_text, 
      current_text, 
      format = 'raw',  # Plain text format - much smaller than HTML
      mode = 'unified',
      ignore.white.space = TRUE,
      pager = 'off'
    )
  })
  
  # Return as plain text for code block display
  paste(diff_output, collapse = "\n")
}

# --- Generate Chapters by Politician ---
# No artificial limits - show all historical changes
message("Generating chapters by politician...")
politician_files <- character()
all_persons <- unique(summary_data$person)

for (p in all_persons) {
  person_data <- summary_data[person == p]
  setorder(person_data, date)  # Oldest first for chronological display
  
  # Create filename
  filename <- file.path(output_dir_politicians, sanitize_filename(p))
  politician_files <- c(politician_files, filename)
  
  # Start writing QMD content
  qmd_content <- paste0("# ", p, "\n\n")
  
  for (i in 1:nrow(person_data)) {
    current_row <- person_data[i, ]

    if (is.na(current_row$prev_text)) {
      # First entry for this person
      qmd_content <- paste0(
        paste0(qmd_content, "## ", format(current_row$date, "%Y-%m-%d"), " {#section-", format(current_row$date, "%Y-%m-%d"), "}\n\n"),
        "First registered entry:\n\n",
        "```text\n",
        current_row$full_text,
        "\n```\n\n"
      )
    } else {
      # Check for actual changes
      any_change_from_previous <- all(gsub("[[:space:]]", "", current_row$full_text) == gsub("[[:space:]]", "", current_row$prev_text))
      if (!any_change_from_previous) {
        # Show the diff as plain text
        diff_text <- compute_text_diff(current_row$prev_text, current_row$full_text)
        qmd_content <- paste0(
          paste0(qmd_content, "## ", format(current_row$date, "%Y-%m-%d"), " {#section-", format(current_row$date, "%Y-%m-%d"), "}\n\n"),
          "Changes since ", format(current_row$prev_date, "%Y-%m-%d"), ":\n\n",
          "```diff\n",
          diff_text,
          "\n```\n\n"
        )
      }
    }
  }

  writeLines(qmd_content, filename, useBytes = TRUE) # Use UTF-8 encoding
}
message("Generated ", length(politician_files), " politician chapter files.")


# --- Generate Chapters by Date ---
# Create lightweight date pages that just summarize changes (links to politician pages for full diffs)
# This avoids duplicating all diff HTML and dramatically reduces memory usage
message("Generating chapters by date...")
date_files <- character()
all_dates <- sort(unique(summary_data$date))

for (d in all_dates) {
  date_data <- summary_data[date == d]
  setorder(date_data, person) # Order by person within the date
  
  # Collect changes for this date
  new_registrations <- character()
  updates <- character()
  
  for (p in date_data$person) {
    current_row <- date_data[person == p]
    
    # Need the previous state *for this person*
    person_history <- summary_data[person == p & date <= d]
    setorder(person_history, date)
    prev_row <- if(nrow(person_history) > 1) person_history[nrow(person_history)-1] else NULL
    
    if (is.null(prev_row)) {
      # First entry for this person
      new_registrations <- c(new_registrations, p)
    } else {
      any_change_from_previous <- all(gsub("[[:space:]]", "", current_row$full_text) == gsub("[[:space:]]", "", prev_row$full_text))
      if (!any_change_from_previous) {
        updates <- c(updates, p)
      }
    }
  }
  
  # Only create file if there are changes
  if (length(new_registrations) > 0 || length(updates) > 0) {
    qmd_content <- paste0("# ", format(as.Date(d), "%Y-%m-%d"), "\n\n")
    
    if (length(new_registrations) > 0) {
      qmd_content <- paste0(qmd_content, "## New Registrations\n\n")
      for (p in new_registrations) {
        # Create link to politician page
        politician_file <- sanitize_filename(p)
        politician_file <- sub("\\.qmd$", ".html", politician_file)
        qmd_content <- paste0(qmd_content, "- [", p, "](../politicians/", politician_file, ")\n")
      }
      qmd_content <- paste0(qmd_content, "\n")
    }
    
    if (length(updates) > 0) {
      qmd_content <- paste0(qmd_content, "## Updated Registrations\n\n")
      for (p in updates) {
        # Create link to politician page with anchor to this date
        politician_file <- sanitize_filename(p)
        politician_file <- sub("\\.qmd$", ".html", politician_file)
        date_anchor <- format(as.Date(d), "%Y-%m-%d")
        qmd_content <- paste0(qmd_content, "- [", p, "](../politicians/", politician_file, "#section-", date_anchor, ")\n")
      }
      qmd_content <- paste0(qmd_content, "\n")
    }
    
    filename <- file.path(output_dir_dates, sanitize_filename(format(as.Date(d), "%Y_%m_%d"), prefix = "date_"))
    date_files <- c(date_files, filename)
    writeLines(qmd_content, filename, useBytes = TRUE)
  }
}
message("Generated ", length(date_files), " date chapter files (lightweight summaries with links).")

# --- Parse politician metadata for index pages ---
message("Parsing politician metadata...")
politician_info <- lapply(all_persons, parse_politician_info)
names(politician_info) <- all_persons

# Create a data.table for easier grouping
politician_meta <- data.table(
  full_name = all_persons,
  last_name = sapply(politician_info, function(x) x$last_name),
  first_name = sapply(politician_info, function(x) x$first_name),
  party = sapply(politician_info, function(x) x$party),
  region = sapply(politician_info, function(x) x$region),
  filename = politician_files
)

# --- Generate Index by Party ---
message("Generating index by party...")
parties <- sort(unique(politician_meta$party))

party_index_content <- "# Politicians by Party\n\n"
party_index_content <- paste0(party_index_content, "Browse members of the Storting by their political party.\n\n")

for (p in parties) {
  party_members <- politician_meta[party == p][order(last_name)]
  party_full_name <- if (!is.null(party_names[[p]])) party_names[[p]] else p
  
  party_index_content <- paste0(party_index_content, "## ", party_full_name, " (", p, ")\n\n")
  
  for (i in 1:nrow(party_members)) {
    member <- party_members[i]
    html_file <- sub("\\.qmd$", ".html", basename(member$filename))
    party_index_content <- paste0(
      party_index_content,
      "- [", member$full_name, "](../politicians/", html_file, ")\n"
    )
  }
  party_index_content <- paste0(party_index_content, "\n")
}

party_index_file <- file.path(output_dir_indexes, "by-party.qmd")
writeLines(party_index_content, party_index_file, useBytes = TRUE)

# --- Generate Index by Last Name (Alphabetical) ---
message("Generating alphabetical index...")
politician_meta[, first_letter := toupper(substr(last_name, 1, 1))]
letters_used <- sort(unique(politician_meta$first_letter))

alpha_index_content <- "# Politicians A-Å\n\n"
alpha_index_content <- paste0(alpha_index_content, "Browse members of the Storting alphabetically by last name.\n\n")

for (letter in letters_used) {
  letter_members <- politician_meta[first_letter == letter][order(last_name, first_name)]
  
  alpha_index_content <- paste0(alpha_index_content, "## ", letter, "\n\n")
  
  for (i in 1:nrow(letter_members)) {
    member <- letter_members[i]
    html_file <- sub("\\.qmd$", ".html", basename(member$filename))
    alpha_index_content <- paste0(
      alpha_index_content,
      "- [", member$full_name, "](../politicians/", html_file, ") - ", member$party, "\n"
    )
  }
  alpha_index_content <- paste0(alpha_index_content, "\n")
}

alpha_index_file <- file.path(output_dir_indexes, "by-name.qmd")
writeLines(alpha_index_content, alpha_index_file, useBytes = TRUE)

# --- Generate Index by Region ---
message("Generating index by region...")
regions <- sort(unique(politician_meta$region))

region_index_content <- "# Politicians by Region\n\n"
region_index_content <- paste0(region_index_content, "Browse members of the Storting by their electoral district.\n\n")

for (r in regions) {
  region_members <- politician_meta[region == r][order(last_name)]
  
  region_index_content <- paste0(region_index_content, "## ", r, "\n\n")
  
  for (i in 1:nrow(region_members)) {
    member <- region_members[i]
    html_file <- sub("\\.qmd$", ".html", basename(member$filename))
    region_index_content <- paste0(
      region_index_content,
      "- [", member$full_name, "](../politicians/", html_file, ") - ", member$party, "\n"
    )
  }
  region_index_content <- paste0(region_index_content, "\n")
}

region_index_file <- file.path(output_dir_indexes, "by-region.qmd")
writeLines(region_index_content, region_index_file, useBytes = TRUE)

# --- Generate Date Index (timeline) ---
message("Generating date timeline index...")
date_index_content <- "# Timeline of Changes\n\n"
date_index_content <- paste0(date_index_content, "Browse changes by date. Click a date to see who registered new interests or updated their registration.\n\n")

# Group by year for better organization
all_dates_parsed <- as.Date(all_dates)
years <- sort(unique(format(all_dates_parsed, "%Y")), decreasing = TRUE)

for (year in years) {
  year_dates <- all_dates[format(all_dates_parsed, "%Y") == year]
  year_dates <- sort(year_dates, decreasing = TRUE)
  
  date_index_content <- paste0(date_index_content, "## ", year, "\n\n")
  
  for (d in year_dates) {
    # Check if we have a date file for this date
    date_filename <- sanitize_filename(format(as.Date(d), "%Y_%m_%d"), prefix = "date_")
    date_filepath <- file.path(output_dir_dates, date_filename)
    
    if (date_filepath %in% date_files) {
      html_file <- sub("\\.qmd$", ".html", date_filename)
      date_index_content <- paste0(
        date_index_content,
        "- [", format(as.Date(d), "%Y-%m-%d"), "](../dates/", html_file, ")\n"
      )
    }
  }
  date_index_content <- paste0(date_index_content, "\n")
}

date_index_file <- file.path(output_dir_indexes, "by-date.qmd")
writeLines(date_index_content, date_index_file, useBytes = TRUE)

message("Generated 4 index pages.")

# --- Generate _quarto.yml ---
# Using website type instead of book - better for many pages, lower memory usage
# Key: embed-resources: false prevents loading all resources into memory per page
message("Generating ", quarto_yaml_file, "...")
quarto_config <- list(
  project = list(
    type = "website",
    `output-dir` = "_book"
  ),
  website = list(
    title = "Stortingets interesseregister",
    description = "Track changes in Norwegian politicians' registered financial interests over time",
    `repo-url` = "https://github.com/andjar/Stortingets-interesseregister",
    `repo-branch` = "main",
    `repo-actions` = list("edit", "issue"),
    `reader-mode` = TRUE,
    navbar = list(
      title = "Stortingets interesseregister",
      left = list(
        list(text = "Home", href = index_qmd_file),
        list(text = "By Party", href = party_index_file),
        list(text = "By Name", href = alpha_index_file),
        list(text = "By Region", href = region_index_file),
        list(text = "Timeline", href = date_index_file)
      ),
      right = list(
        list(icon = "github", href = "https://github.com/andjar/Stortingets-interesseregister")
      )
    ),
    sidebar = list(
      list(
        id = "politicians",
        title = "Politicians",
        style = "docked",
        search = TRUE,
        `collapse-level` = 1,
        contents = list(
          list(
            section = "Browse",
            contents = list(
              list(text = "By Party", href = party_index_file),
              list(text = "By Name (A-Å)", href = alpha_index_file),
              list(text = "By Region", href = region_index_file),
              list(text = "Timeline", href = date_index_file)
            )
          ),
          list(
            section = "All Politicians",
            contents = as.list(politician_files)
          )
        )
      )
    ),
    `page-navigation` = TRUE
  ),
  format = list(
    html = list(
      theme = "cosmo",
      toc = TRUE,
      `toc-depth` = 2L,
      `embed-resources` = FALSE  # CRITICAL: prevents memory accumulation
    )
  )
)

write_yaml(quarto_config, quarto_yaml_file, handlers = list(logical = yaml::verbatim_logical))
message(quarto_yaml_file, " generated successfully.")
message("Script finished.")
