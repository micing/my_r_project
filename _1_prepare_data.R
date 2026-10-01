#### Libraries ####

library(tidyverse)
library(readxl)

#### Declarations ####

workbook_path <- file.path("data", "messy_psc_data.xlsx")

overwrite_raw <- TRUE
overwrite_cleaned <- TRUE

# Import everything as character so cleaning is explicit.
import_column_type <- "text"

# Treat blank Excel cells as missing, but preserve literal "NA".
import_missing_values <- ""

# Preserve spaces during import so data$raw remains uncleaned.
trim_whitespace <- FALSE

# Text values that should become actual missing values.
# Matching will ignore capitalization and surrounding spaces.
missing_value_markers <- c("", "NA")

# Dates created by _0_create_data.R:
# Visit dates:     year-month-day, such as "2020-03-15".
# Diagnosis dates: month/day/year, such as "03/15/2010".
visit_date_columns <- c(
  "date"
)

diagnosis_date_columns <- c(
  "psc_diagnosis_date",
  "ibd_diagnosis_date"
)

# Numeric columns may contain literal "NA" instead of a number.
numeric_with_literal_na_columns <- c(
  "wave",
  "age",
  "alp_u_l",
  "ggt_u_l",
  "alt_u_l",
  "ast_u_l",
  "bilirubin_mg_dl",
  "albumin_g_dl",
  "platelets_10e9_l",
  "crp_mg_l",
  "inr",
  "creatinine_mg_dl",
  "ca19_9_u_ml",
  "elastography_kpa"
)

messy_gender_columns <- c(
  "gender"
)

# After trimming spaces and converting to lowercase,
# these are the accepted gender labels.
gender_lookup <- c(
  "female" = "Female",
  "f" = "Female",
  "male" = "Male",
  "m" = "Male"
)

# Remove this suffix only when identifying column types.
# The actual wide column names remain unchanged.
wide_column_suffix <- "_wave[0-9]+$"


#### Functions ####

# Input: a character column and missing-value markers.
# Output: trimmed text with the selected markers replaced by NA.
clean_text <- function(column_values, missing_markers) {
  column_values <- str_trim(column_values)

  missing_here <- str_to_upper(column_values) %in%
    str_to_upper(missing_markers)

  column_values[missing_here] <- NA_character_

  return(column_values)
}

# Input: cleaned character values and their column name.
# Output: numbers, or an error if a nonmissing value cannot be parsed.
clean_numeric <- function(column_values, column_name) {
  numeric_values <- suppressWarnings(
    parse_double(column_values, na = character())
  )

  invalid_here <- !is.na(column_values) & is.na(numeric_values)

  if (any(invalid_here)) {
    stop(
      "Cannot convert column '", column_name, "' to numbers.",
      "\nUnexpected values: ",
      paste(unique(column_values[invalid_here]), collapse = ", ")
    )
  }

  return(numeric_values)
}

# Input: cleaned date text, its column name, and "ymd" or "mdy".
# Output: Date values, or an error if parsing fails.
clean_date <- function(column_values, column_name, date_order) {
  if (date_order == "ymd") {
    date_values <- ymd(column_values, quiet = TRUE)
  } else if (date_order == "mdy") {
    date_values <- mdy(column_values, quiet = TRUE)
  } else {
    stop("Unsupported date order: ", date_order)
  }

  invalid_here <- !is.na(column_values) & is.na(date_values)

  if (any(invalid_here)) {
    stop(
      "Cannot convert column '", column_name, "' to dates.",
      "\nExpected date order: ", date_order,
      "\nUnexpected values: ",
      paste(unique(column_values[invalid_here]), collapse = ", ")
    )
  }

  return(date_values)
}

# Input: cleaned gender text, its column name, and a label lookup.
# Output: standardized labels, or an error for an unknown label.
clean_gender <- function(column_values, column_name, label_lookup) {
  gender_keys <- str_to_lower(column_values)

  invalid_here <- !is.na(gender_keys) &
    !(gender_keys %in% names(label_lookup))

  if (any(invalid_here)) {
    stop(
      "Unrecognized labels in column '", column_name, "'.",
      "\nUnexpected values: ",
      paste(unique(column_values[invalid_here]), collapse = ", "),
      "\nAdd the intended labels to gender_lookup."
    )
  }

  gender_values <- unname(label_lookup[gender_keys])

  return(gender_values)
}


#### Check inputs ####

if (!file.exists(workbook_path)) {
  stop(
    "Workbook not found: ", workbook_path,
    "\nCurrent working directory: ", getwd(),
    "\nCheck the path or run _0_create_data.R first."
  )
}

# Avoid confusing our container with R's built-in data() function.
data_exists <- exists("data", inherits = FALSE)

if (data_exists) {
  if (!is.list(data) || is.data.frame(data)) {
    stop("The existing 'data' object must be a list.")
  }

  if (!is.null(data$raw) && !overwrite_raw) {
    stop(
      "data$raw already exists.",
      "\nSet overwrite_raw <- TRUE to intentionally replace it."
    )
  }

  if (!is.null(data$cleaned) && !overwrite_cleaned) {
    stop(
      "data$cleaned already exists.",
      "\nSet overwrite_cleaned <- TRUE to intentionally replace it."
    )
  }
}


#### Read and prepare data ####

sheet_names <- excel_sheets(workbook_path)

if (length(sheet_names) == 0L) {
  stop("The workbook contains no sheets.")
}

raw_sheets <- sheet_names |>
  set_names(sheet_names) |>
  map(function(sheet_name) {
    read_excel(
      path = workbook_path,
      sheet = sheet_name,
      col_types = import_column_type,
      na = import_missing_values,
      trim_ws = trim_whitespace,
      .name_repair = "unique"
    )
  })


#### Clean data ####

# Work on copies of the imported tables.
# Nothing in raw_sheets is changed.
cleaned_sheets <- raw_sheets |>
  map(function(sheet_data) {

    # Match wide names to their underlying variable.
    # Example: "alp_u_l_wave1" becomes "alp_u_l" for matching only.
    column_names <- names(sheet_data)

    base_column_names <- str_remove(
      column_names,
      wide_column_suffix
    )

    sheet_visit_date_columns <- column_names[
      base_column_names %in% visit_date_columns
    ]

    sheet_diagnosis_date_columns <- column_names[
      base_column_names %in% diagnosis_date_columns
    ]

    sheet_numeric_columns <- column_names[
      base_column_names %in% numeric_with_literal_na_columns
    ]

    sheet_gender_columns <- column_names[
      base_column_names %in% messy_gender_columns
    ]

    # Clean whitespace and missing-value text before parsing.
    cleaned_sheet <- sheet_data |>
      mutate(
        across(
          where(is.character),
          ~ clean_text(.x, missing_value_markers)
        )
      )

    # Convert the selected columns to their intended types.
    cleaned_sheet <- cleaned_sheet |>
      mutate(
        across(
          all_of(sheet_visit_date_columns),
          ~ clean_date(.x, cur_column(), date_order = "ymd")
        ),

        across(
          all_of(sheet_diagnosis_date_columns),
          ~ clean_date(.x, cur_column(), date_order = "mdy")
        ),

        across(
          all_of(sheet_numeric_columns),
          ~ clean_numeric(.x, cur_column())
        ),

        across(
          all_of(sheet_gender_columns),
          ~ clean_gender(.x, cur_column(), gender_lookup)
        )
      )

    return(cleaned_sheet)
  })


#### Check cleaned data ####

sheet_summary <- tibble(
  sheet_name = names(raw_sheets),
  raw_rows = map_int(raw_sheets, nrow),
  cleaned_rows = map_int(cleaned_sheets, nrow),
  columns = map_int(cleaned_sheets, ncol)
)

if (any(sheet_summary$raw_rows != sheet_summary$cleaned_rows)) {
  stop("Cleaning unexpectedly changed the number of rows.")
}

# Keep duplicate visits for inspection rather than deleting them.
if ("messy_excel_long" %in% names(cleaned_sheets)) {
  duplicate_visits <- cleaned_sheets$messy_excel_long |>
    count(id, wave, name = "n_rows") |>
    filter(n_rows > 1L)

  if (nrow(duplicate_visits) > 0L) {
    print(duplicate_visits)
    stop("Duplicate participant-and-wave combinations were found.")
  }
}


#### Store results ####

# Store results only after import, cleaning, and checks succeed.
if (!data_exists) {
  data <- list()
}

if (!is.null(data$raw)) {
  message("Replacing existing data$raw.")
}

if (!is.null(data$cleaned)) {
  message(
    "Replacing existing data$cleaned. ",
    "Existing models are not updated automatically."
  )
}

if (length(sheet_names) > 1L) {
  data$raw <- raw_sheets
  data$cleaned <- cleaned_sheets
} else {
  data$raw <- raw_sheets[[1]]
  data$cleaned <- cleaned_sheets[[1]]
}

data$workbook_path <- workbook_path
data$sheet_names <- sheet_names
data$sheet_summary <- sheet_summary

print(data$sheet_summary)

message("Import and cleaning complete. No rows were removed.")