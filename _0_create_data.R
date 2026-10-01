# ============================================================
# _0_create_data.R
#
# Create fictional, deliberately messy data for learning R.
#
# The Excel workbook contains:
#   messy_excel_long: one row per participant per wave
#   messy_excel_wide: one row per participant
#
# These are practice data, not a clinical model of PSC.
# ============================================================


#### Libraries ####

library(tidyverse)
library(writexl)


#### Declarations ####

# Files and folders.
output_directory <- "data"
output_file <- file.path(output_directory, "messy_psc_data.xlsx")

long_sheet_name <- "messy_excel_long"
wide_sheet_name <- "messy_excel_wide"

# Set TRUE only when you want to replace an existing workbook.
overwrite <- FALSE

# Using the same seed makes the random results repeatable.
random_seed <- 2026

n_participants <- 40L
n_waves <- 5L

# Participant characteristics.
age_range <- 25:75

gender_options <- c("Female", "Male")

ibd_options <- c("Yes", "No")
ibd_probabilities <- c(0.7, 0.3)

# Deliberately inconsistent text for our cleaning exercise.
messy_gender_values <- c(
  "female",
  " Male ",
  "F",
  "M",
  "FEMALE",
  "male"
)

# Dates and visit timing.
baseline_start_date <- ymd("2020-01-01")
baseline_day_offsets <- 0:300

psc_start_date <- ymd("2010-01-01")
psc_day_offsets <- 0:3000

ibd_start_date <- ymd("2005-01-01")
ibd_day_offsets <- 0:4500

days_between_waves <- 365L
visit_jitter_range <- -21:21

visit_date_format <- "%Y-%m-%d"
diagnosis_date_format <- "%m/%d/%Y"

# Missing-data settings.
measurement_missing_probability <- 0.08
missing_text <- "NA"

# Participants have different underlying measurement levels.
# This is just a way to add variation, not a clinical model.
multiplier_min <- 0.7
multiplier_max <- 1.6

# Settings for measurements generated with rlnorm().
# Named vectors let us retrieve settings using a biomarker name.
#
# The median describes the middle of the generated distribution.
# log_sd controls how spread out the values are.
lognormal_medians <- c(
  alp_u_l = 180,
  ggt_u_l = 120,
  alt_u_l = 45,
  ast_u_l = 40,
  bilirubin_mg_dl = 0.9,
  crp_mg_l = 3,
  creatinine_mg_dl = 0.9,
  ca19_9_u_ml = 20,
  elastography_kpa = 7
)

lognormal_log_sd <- c(
  alp_u_l = 0.35,
  ggt_u_l = 0.45,
  alt_u_l = 0.40,
  ast_u_l = 0.35,
  bilirubin_mg_dl = 0.40,
  crp_mg_l = 0.90,
  creatinine_mg_dl = 0.25,
  ca19_9_u_ml = 0.75,
  elastography_kpa = 0.30
)

# Settings for measurements generated with rnorm().
normal_means <- c(
  albumin_g_dl = 4.0,
  platelets_10e9_l = 240,
  inr = 1.05
)

normal_sd <- c(
  albumin_g_dl = 0.35,
  platelets_10e9_l = 55,
  inr = 0.15
)

# Artificial limits for our practice data.
albumin_min <- 2.5
albumin_max <- 5.2
platelets_min <- 60
inr_min <- 0.8

# Number of decimal places to keep.
measurement_digits <- c(
  alp_u_l = 0,
  ggt_u_l = 0,
  alt_u_l = 0,
  ast_u_l = 0,
  bilirubin_mg_dl = 2,
  albumin_g_dl = 1,
  platelets_10e9_l = 0,
  crp_mg_l = 1,
  inr = 2,
  creatinine_mg_dl = 2,
  ca19_9_u_ml = 1,
  elastography_kpa = 1
)

measurement_columns <- names(measurement_digits)

# Use these columns to link repeated visits to a participant.
participant_columns <- c(
  "id",
  "gender",
  "ibd",
  "psc_diagnosis_date",
  "ibd_diagnosis_date"
)

preview_rows <- 10L


#### Functions ####

# Replace randomly selected values with actual R missing values.
# Input: a vector and the probability of making each value missing.
# Output: the same vector with some values replaced by NA.
add_missing_values <- function(x, probability) {
  missing_here <- runif(length(x)) < probability

  x[missing_here] <- NA

  return(x)
}

# Replace actual missing values with text such as "NA".
# A column containing missing values becomes a text column.
# Columns without missing values keep their existing type.
write_missing_as_text <- function(x, missing_label) {
  if (anyNA(x)) {
    x <- as.character(x)
    x[is.na(x)] <- missing_label
  }

  return(x)
}


#### Check settings and output path ####

# Stop early if the basic settings cannot produce our dataset.
if (
  length(n_participants) != 1L ||
  !is.finite(n_participants) ||
  n_participants < 1 ||
  n_participants != floor(n_participants)
) {
  stop("n_participants must be a positive whole number.")
}

if (
  length(n_waves) != 1L ||
  !is.finite(n_waves) ||
  n_waves < 1 ||
  n_waves != floor(n_waves)
) {
  stop("n_waves must be a positive whole number.")
}

if (
  length(measurement_missing_probability) != 1L ||
  !is.finite(measurement_missing_probability) ||
  measurement_missing_probability < 0 ||
  measurement_missing_probability > 1
) {
  stop("measurement_missing_probability must be between 0 and 1.")
}

if (file.exists(output_file) && !overwrite) {
  stop(
    "The workbook already exists: ", output_file,
    "\nSet overwrite <- TRUE to replace it."
  )
}

dir.create(
  output_directory,
  showWarnings = FALSE,
  recursive = TRUE
)

message("Working directory: ", getwd())

set.seed(random_seed)


#### Create participant data ####

# A tibble is a table.
# Here, each row represents one participant.
participants <- tibble(
  id = sprintf("PSC%03d", seq_len(n_participants)),

  baseline_age = sample(
    age_range,
    size = n_participants,
    replace = TRUE
  ),

  gender = sample(
    gender_options,
    size = n_participants,
    replace = TRUE
  ),

  ibd = sample(
    ibd_options,
    size = n_participants,
    replace = TRUE,
    prob = ibd_probabilities
  ),

  baseline_date = baseline_start_date +
    sample(
      baseline_day_offsets,
      size = n_participants,
      replace = TRUE
    ),

  psc_diagnosis_date = psc_start_date +
    sample(
      psc_day_offsets,
      size = n_participants,
      replace = TRUE
    ),

  ibd_diagnosis_date = ibd_start_date +
    sample(
      ibd_day_offsets,
      size = n_participants,
      replace = TRUE
    ),

  measurement_multiplier = runif(
    n_participants,
    min = multiplier_min,
    max = multiplier_max
  )
) |>
  mutate(
    # A participant without IBD has no IBD diagnosis date.
    ibd_diagnosis_date = if_else(
      ibd == "Yes",
      ibd_diagnosis_date,
      as.Date(NA)
    )
  )

# Replace a few gender entries with deliberately messy text.
# min() keeps this safe if we choose fewer than six participants.
messy_gender_rows <- seq_len(
  min(n_participants, length(messy_gender_values))
)

participants$gender[messy_gender_rows] <-
  messy_gender_values[messy_gender_rows]


#### Create repeated visits ####

# crossing() creates every participant-and-wave combination.
# left_join() adds the participant information to each visit.
visits <- crossing(
  id = participants$id,
  wave = seq_len(n_waves)
) |>
  left_join(participants, by = "id") |>
  mutate(
    # Later visits can happen a little early or late.
    # The first wave uses the baseline date exactly.
    visit_jitter_days = if_else(
      wave == 1L,
      0L,
      sample(
        visit_jitter_range,
        size = n(),
        replace = TRUE
      )
    ),

    date = baseline_date +
      days_between_waves * (wave - 1L) +
      visit_jitter_days,

    # This simplified age increases by one per wave.
    age = baseline_age + wave - 1L
  )


#### Create synthetic measurements ####

# rlnorm() generates positive values with some larger values.
# rnorm() generates values around a chosen average.
#
# [["alp_u_l"]] retrieves the setting named "alp_u_l".
visits <- visits |>
  mutate(
    alp_u_l = rlnorm(
      n(),
      meanlog = log(
        lognormal_medians[["alp_u_l"]] * measurement_multiplier
      ),
      sdlog = lognormal_log_sd[["alp_u_l"]]
    ),

    ggt_u_l = rlnorm(
      n(),
      meanlog = log(
        lognormal_medians[["ggt_u_l"]] * measurement_multiplier
      ),
      sdlog = lognormal_log_sd[["ggt_u_l"]]
    ),

    alt_u_l = rlnorm(
      n(),
      meanlog = log(lognormal_medians[["alt_u_l"]]),
      sdlog = lognormal_log_sd[["alt_u_l"]]
    ),

    ast_u_l = rlnorm(
      n(),
      meanlog = log(lognormal_medians[["ast_u_l"]]),
      sdlog = lognormal_log_sd[["ast_u_l"]]
    ),

    bilirubin_mg_dl = rlnorm(
      n(),
      meanlog = log(
        lognormal_medians[["bilirubin_mg_dl"]] *
          measurement_multiplier
      ),
      sdlog = lognormal_log_sd[["bilirubin_mg_dl"]]
    ),

    albumin_g_dl = pmin(
      albumin_max,
      pmax(
        albumin_min,
        rnorm(
          n(),
          mean = normal_means[["albumin_g_dl"]],
          sd = normal_sd[["albumin_g_dl"]]
        )
      )
    ),

    platelets_10e9_l = pmax(
      platelets_min,
      rnorm(
        n(),
        mean = normal_means[["platelets_10e9_l"]],
        sd = normal_sd[["platelets_10e9_l"]]
      )
    ),

    crp_mg_l = rlnorm(
      n(),
      meanlog = log(lognormal_medians[["crp_mg_l"]]),
      sdlog = lognormal_log_sd[["crp_mg_l"]]
    ),

    inr = pmax(
      inr_min,
      rnorm(
        n(),
        mean = normal_means[["inr"]],
        sd = normal_sd[["inr"]]
      )
    ),

    creatinine_mg_dl = rlnorm(
      n(),
      meanlog = log(lognormal_medians[["creatinine_mg_dl"]]),
      sdlog = lognormal_log_sd[["creatinine_mg_dl"]]
    ),

    ca19_9_u_ml = rlnorm(
      n(),
      meanlog = log(
        lognormal_medians[["ca19_9_u_ml"]] *
          measurement_multiplier
      ),
      sdlog = lognormal_log_sd[["ca19_9_u_ml"]]
    ),

    elastography_kpa = rlnorm(
      n(),
      meanlog = log(
        lognormal_medians[["elastography_kpa"]] *
          measurement_multiplier
      ),
      sdlog = lognormal_log_sd[["elastography_kpa"]]
    )
  )

# Round each measurement using its declared decimal places.
# cur_column() tells us which column across() is currently using.
visits <- visits |>
  mutate(
    across(
      all_of(measurement_columns),
      function(x) {
        round(
          x,
          digits = measurement_digits[[cur_column()]]
        )
      }
    )
  )


#### Prepare long data ####

# Keep only the columns that belong in the workbook.
clean_long <- visits |>
  select(
    id,
    wave,
    date,
    age,
    gender,
    ibd,
    psc_diagnosis_date,
    ibd_diagnosis_date,
    all_of(measurement_columns)
  ) |>
  arrange(id, wave)

# Keep clean_long intact and make a separate messy version.
messy_excel_long <- clean_long |>
  mutate(
    across(
      all_of(measurement_columns),
      function(x) {
        add_missing_values(
          x,
          probability = measurement_missing_probability
        )
      }
    )
  )

# Guarantee one missing measurement for the cleaning exercise.
messy_excel_long$alp_u_l[1] <- NA_real_

# Export dates as text using two different formats.
messy_excel_long <- messy_excel_long |>
  mutate(
    date = format(date, visit_date_format),

    psc_diagnosis_date = format(
      psc_diagnosis_date,
      diagnosis_date_format
    ),

    ibd_diagnosis_date = format(
      ibd_diagnosis_date,
      diagnosis_date_format
    )
  ) |>
  mutate(
    # NA is a missing value; "NA" is ordinary text.
    across(
      everything(),
      function(x) {
        write_missing_as_text(
          x,
          missing_label = missing_text
        )
      }
    )
  )


#### Prepare wide data ####

# Reshape the same messy data rather than generating new values.
# This keeps both sheets consistent.
messy_excel_wide <- messy_excel_long |>
  pivot_wider(
    id_cols = all_of(participant_columns),
    names_from = wave,
    values_from = c(
      date,
      age,
      all_of(measurement_columns)
    ),
    names_glue = "{.value}_wave{wave}"
  ) |>
  arrange(id)


#### Check data ####

# Stop if the generated tables do not have the expected structure.
if (nrow(messy_excel_long) != n_participants * n_waves) {
  stop("The long dataset has an unexpected number of rows.")
}

if (
  nrow(messy_excel_wide) != n_participants ||
  n_distinct(messy_excel_long$id) != n_participants
) {
  stop("The datasets have an unexpected number of participants.")
}

visits_per_participant <- messy_excel_long |>
  count(id, name = "n_visits")

if (any(visits_per_participant$n_visits != n_waves)) {
  stop("Not every participant has the expected number of waves.")
}

duplicate_visits <- messy_excel_long |>
  count(id, wave, name = "n_rows") |>
  filter(n_rows > 1L)

if (nrow(duplicate_visits) > 0L) {
  stop("Some participant-and-wave combinations appear more than once.")
}

if (anyNA(messy_excel_long) || anyNA(messy_excel_wide)) {
  stop("Some missing values were not converted to literal text.")
}


#### Save workbook ####

# Each named list element becomes one Excel sheet.
workbook_sheets <- list(
  messy_excel_long,
  messy_excel_wide
) |>
  set_names(c(long_sheet_name, wide_sheet_name))

write_xlsx(
  x = workbook_sheets,
  path = output_file
)


#### Preview results ####

# Summarize each sheet without printing the whole dataset.
sheet_summary <- tibble(
  sheet_name = names(workbook_sheets),
  rows = map_int(workbook_sheets, nrow),
  columns = map_int(workbook_sheets, ncol)
)

message(
  "\nWorkbook created:\n",
  normalizePath(output_file, winslash = "/")
)

print(sheet_summary)

messy_excel_long |>
  select(
    id,
    wave,
    date,
    age,
    gender,
    alp_u_l,
    elastography_kpa
  ) |>
  slice_head(n = preview_rows) |>
  print()