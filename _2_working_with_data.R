#### Libraries ####

library(tidyverse)

#### Declarations ####

prepare_script <- "_1_prepare_data.R"
working_sheet <- "messy_excel_long"

baseline_wave <- 1L

overwrite_results <- FALSE

# Start with four measurements to keep the output manageable.
measurement_columns <- c(
  "alp_u_l",
  "bilirubin_mg_dl",
  "albumin_g_dl",
  "elastography_kpa"
)

measurement_labels <- c(
  "alp_u_l" = "ALP (U/L)",
  "bilirubin_mg_dl" = "Bilirubin (mg/dL)",
  "albumin_g_dl" = "Albumin (g/dL)",
  "elastography_kpa" = "Elastography (kPa)"
)

required_columns <- c(
  "id",
  "wave",
  "age",
  "gender",
  "ibd",
  measurement_columns
)

plot_names <- c(
  "baseline_distributions",
  "measurement_trends",
  "missing_measurements"
)

# A fixed seed makes the jittered points appear in the same places.
jitter_seed <- 2026
jitter_width <- 0.15


#### Read and prepare data ####

# In a fresh session, prepare the data first.
# Otherwise, reuse the existing container without resetting it.
if (!exists("data", inherits = FALSE)) {
  if (!file.exists(prepare_script)) {
    stop("Preparation script not found: ", prepare_script)
  }

  source(prepare_script)
}

if (!is.list(data) || is.data.frame(data)) {
  stop("'data' must be the project's list container.")
}

if (
  !is.list(data$cleaned) ||
  is.data.frame(data$cleaned) ||
  !(working_sheet %in% names(data$cleaned))
) {
  stop(
    "The cleaned sheet '", working_sheet, "' is not available.",
    "\nRun _1_prepare_data.R before continuing."
  )
}

# Explicitly select the dataset for this task.
d <- data$cleaned[[working_sheet]]


#### Check data ####

missing_columns <- setdiff(required_columns, names(d))

if (length(missing_columns) > 0L) {
  stop(
    "Required columns are missing: ",
    paste(missing_columns, collapse = ", ")
  )
}

numeric_columns <- c("wave", "age", measurement_columns)

columns_are_numeric <- map_lgl(
  d[numeric_columns],
  is.numeric
)

if (!all(columns_are_numeric)) {
  stop(
    "These columns should be numeric: ",
    paste(
      names(columns_are_numeric)[!columns_are_numeric],
      collapse = ", "
    ),
    "\nCheck the cleaning steps in _1_prepare_data.R."
  )
}

if (anyNA(d$id) || anyNA(d$wave)) {
  stop("Participant IDs and wave numbers must not be missing.")
}

duplicate_visits <- d |>
  count(id, wave, name = "n_rows") |>
  filter(n_rows > 1L)

if (nrow(duplicate_visits) > 0L) {
  print(duplicate_visits)
  stop("Some participants have more than one row for the same wave.")
}

# Check output containers before replacing any existing results.
if (exists("plots", inherits = FALSE)) {
  if (!is.list(plots) || is.data.frame(plots)) {
    stop("'plots' must be a named list of plot objects.")
  }

  existing_plot_names <- intersect(plot_names, names(plots))
} else {
  existing_plot_names <- character()
}

if (
  !overwrite_results &&
  (
    !is.null(data$descriptive) ||
    length(existing_plot_names) > 0L
  )
) {
  stop(
    "Some descriptive results or project plots already exist.",
    "\nSet overwrite_results <- TRUE to intentionally replace them."
  )
}


#### Prepare analysis tables ####

# Baseline means the first wave selected in Declarations.
# Using baseline prevents counting repeated visits as new participants.
baseline_data <- d |>
  filter(wave == baseline_wave)

if (nrow(baseline_data) != n_distinct(d$id)) {
  stop(
    "Not every participant has a row at baseline_wave = ",
    baseline_wave,
    ". Check the selected wave."
  )
}

# Put the selected measurements into two columns:
# measurement identifies the variable; value holds its number.
#
# Missing values are retained so we can count them.
measurement_data <- d |>
  select(id, wave, all_of(measurement_columns)) |>
  pivot_longer(
    cols = all_of(measurement_columns),
    names_to = "measurement",
    values_to = "value",
    values_drop_na = FALSE
  ) |>
  mutate(
    measurement_label = factor(
      measurement,
      levels = measurement_columns,
      labels = unname(measurement_labels[measurement_columns])
    )
  )

baseline_measurement_data <- measurement_data |>
  filter(wave == baseline_wave)


#### Descriptive statistics ####

study_overview <- d |>
  summarise(
    n_participants = n_distinct(id),
    n_visits = n(),
    n_waves = n_distinct(wave)
  )

baseline_age_summary <- baseline_data |>
  summarise(
    n_participants = n(),
    n_age_observed = sum(!is.na(age)),
    n_age_missing = sum(is.na(age)),

    mean_age = if (n_age_observed > 0L) {
      mean(age, na.rm = TRUE)
    } else {
      NA_real_
    },

    median_age = if (n_age_observed > 0L) {
      median(age, na.rm = TRUE)
    } else {
      NA_real_
    },

    sd_age = sd(age, na.rm = TRUE)
  )

baseline_gender_counts <- baseline_data |>
  count(gender, name = "n_participants") |>
  mutate(
    percent = 100 * n_participants / sum(n_participants)
  )

baseline_ibd_counts <- baseline_data |>
  count(ibd, name = "n_participants") |>
  mutate(
    percent = 100 * n_participants / sum(n_participants)
  )

# Summarize each measurement separately within each wave.
# Missing values are counted but excluded from numeric statistics.
measurement_summary <- measurement_data |>
  group_by(wave, measurement, measurement_label) |>
  summarise(
    n_total = n(),
    n_observed = sum(!is.na(value)),
    n_missing = sum(is.na(value)),
    percent_missing = 100 * n_missing / n_total,

    mean_value = if (n_observed > 0L) {
      mean(value, na.rm = TRUE)
    } else {
      NA_real_
    },

    median_value = if (n_observed > 0L) {
      median(value, na.rm = TRUE)
    } else {
      NA_real_
    },

    sd_value = sd(value, na.rm = TRUE),

    minimum_value = if (n_observed > 0L) {
      min(value, na.rm = TRUE)
    } else {
      NA_real_
    },

    maximum_value = if (n_observed > 0L) {
      max(value, na.rm = TRUE)
    } else {
      NA_real_
    },

    .groups = "drop"
  )

baseline_measurement_summary <- measurement_summary |>
  filter(wave == baseline_wave)


#### Create plots ####

# Initialize the plot container only if it does not exist.
if (!exists("plots", inherits = FALSE)) {
  plots <- list()
}

# Plot 1: baseline distributions.
#
# Points with missing measurements cannot be drawn.
# They remain in the analysis tables and missingness counts.
#
# Hide the boxplot's separate outlier symbols because the jitter
# layer already displays every observed value.
plots$baseline_distributions <- baseline_measurement_data |>
  ggplot(aes(x = "", y = value)) +
  geom_boxplot(
    width = 0.4,
    fill = "lightblue",
    outlier.shape = NA,
    na.rm = TRUE
  ) +
  geom_point(
    position = position_jitter(
      width = jitter_width,
      height = 0,
      seed = jitter_seed
    ),
    alpha = 0.6,
    size = 1.8,
    na.rm = TRUE
  ) +
  facet_wrap(
    vars(measurement_label),
    scales = "free_y"
  ) +
  labs(
    title = "Measurement distributions at baseline",
    subtitle = paste("Wave", baseline_wave),
    x = NULL,
    y = "Value",
    caption = "Each point is one observed participant measurement."
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )

# Plot 2: the median of observed values at each wave.
# This is a group summary, not an individual participant's path.
plots$measurement_trends <- measurement_summary |>
  ggplot(aes(x = wave, y = median_value, group = 1)) +
  geom_line(
    colour = "steelblue",
    linewidth = 0.8,
    na.rm = TRUE
  ) +
  geom_point(
    colour = "steelblue",
    size = 2.5,
    na.rm = TRUE
  ) +
  facet_wrap(
    vars(measurement_label),
    scales = "free_y"
  ) +
  scale_x_continuous(
    breaks = sort(unique(d$wave))
  ) +
  labs(
    title = "Median measurements across waves",
    subtitle = "Each point summarizes the available values at one wave",
    x = "Wave",
    y = "Median value",
    caption = "Missing values are excluded; the observed participants may differ by wave."
  ) +
  theme_minimal()

# Plot 3: measurement missingness at each wave.
plots$missing_measurements <- measurement_summary |>
  ggplot(aes(x = factor(wave), y = percent_missing)) +
  geom_col(
    fill = "coral",
    width = 0.7
  ) +
  facet_wrap(vars(measurement_label)) +
  scale_y_continuous(
    limits = c(0, 100)
  ) +
  labs(
    title = "Missing measurements across waves",
    x = "Wave",
    y = "Missing values (%)",
    caption = "All panels use the same 0–100% scale."
  ) +
  theme_minimal()


#### Store descriptive results ####

# This assignment intentionally replaces the descriptive component
# when overwrite_results is TRUE. Other data components are unchanged.
data$descriptive <- list(
  study_overview = study_overview,
  baseline_age_summary = baseline_age_summary,
  baseline_gender_counts = baseline_gender_counts,
  baseline_ibd_counts = baseline_ibd_counts,
  baseline_measurement_summary = baseline_measurement_summary,
  measurement_summary_by_wave = measurement_summary
)


#### Inspect results ####

print(data$descriptive$study_overview)
print(data$descriptive$baseline_age_summary)
print(data$descriptive$baseline_gender_counts)
print(data$descriptive$baseline_ibd_counts)
print(data$descriptive$baseline_measurement_summary)

print(plots$baseline_distributions)
print(plots$measurement_trends)
print(plots$missing_measurements)