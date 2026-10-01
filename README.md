# Learning R with Messy Practice Data

A small R project for learning how to import, clean, summarize, and plot data using the tidyverse in Positron.

The project uses fictional data about participants with primary sclerosing cholangitis (PSC), measured across repeated visits.

**These are practice data, not real patient data or a clinical model. Results must not be interpreted as clinical evidence.**

## Project structure

```text
project/
├── README.md
├── docs/
└── prompts/
    └── system_prompt.txt
├── _0_create_data.R
├── _1_prepare_data.R
├── _2_working_with_data.R
└── data/
    └── messy_psc_data.xlsx
```

The `data/` folder and workbook are created by `_0_create_data.R`.

| File | Purpose |
|---|---|
| `_0_create_data.R` | Creates a fictional, deliberately messy Excel workbook. |
| `_1_prepare_data.R` | Imports the workbook and creates cleaned datasets. |
| `_2_working_with_data.R` | Creates descriptive statistics and plots. |
| `system_prompt.txt` | Records coding preferences and instructions for the AI tutor. |
| `README.md` | Explains the project and how to run it. |

## Setup

Open the project folder in Positron and start an R session.

Install these packages once if they are not already installed:

```r
install.packages(c(
  "tidyverse",
  "readxl",
  "writexl",
  "lubridate"
))
```

Package installation is separate from the analysis scripts.

Run scripts from the project folder. Paths such as `data/messy_psc_data.xlsx` are relative to that folder.

To check the current working directory:

```r
getwd()
```

## Run the project

For the first run, execute these commands in order:

```r
source("_0_create_data.R")
source("_1_prepare_data.R")
source("_2_working_with_data.R")
```

### 1. Create the practice workbook

`_0_create_data.R` creates:

- 40 participants.
- 5 waves, or repeated visits, per participant.
- Participant characteristics, diagnosis dates, and measurements.
- Deliberately inconsistent gender labels.
- Dates stored as text in two formats.
- Missing values stored as literal `"NA"` text.

The workbook contains two sheets:

| Sheet | Structure | Expected rows with default settings |
|---|---|---:|
| `messy_excel_long` | One row per participant per wave | 200 |
| `messy_excel_wide` | One row per participant, with separate columns for each wave | 40 |

Both sheets represent the same underlying data.

The random seed makes generation repeatable when the settings remain unchanged.

### 2. Import and clean the workbook

`_1_prepare_data.R` imports columns as character so that conversion happens explicitly during cleaning.

It:

- Preserves imported data under `data$raw`.
- Removes surrounding whitespace.
- Converts blanks and literal `"NA"` text to actual missing values.
- Converts selected measurement, age, and wave columns to numbers.
- Parses visit dates as year-month-day.
- Parses diagnosis dates as month/day/year.
- Standardizes gender labels to `"Female"` and `"Male"`.
- Handles wide columns such as `date_wave1` and `alp_u_l_wave1`.
- Checks for values that cannot be parsed.
- Checks for duplicate participant-and-wave combinations in the long sheet.

Cleaned results are stored under `data$cleaned`.

Cleaning does not fill in missing measurements or remove rows.

### 3. Explore the cleaned data

`_2_working_with_data.R` uses the cleaned long dataset to create:

- A study overview.
- Baseline age statistics.
- Baseline gender and IBD counts.
- Measurement summaries at baseline and across waves.
- Three plots.

It reuses prepared data when available. In a fresh session, it runs `_1_prepare_data.R` if the `data` container does not exist.

## Main R objects

### `data`

`data` is the main list containing project datasets and results.

```r
# Imported, uncleaned data:
data$raw$messy_excel_long
data$raw$messy_excel_wide

# Cleaned data:
data$cleaned$messy_excel_long
data$cleaned$messy_excel_wide

# Descriptive results:
data$descriptive
```

With multiple workbook sheets, `data$raw` and `data$cleaned` are named lists of tables. With a single sheet, each is a single table.

### `d`

`d` is the dataset selected for the current task.

```r
d <- data$cleaned$messy_excel_long

glimpse(d)
```

Work with `d` rather than modifying `data$raw`.

### `plots`

`plots` is a named list of ggplot objects.

```r
names(plots)

plots$baseline_distributions
plots$measurement_trends
plots$missing_measurements
```

Plots are stored in the R session. The script does not automatically save them as image files.

## Descriptive statistics

Explore saved results with:

```r
data$descriptive$study_overview
data$descriptive$baseline_age_summary
data$descriptive$baseline_gender_counts
data$descriptive$baseline_ibd_counts
data$descriptive$baseline_measurement_summary
data$descriptive$measurement_summary_by_wave
```

The measurement summaries include:

- Number of observed and missing values.
- Percentage missing.
- Mean: the average.
- Median: the middle value.
- Standard deviation: a measure of spread.
- Minimum and maximum.

Numeric summaries exclude missing values. Missing values are counted separately.

Participant characteristics are summarized at baseline so that repeated visits do not count the same participant multiple times.

## Plots

### Baseline distributions

```r
plots$baseline_distributions
```

Boxplots and individual points show the available measurements at the first selected wave.

Each measurement has its own panel and vertical scale.

### Measurement trends

```r
plots$measurement_trends
```

Points show the median of available values at each wave.

The lines connect group summaries, not individual participant trajectories. Because measurements can be missing, the observed participants may differ between waves.

### Missing measurements

```r
plots$missing_measurements
```

Bars show the percentage of missing values for each selected measurement at each wave.

All panels use the same 0–100% scale.

## Missing values

In R, these are different:

```r
NA      # An actual missing value
"NA"    # Text containing the letters N and A
```

The workbook deliberately contains `"NA"` text. The preparation script converts it to actual missing values before parsing columns.

Missing IBD diagnosis dates are expected for participants without IBD.

The analysis does not replace missing measurements with guessed values.

## Change settings

Configurable settings are near the top of each script under:

```r
#### Declarations ####
```

Examples include:

| Script | Settings |
|---|---|
| `_0_create_data.R` | Participant count, wave count, random seed, missingness probability |
| `_1_prepare_data.R` | Workbook path, column lists, missing-value markers, gender labels |
| `_2_working_with_data.R` | Baseline wave, selected measurements, plot labels |

When adding a measurement to `_2_working_with_data.R`, update both `measurement_columns` and `measurement_labels`.

## Rerunning scripts safely

The scripts include overwrite settings to prevent accidental replacement.

### Replace the generated workbook

In `_0_create_data.R`:

```r
overwrite <- TRUE
```

**This replaces the existing Excel workbook.**

### Replace imported and cleaned objects

In `_1_prepare_data.R`:

```r
overwrite_raw <- TRUE
overwrite_cleaned <- TRUE
```

This replaces `data$raw` and `data$cleaned` in the R session. It does not change the workbook.

### Replace descriptive results and project plots

In `_2_working_with_data.R`:

```r
overwrite_results <- TRUE
```

This replaces `data$descriptive` and the three plots created by that script. Unrelated plots and other data components are preserved.

After changing or regenerating data, rerun preparation and analysis. Existing summaries and plots do not update automatically.

## Troubleshooting

### “Workbook not found”

Run `_0_create_data.R` first and check that Positron is open in the project folder.

```r
getwd()
file.exists(file.path("data", "messy_psc_data.xlsx"))
```

### “The workbook already exists”

Keep the existing workbook, or intentionally enable `overwrite <- TRUE` in `_0_create_data.R`.

### “data$raw already exists” or “data$cleaned already exists”

Enable the corresponding overwrite settings in `_1_prepare_data.R` if replacement is intended.

### “Some descriptive results or project plots already exist”

Set `overwrite_results <- TRUE` in `_2_working_with_data.R` to replace those results.

### A value cannot be converted to a number or date

Read the error message and inspect the named column in `data$raw`.

Correct the cleaning rules rather than silently discarding the value.

### An unfamiliar gender label is found

Add its intended standardized label to `gender_lookup` in `_1_prepare_data.R`.

## Learning goals

This project introduces:

- Scripts and project-relative paths.
- Lists and tables.
- Selecting and filtering data with dplyr.
- Reshaping data with tidyr.
- Working with lists using purrr.
- Parsing dates with lubridate.
- Explicit missing-value handling.
- Grouped descriptive statistics.
- Creating and storing ggplot objects.

The aim is readable, understandable code—not a production system or a clinical analysis.