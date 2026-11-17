# NCAcelerate

NCAcelerate is an R package and Shiny app that automates reporting for
first-in-human (FIH) **single ascending dose (SAD)** and **multiple ascending
dose (MAD)** pharmacokinetic (PK) analyses that come out of a Phoenix NCA
workflow. It produces:

- **Time-concentration plots** for SAD, and for MAD faceted by study day.
- A **summary PK parameter table** showing geometric mean (CV%),
  median (min-max) for time variables, and accumulation ratios. You can
  export it to Word.
- A **dose proportionality** assessment using the power model, with
  parameter projections and exposure margins.

The logic lives in plain R functions that you can call from scripts and test
on their own. The Shiny UI is split into one module per tab.

## Installation

```r
# From the package folder
install.packages("devtools")
devtools::install("path/to/NCAcelerate")

# or, during development
devtools::load_all("path/to/NCAcelerate")
```

You need R 4.1 or later. Word export (`.docx`) needs
[pandoc](https://pandoc.org/), which comes with RStudio.

## Run the app

```r
NCAcelerate::run_app()
```

To try it, upload the simulated example files from
`system.file("extdata", package = "NCAcelerate")`:

| File | Content |
|------|---------|
| `example_concentration.csv` | SAD time-concentration data, Watson-styled |
| `example_pk_parameters.csv` | SAD PK parameters (stacked), Watson-styled |
| `example_concentration_mad.csv` | MAD time-concentration data (Day 1 and Day 7), Ardena-styled |
| `example_pk_parameters_mad.csv` | MAD PK parameters (stacked), Ardena-styled |

These files are simulated with a one-compartment model; see
`data-raw/make_example_data.R`.

## Use the functions without the app

```r
library(NCAcelerate)

# Time-concentration data
conc <- read_concentration_data(
  system.file("extdata", "example_concentration.csv", package = "NCAcelerate")
)
conc$PKEXTORRES <- convert_concentration(conc$PKEXTORRES, "ng/mL", "nM", mw = 150)
summary <- summarise_concentration(conc, lloq = 1, apply_blq = TRUE, impute_gm = TRUE)
plot_concentration_time(summary, stat = "GM", log_y = TRUE, error_bar = TRUE, lloq = 1)

# PK parameter table
raw  <- read_pk_parameter_data(
  system.file("extdata", "example_pk_parameters_mad.csv", package = "NCAcelerate")
)
wide <- add_accumulation_ratio(transform_pk_parameters(raw), "Cmax")
summarise_pk_parameters(wide,
                        parameters    = c("Cmax", "Tmax", "AUClast"),
                        strata        = c("Dose", "Day"),
                        time_vars     = "Tmax",
                        ar_parameters = "Cmax",
                        labels        = pk_parameter_labels(raw))

# Dose proportionality (SAD)
sad_pk <- transform_pk_parameters(read_pk_parameter_data(
  system.file("extdata", "example_pk_parameters.csv", package = "NCAcelerate")
))
dp    <- prepare_dose_proportionality(sad_pk, "AUClast")
model <- fit_dose_proportionality(dp)
assess_dose_proportionality(model)
plot_dose_proportionality(dp, "AUClast", dose_unit = "mg")
project_pk_parameters(model, summarise_dose_groups(dp, "AUClast"),
                      doses = c(400, 800), noael = 50000, parameter = "AUClast")
```

## Input data

Use the outputs of these Phoenix NCA workflow steps:

- **Step 1a: Prep Sample Data_EXPORT for RShiny CvT Plot** (time-concentration data)
- **Step 4a/c: EXPORT Results for RShiny PK Parameter Table** (stacked PK parameters)

The app accepts `.csv`, `.xlsx`, and `.xls` files. Excel files must have a
single sheet.

### Time-concentration data

| Column | Meaning | Watson | Ardena |
|--------|---------|:------:|:------:|
| `PKEXTNAM` | value `"ARDENA"` |  | ✓ |
| `PKEXTSTUDYID` | study ID | ✓ | ✓ |
| `PKEXTSUBJID` | subject ID | ✓ | ✓ |
| `PKEXTSRCANL` | analyte | ✓ | ✓ |
| `PKEXTSPECAB` | matrix | ✓ | ✓ |
| `PKEXTPKSEQ` | panel | ✓ | ✓ |
| `PKEXTPERIOD` | period (empty or missing → 1) | ✓ | ✓ |
| `PKEXTSTDYDY` | day (`1` or `Day1`) | ✓ | ✓ |
| `PKEXTNMHR` | nominal hour | ✓ | ✓ |
| `PKEXTORRES` | concentration | ✓ | ✓ |
| `PKEXTDOSE` | dose | value (e.g. `50`) | value + unit (e.g. `50 mg`) |

The **second row** of the file holds units: `PKEXTNMHR` (e.g. `h`) and
`PKEXTORRES` (e.g. `ng/mL`). For Watson data it also holds the `PKEXTDOSE`
unit (e.g. `mg`).

### PK parameter data (stacked)

Required columns: `PKEXTSUBJID`, `PKEXTSRCANL`, `PKEXTSPECAB`, `PKEXTDOSE`,
`PKEXTPKSEQ`, `PKEXTSTDYDY`, `Parameter`, `Units`, and `Estimate`.
`PKEXTPERIOD` is optional. In Watson exports, the row right after the header
is a units row with an empty `Parameter` and the dose unit in `PKEXTDOSE`.

## Methods

- **BLQ rules** (Mean Concentration Data tab): a time point after Tmax is
  excluded when more than 50% of subjects are below LLOQ, or when the mean is
  below LLOQ. Pre-dose is never excluded.
- **GM imputation**: you can replace post-dose zeros with LLOQ/2 for geometric
  statistics only. Without imputation, `GM` shows `NC`.
- **PK table**: values are GM (CV%) unless noted. Time variables you select
  show median (min-max). The table shows `NC` when any value is 0 and `-` when
  any value is missing.
- **Accumulation ratio**: the last-day value divided by the first-day value,
  within subject, dose, panel, and period.
- **Dose proportionality**: fits the power model
  `log(PK) = a + b·log(Dose)`. The parameter is called dose proportional when
  the 95% CI of `b` contains 1, and less or more than proportional when the
  CI is entirely below or above 1. The exposure margin is the NOAEL exposure
  divided by the observed or predicted value.

## Package structure

```
NCAcelerate/
├── DESCRIPTION, NAMESPACE, LICENSE.md
├── app.R                         # deployment entry point (shinyapps.io / Connect)
├── R/
│   ├── data_concentration.R      # read_concentration_data(), convert_concentration(), filters
│   ├── summarise_concentration.R # summarise_concentration() - BLQ rules, GM imputation
│   ├── plot_concentration.R      # plot_concentration_time() - SAD and MAD plots
│   ├── pk_parameters.R           # read/transform PK parameters, AR, summary table
│   ├── dose_proportionality.R    # power model, assessment, projections, plot
│   ├── utils.R                   # geometric statistics, input parsing, Word export
│   ├── mod_upload.R              # Shiny module: file uploads
│   ├── mod_filter.R              # Shiny module: cascading data filters
│   ├── mod_data_check.R          # Shiny module: raw data + unit conversion
│   ├── mod_mean_conc.R           # Shiny module: mean concentration table
│   ├── mod_conc_plot.R           # Shiny module: SAD / MAD time-concentration plot
│   ├── mod_pk_table.R            # Shiny module: PK parameter table
│   ├── mod_dose_prop.R           # Shiny module: dose proportionality
│   ├── app_ui.R, app_server.R    # wire modules together
│   └── run_app.R
├── inst/
│   ├── app/www/                  # styles.css, instructions.html
│   ├── extdata/                  # simulated example data
│   └── reports/table_report.Rmd  # Word export template
├── data-raw/make_example_data.R
└── tests/testthat/               # unit tests and shiny::testServer() module tests
```

Data flows through the app as:
`upload → data check (unit conversion) → filter → mean concentration → SAD/MAD plots`
and `upload → PK parameters → PK table / dose proportionality`.

## Development

```r
devtools::document()   # regenerate NAMESPACE and man/ from roxygen comments
devtools::test()       # run the test suite
devtools::check()      # full R CMD check
```

If R can't find pandoc when you run the tests outside RStudio, set
`RSTUDIO_PANDOC` to RStudio's pandoc folder, for example
`C:/Program Files/RStudio/resources/app/bin/quarto/bin/tools`. Otherwise the
Word export tests are skipped.

## Deployment

The `app.R` at the package root loads the package with `pkgload` and calls
`run_app()`. Deploy the package folder as the app directory:

```r
rsconnect::deployApp("path/to/NCAcelerate", appName = "NCAcelerate")
```

## License

GPL (>= 3). See `LICENSE.md`. For third-party notices, see
`inst/LICENSES_THIRD_PARTY.txt`.

## Author

Ruilian (Roxy) Zhang
