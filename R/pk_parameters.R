#' Read Phoenix PK parameter export (stacked)
#'
#' Reads the "Step 4a/c: EXPORT Results for RShiny PK Parameter Table" output.
#' Watson-styled exports carry a units row directly below the header (blank
#' `Parameter`, dose unit in `PKEXTDOSE`); it is removed and the dose unit is
#' appended to `PKEXTDOSE` (e.g. `"50 mg"`) to match Ardena-styled exports.
#'
#' @inheritParams read_concentration_data
#' @return A data frame in stacked (long) format with columns `Parameter`,
#'   `Units`, `Estimate` and the `PKEXT*` identifiers.
#' @export
#' @examples
#' path <- system.file("extdata", "example_pk_parameters.csv",
#'                     package = "NCAcelerate")
#' head(read_pk_parameter_data(path))
read_pk_parameter_data <- function(path, file_name = path) {
  ext <- tolower(tools::file_ext(file_name))
  if (ext == "csv") {
    df <- utils::read.csv(path, check.names = FALSE)
  } else if (ext %in% c("xlsx", "xls")) {
    df <- as.data.frame(readxl::read_excel(path, sheet = 1))
  } else {
    stop("Unsupported file format. Please upload a CSV or Excel file.",
         call. = FALSE)
  }

  required <- c("PKEXTSUBJID", "PKEXTSRCANL", "PKEXTSPECAB", "PKEXTDOSE",
                "PKEXTPKSEQ", "PKEXTSTDYDY", "Parameter", "Units", "Estimate")
  missing_cols <- setdiff(required, names(df))
  if (length(missing_cols) > 0) {
    stop("Missing required column(s) in PK parameter data: ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  if (nrow(df) > 0 && (is.na(df$Parameter[1]) || df$Parameter[1] == "")) {
    dose_unit <- df$PKEXTDOSE[1]
    df <- df[-1, , drop = FALSE]
    df$PKEXTDOSE <- paste0(df$PKEXTDOSE, " ", dose_unit)
    df$Estimate <- suppressWarnings(as.numeric(df$Estimate))
  }
  rownames(df) <- NULL
  df
}

#' Pivot stacked PK parameters to one row per subject
#'
#' @param data Output of [read_pk_parameter_data()].
#' @return A data frame with one column per PK parameter and the identifiers
#'   renamed to `Analyte`, `Matrix`, `Dose` (numeric), `Period`, `Panel`,
#'   `Day` (numeric) and `PKEXTSUBJID`.
#' @export
transform_pk_parameters <- function(data) {
  if (!"PKEXTPERIOD" %in% names(data)) data$PKEXTPERIOD <- NA

  data %>%
    dplyr::filter(Parameter != "Flag_AUC_%Ext_pred") %>%
    dplyr::select(-Units) %>%
    tidyr::pivot_wider(names_from = Parameter, values_from = Estimate) %>%
    dplyr::mutate(PKEXTDOSE = as.numeric(gsub("[^0-9.]", "", PKEXTDOSE)),
                  PKEXTPERIOD = fill_period(PKEXTPERIOD)) %>%
    dplyr::arrange(PKEXTDOSE) %>%
    dplyr::rename(
      Analyte = "PKEXTSRCANL",
      Matrix = "PKEXTSPECAB",
      Dose = "PKEXTDOSE",
      Period = "PKEXTPERIOD",
      Panel = "PKEXTPKSEQ",
      Day = "PKEXTSTDYDY"
    ) %>%
    dplyr::mutate(Day = suppressWarnings(as.numeric(gsub("[^0-9.]", "", Day)))) %>%
    as.data.frame()
}

#' Identifier columns of transformed PK parameter data
#' @export
pk_id_columns <- c("Analyte", "Matrix", "Dose", "Period", "Panel",
                   "PKEXTSUBJID", "Day")

#' PK parameter column names available for summary
#'
#' @param data Output of [transform_pk_parameters()].
#' @return Sorted character vector of parameter names.
#' @export
pk_parameter_names <- function(data) {
  sort(setdiff(colnames(data), pk_id_columns))
}

#' Column labels with units
#'
#' Builds a named vector mapping parameter name to `"name (unit)"`, used to
#' add units to the headers of the summary table. `HL_Lambda_z`,
#' `AUCINF_pred` and `AUClast` are keyed as `t1/2`, `AUC0-inf` and
#' `AUC0-last`.
#'
#' @param data Output of [read_pk_parameter_data()].
#' @return A named character vector.
#' @export
pk_parameter_labels <- function(data) {
  map <- data %>%
    dplyr::select(Parameter, Units) %>%
    dplyr::distinct() %>%
    dplyr::filter(!is.na(Units) & Units != "") %>%
    dplyr::mutate(Parameter = dplyr::case_when(
      Parameter == "HL_Lambda_z" ~ "t1/2",
      Parameter == "AUCINF_pred" ~ "AUC0-inf",
      Parameter == "AUClast" ~ "AUC0-last",
      TRUE ~ Parameter
    ))
  stats::setNames(paste0(map$Parameter, " (", map$Units, ")"), map$Parameter)
}

#' Add accumulation ratios
#'
#' For each parameter, the accumulation ratio is the value on the last study
#' day divided by the value on the first study day within subject, dose,
#' panel and period. It is reported on the last-day row (`NA` elsewhere) in a
#' new column `<parameter>_AR`. Nothing is added when `Day` has a single
#' value.
#'
#' @param data Output of [transform_pk_parameters()].
#' @param parameters Parameters to compute accumulation ratios for.
#' @return `data` with additional `_AR` columns.
#' @export
add_accumulation_ratio <- function(data, parameters) {
  if (length(parameters) == 0 || length(unique(data$Day)) <= 1) return(data)
  by <- c("PKEXTSUBJID", "Dose", "Panel", "Period")

  for (param in parameters) {
    ar_col <- paste0(param, "_AR")
    ar_data <- data %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(by))) %>%
      dplyr::summarise(
        max_day = max(Day),
        min_day = min(Day),
        min_day_value = .data[[param]][Day == min(Day)][1],
        max_day_value = .data[[param]][Day == max(Day)][1],
        .groups = "drop"
      ) %>%
      dplyr::mutate(!!ar_col := ifelse(min_day == max_day, NA_real_,
                                       max_day_value / min_day_value)) %>%
      dplyr::select(dplyr::all_of(c(by, "max_day", ar_col)))

    data <- data %>%
      dplyr::left_join(ar_data, by = by) %>%
      dplyr::mutate(!!ar_col := dplyr::if_else(Day == max_day,
                                               .data[[ar_col]], NA_real_)) %>%
      dplyr::select(-max_day)
  }
  data
}

#' Summary PK parameter table
#'
#' Summarises PK parameters by the chosen strata: geometric mean (CV%) for
#' most parameters and median (min-max) for time variables. Groups with any
#' zero value report `"NC"` and groups with any missing value report `"-"`.
#'
#' @param data Output of [transform_pk_parameters()] (optionally with
#'   [add_accumulation_ratio()] columns).
#' @param parameters Parameter columns to summarise.
#' @param strata Columns to stratify by (from [pk_id_columns]).
#' @param time_vars Parameters reported as median (min-max).
#' @param ar_parameters Parameters whose `_AR` columns should be included.
#' @param labels Optional named vector from [pk_parameter_labels()] used to
#'   add units to column headers.
#' @return A data frame with the strata, `N`, and one column per parameter.
#' @export
#' @examples
#' path <- system.file("extdata", "example_pk_parameters.csv",
#'                     package = "NCAcelerate")
#' raw <- read_pk_parameter_data(path)
#' summarise_pk_parameters(transform_pk_parameters(raw),
#'                         parameters = c("Cmax", "Tmax", "AUClast"),
#'                         strata = c("Panel", "Dose"),
#'                         time_vars = "Tmax",
#'                         labels = pk_parameter_labels(raw))
summarise_pk_parameters <- function(data, parameters, strata, time_vars = NULL,
                                    ar_parameters = NULL, labels = NULL) {
  ar_cols <- intersect(paste0(ar_parameters, "_AR"), names(data))
  selected <- c(parameters, ar_cols)

  missing_cols <- setdiff(c(strata, selected), names(data))
  if (length(missing_cols) > 0) {
    stop("Column(s) not found in PK parameter data: ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  summarise_column <- function(x, column) {
    if (is.numeric(x) && column %in% time_vars) {
      paste0(format(signif(stats::median(x, na.rm = TRUE)), nsmall = 2),
             " (", format(signif(min(x, na.rm = TRUE)), nsmall = 2),
             "-", format(signif(max(x, na.rm = TRUE)), nsmall = 2), ")")
    } else if (is.numeric(x)) {
      if (any(x == 0, na.rm = TRUE)) {
        "NC"
      } else if (any(is.na(x))) {
        "-"
      } else {
        paste0(table1::signif_pad(geo_mean(x), digits = 3), " (",
               table1::signif_pad(geo_cv(x), digits = 3), ")")
      }
    } else {
      paste(unique(x), collapse = ", ")
    }
  }

  result <- data[, c(strata, selected), drop = FALSE] %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(strata))) %>%
    dplyr::summarise(
      dplyr::across(dplyr::all_of(selected),
                    ~ summarise_column(.x, dplyr::cur_column())),
      N = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::select(dplyr::all_of(c(strata, "N", selected))) %>%
    as.data.frame()

  if (!is.null(labels)) {
    to_rename <- names(result) %in% names(labels)
    names(result)[to_rename] <- labels[names(result)[to_rename]]
  }
  result
}

#' Footnotes printed below the summary PK parameter table
#' @export
pk_table_footnotes <- c(
  "Results are geometric mean (coefficient of variation %) unless otherwise stated.",
  "Time variables not including t1/2 are reported in median (min-max) format.",
  "NC (Not Calculable) is reported for parameters where at least one individual had a parameter value of 0, therefore Geometric Mean cannot be calculated.",
  "A dash '-' is reported for parameters where at least one individual had a missing parameter value."
)
