#' Columns required in time-concentration data
#' @keywords internal
required_concentration_columns <- c(
  "PKEXTSTUDYID", "PKEXTSUBJID", "PKEXTSRCANL", "PKEXTSPECAB", "PKEXTPKSEQ",
  "PKEXTSTDYDY", "PKEXTNMHR", "PKEXTORRES", "PKEXTDOSE"
)

#' Read the first two rows (column names + units row) and the data body
#' @keywords internal
read_two_header_file <- function(path, file_name = path) {
  ext <- tolower(tools::file_ext(file_name))
  if (ext == "csv") {
    top <- utils::read.csv(path, header = FALSE, nrows = 2,
                           colClasses = "character", na.strings = "")
    body <- utils::read.csv(path, skip = 2, header = FALSE)
  } else if (ext %in% c("xlsx", "xls")) {
    top <- as.data.frame(readxl::read_excel(path, sheet = 1, n_max = 2,
                                            col_names = FALSE,
                                            col_types = "text",
                                            .name_repair = "minimal"))
    body <- as.data.frame(readxl::read_excel(path, sheet = 1, skip = 2,
                                             col_names = FALSE,
                                             .name_repair = "minimal"))
  } else {
    stop("Unsupported file format. Please upload a CSV or Excel file.",
         call. = FALSE)
  }
  headers <- trimws(as.character(unlist(top[1, ], use.names = FALSE)))
  units <- as.character(unlist(top[2, ], use.names = FALSE))
  names(units) <- headers
  list(headers = headers, units = units, body = body)
}

#' Read Phoenix time-concentration export
#'
#' Reads the "Step 1a: Prep Sample Data_EXPORT for RShiny CvT Plot" output of
#' the Phoenix NCA workflow. Both Watson-styled data (dose value in
#' `PKEXTDOSE` and the dose unit in the second header row) and Ardena-styled
#' data (column `PKEXTNAM` present, `PKEXTDOSE` such as `"50 mg"`) are
#' supported. The second row of the file holds the units of `PKEXTNMHR` and
#' `PKEXTORRES`.
#'
#' @param path Path to a `.csv`, `.xlsx` or `.xls` file.
#' @param file_name Original file name, used to detect the file type (useful
#'   for Shiny uploads where `path` has no extension).
#' @return A data frame with the standardised columns `PKEXTSTUDYID`,
#'   `PKEXTSUBJID`, `PKEXTPKSEQ`, `PKEXTPERIOD`, `PKEXTSTDYDY`, `PKEXTNMHR`,
#'   `NMHRU`, `PKEXTORRES`, `ORRESU`, `DOSE`, `DOSEUNIT` followed by the
#'   remaining input columns.
#' @export
#' @examples
#' path <- system.file("extdata", "example_concentration.csv",
#'                     package = "NCAcelerate")
#' head(read_concentration_data(path))
read_concentration_data <- function(path, file_name = path) {
  raw <- read_two_header_file(path, file_name)
  headers <- raw$headers
  df <- raw$body

  missing_cols <- setdiff(required_concentration_columns, headers)
  if (length(missing_cols) > 0) {
    stop("Missing required column(s) in concentration data: ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  }
  if (ncol(df) != length(headers)) {
    stop("The number of data columns does not match the header row.",
         call. = FALSE)
  }

  colnames(df) <- headers
  df$ORRESU <- rep(raw$units[["PKEXTORRES"]], nrow(df))
  df$NMHRU <- rep(raw$units[["PKEXTNMHR"]], nrow(df))

  is_ardena <- "PKEXTNAM" %in% headers
  if (!is_ardena) {
    df$DOSEUNIT <- rep(raw$units[["PKEXTDOSE"]], nrow(df))
    df <- dplyr::rename(df, DOSE = PKEXTDOSE)
  } else {
    # Accommodates both "10 mg" and "10mg" (and decimals such as "0.5 mg")
    df <- df %>%
      dplyr::mutate(
        DOSE = as.numeric(gsub("[^0-9.]", "", PKEXTDOSE)),
        DOSEUNIT = trimws(gsub("[0-9. ]", "", PKEXTDOSE))
      ) %>%
      dplyr::select(-PKEXTDOSE)
  }

  if (!"PKEXTPERIOD" %in% names(df)) df$PKEXTPERIOD <- NA

  df %>%
    dplyr::mutate(
      # Reads both "Day1" and "1" formats
      PKEXTSTDYDY = suppressWarnings(as.numeric(gsub("[^0-9]", "", as.character(PKEXTSTDYDY)))),
      PKEXTPERIOD = fill_period(PKEXTPERIOD)
    ) %>%
    dplyr::select(PKEXTSTUDYID, PKEXTSUBJID, PKEXTPKSEQ, PKEXTPERIOD,
                  PKEXTSTDYDY, PKEXTNMHR, NMHRU, PKEXTORRES, ORRESU, DOSE,
                  DOSEUNIT, dplyr::everything()) %>%
    dplyr::arrange(PKEXTSTUDYID, PKEXTPKSEQ, PKEXTPERIOD, PKEXTSTDYDY,
                   PKEXTNMHR, DOSE, PKEXTSUBJID) %>%
    dplyr::mutate(PKEXTORRES = suppressWarnings(as.numeric(as.character(PKEXTORRES)))) %>%
    as.data.frame()
}

#' Supported concentration units and their scale factors
#'
#' Mass units are relative to g/mL and molar units to M.
#' @export
concentration_units <- c(
  "pg/mL" = 1e-12, "ng/mL" = 1e-9, "ug/mL" = 1e-6, "mg/mL" = 1e-3,
  "pM" = 1e-12, "nM" = 1e-9, "uM" = 1e-6, "mM" = 1e-3
)

#' Convert concentrations between mass and molar units
#'
#' Conversion between mass (`xg/mL`) and molar (`xM`) units uses the
#' molecular weight in kDa. Conversion within the same dimension (e.g. nM to
#' uM) does not need a molecular weight.
#'
#' @param conc Numeric vector of concentrations.
#' @param from_unit,to_unit One of `names(concentration_units)`.
#' @param mw Molecular weight in kDa. Required when converting between mass
#'   and molar units; `NA`/`NULL` is treated as 1.
#' @return Numeric vector of converted concentrations.
#' @export
#' @examples
#' convert_concentration(100, "ng/mL", "nM", mw = 150)
#' convert_concentration(1, "nM", "pM")
convert_concentration <- function(conc, from_unit, to_unit, mw = NA) {
  units <- names(concentration_units)
  if (!from_unit %in% units || !to_unit %in% units) {
    stop("Unsupported unit. Choose from: ", paste(units, collapse = ", "),
         call. = FALSE)
  }
  mw <- parse_number_input(mw)
  if (is.na(mw)) mw <- 1

  factor <- concentration_units[[from_unit]] / concentration_units[[to_unit]]
  from_mass <- grepl("g/mL$", from_unit)
  to_mass <- grepl("g/mL$", to_unit)

  conc <- as.numeric(conc) * factor
  if (from_mass && !to_mass) {
    conc / mw
  } else if (!from_mass && to_mass) {
    conc * mw
  } else {
    conc
  }
}

#' Build the group key used to exclude concentration groups
#'
#' @param data Concentration data as returned by [read_concentration_data()].
#' @return Character vector `analyte_matrix_panel_period_dose`.
#' @export
concentration_group_key <- function(data) {
  paste(data$PKEXTSRCANL, data$PKEXTSPECAB, data$PKEXTPKSEQ,
        data$PKEXTPERIOD, data$DOSE, sep = "_")
}

#' Filter concentration data
#'
#' @param data Concentration data as returned by [read_concentration_data()].
#' @param analyte,matrix,panel,period,dose Values to keep. `NULL` keeps all.
#' @param exclude_groups Group keys (see [concentration_group_key()]) to drop.
#' @return The filtered data frame.
#' @export
filter_concentration_data <- function(data, analyte = NULL, matrix = NULL,
                                      panel = NULL, period = NULL,
                                      dose = NULL, exclude_groups = NULL) {
  keep <- function(column, values) {
    if (is.null(values)) rep(TRUE, nrow(data)) else data[[column]] %in% values
  }
  rows <- keep("PKEXTSRCANL", analyte) & keep("PKEXTSPECAB", matrix) &
    keep("PKEXTPKSEQ", panel) & keep("PKEXTPERIOD", period) &
    keep("DOSE", dose)
  if (length(exclude_groups) > 0) {
    rows <- rows & !concentration_group_key(data) %in% exclude_groups
  }
  data[rows, , drop = FALSE]
}
