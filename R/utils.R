#' Summary statistics used in PK reporting
#'
#' Geometric statistics and the 95% confidence interval of the geometric mean.
#'
#' @param x A numeric vector.
#' @return A single numeric value.
#' @name pk_stats
#' @examples
#' x <- c(10, 12, 15, 9)
#' geo_mean(x)
#' geo_cv(x)
#' geo_sd(x)
#' geo_ci_lower(x)
#' geo_ci_upper(x)
NULL

#' @rdname pk_stats
#' @export
geo_mean <- function(x) exp(mean(log(x), na.rm = TRUE))

#' @rdname pk_stats
#' @export
geo_cv <- function(x) 100 * sqrt(exp(stats::sd(log(x), na.rm = TRUE)^2) - 1)

#' @rdname pk_stats
#' @export
geo_sd <- function(x) exp(stats::sd(log(x), na.rm = TRUE))

#' @rdname pk_stats
#' @export
geo_ci_lower <- function(x) {
  exp(mean(log(x[x > 0])) -
        stats::qt(0.975, length(x) - 1) * stats::sd(log(x)) / sqrt(length(x)))
}

#' @rdname pk_stats
#' @export
geo_ci_upper <- function(x) {
  exp(mean(log(x[x > 0])) +
        stats::qt(0.975, length(x) - 1) * stats::sd(log(x)) / sqrt(length(x)))
}

#' Timestamp used in download file names
#'
#' @param time A `POSIXct` time. Defaults to now.
#' @return A string such as `"202607011430"`.
#' @keywords internal
timestamp_digits <- function(time = Sys.time()) {
  gsub("[^0-9]", "", format(time, "%Y-%m-%d_%H-%M"))
}

#' Parse a free-text numeric input
#'
#' Returns `NA_real_` for `NULL`, empty or non-numeric strings instead of
#' emitting a coercion warning.
#'
#' @param x A length-one character or numeric value.
#' @return A single numeric value or `NA_real_`.
#' @keywords internal
parse_number_input <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x[1]) || !nzchar(trimws(x[1]))) {
    return(NA_real_)
  }
  suppressWarnings(as.numeric(x[1]))
}

#' Parse a comma separated list of numbers (e.g. "200, 400, 800")
#' @keywords internal
parse_number_list <- function(x) {
  if (is.null(x) || !nzchar(trimws(x))) return(numeric(0))
  values <- suppressWarnings(as.numeric(trimws(strsplit(x, ",")[[1]])))
  values[!is.na(values)]
}

#' Fill missing period values with 1
#'
#' Ardena MAD exports leave `PKEXTPERIOD` empty and Watson exports may not
#' contain the column at all; both are treated as period 1.
#'
#' @param x A vector of periods (character or numeric).
#' @return A numeric vector when all values are numeric, otherwise character.
#' @keywords internal
fill_period <- function(x) {
  x <- trimws(as.character(x))
  x[is.na(x) | x == ""] <- "1"
  numeric_x <- suppressWarnings(as.numeric(x))
  if (all(!is.na(numeric_x))) numeric_x else x
}

#' Add a unit to a label when one is provided
#' @keywords internal
label_with_unit <- function(label, unit) {
  if (is.null(unit) || !nzchar(unit)) label else paste0(label, " (", unit, ")")
}

#' Plot size (in inches) of a rendered plot output
#'
#' Uses the client data Shiny records for each `plotOutput()` so that
#' downloads match what is shown on screen. Falls back to 8 x 6 inches when
#' the plot has not been rendered (e.g. hidden tab).
#'
#' @param session A Shiny (module) session.
#' @param output_id The (un-namespaced) output id.
#' @param dpi Screen resolution used when rendering.
#' @keywords internal
plot_size_inches <- function(session, output_id, dpi = 96) {
  full_id <- session$ns(output_id)
  width <- session$clientData[[paste0("output_", full_id, "_width")]]
  height <- session$clientData[[paste0("output_", full_id, "_height")]]
  list(
    width = if (!is.null(width) && width > 0) width / dpi else 8,
    height = if (!is.null(height) && height > 0) height / dpi else 6
  )
}

#' Render a data frame to a Word document
#'
#' @param data A data frame to render as a table.
#' @param file Output path of the `.docx` file.
#' @param title Document title.
#' @param footnotes Character vector of footnotes printed below the table.
#' @return The output path, invisibly.
#' @export
render_table_docx <- function(data, file, title = "Table", footnotes = NULL) {
  template <- system.file("reports", "table_report.Rmd", package = "NCAcelerate",
                          mustWork = TRUE)
  work_dir <- tempfile("ncacelerate_report_")
  dir.create(work_dir)
  on.exit(unlink(work_dir, recursive = TRUE), add = TRUE)
  report <- file.path(work_dir, "table_report.Rmd")
  file.copy(template, report)

  out <- rmarkdown::render(
    report,
    output_format = rmarkdown::word_document(),
    output_dir = work_dir,
    params = list(data = data, title = title, footnotes = footnotes),
    envir = new.env(parent = globalenv()),
    quiet = TRUE
  )
  file.copy(out, file, overwrite = TRUE)
  invisible(file)
}
