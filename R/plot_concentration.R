#' Empty plot carrying a message
#' @keywords internal
message_plot <- function(message) {
  ggplot2::ggplot() + ggplot2::theme_bw() + ggplot2::labs(title = message)
}

#' Log10 axis breaks from `10^from` up to the first power of ten above `max_value`
#' @keywords internal
log10_breaks <- function(max_value, from = 0) {
  top <- ceiling(log10(max_value))
  if (!is.finite(top)) top <- from
  10^seq(from, max(from, top), by = 1)
}

#' Time-concentration plot
#'
#' Plots summarised concentrations (see [summarise_concentration()]) against
#' nominal time, one line per panel - period - dose. Supply `days` to draw a
#' MAD plot faceted by study day.
#'
#' @param summary_data Output of [summarise_concentration()].
#' @param stat `"AM"` (arithmetic mean, SD error bars) or `"GM"` (geometric
#'   mean, geometric SD error bars). Time points with `GM = "NC"` are dropped
#'   for GM plots.
#' @param error_bar Draw error bars?
#' @param log_y Use a log10 y-axis?
#' @param x_limits Optional numeric `c(start, end)` for the x-axis.
#' @param x_interval Optional spacing of x-axis breaks (used with `x_limits`).
#' @param pk_target,lloq Optional reference lines (red / black dashed).
#' @param title,x_title,y_title Optional titles. Empty values use defaults.
#' @param days `NULL` for a single-panel (SAD) plot, or the study days to plot
#'   as facets (MAD).
#' @param free_x Use a free x scale across day facets (ignores `x_limits`).
#' @param log_breaks_from Lowest power of ten used for log-axis breaks.
#' @return A `ggplot` object.
#' @export
#' @examples
#' path <- system.file("extdata", "example_concentration.csv",
#'                     package = "NCAcelerate")
#' summary <- summarise_concentration(read_concentration_data(path))
#' plot_concentration_time(summary, stat = "GM", log_y = TRUE, lloq = 1)
plot_concentration_time <- function(summary_data, stat = c("AM", "GM"),
                                    error_bar = FALSE, log_y = FALSE,
                                    x_limits = NULL, x_interval = NULL,
                                    pk_target = NA, lloq = NA, title = NULL,
                                    x_title = NULL, y_title = NULL,
                                    days = NULL, free_x = FALSE,
                                    log_breaks_from = 0) {
  stat <- match.arg(stat)
  sd_col <- if (stat == "AM") "SD" else "GSD"
  pk_target <- parse_number_input(pk_target)
  lloq <- parse_number_input(lloq)
  faceted <- !is.null(days)

  data_source <- dplyr::filter(summary_data, BLQ_EXCLUDED == "N")
  if (stat == "GM") {
    data_source <- data_source %>%
      dplyr::filter(GM != "NC") %>%
      dplyr::mutate(GM = as.numeric(GM))
  }
  if (faceted) {
    if (length(days) == 0) return(message_plot("Select day(s) to plot"))
    data_source <- dplyr::filter(data_source, DAY %in% days)
  }
  if (nrow(data_source) == 0) {
    return(message_plot("No data points in the selected X-axis range"))
  }

  conc_unit <- paste(unique(data_source$CONC_UNIT), collapse = ", ")
  time_unit <- paste(unique(data_source$NMHR_UNIT), collapse = ", ")
  dose_unit <- paste(unique(data_source$DOSEUNIT), collapse = ", ")
  analyte <- paste(unique(data_source$ANALYTE), collapse = ", ")
  max_y <- max(data_source[[stat]] + data_source$SD, na.rm = TRUE)

  data_source <- data_source %>%
    dplyr::arrange(PANEL, PERIOD, DOSE) %>%
    dplyr::mutate(interaction_var = paste(PANEL, PERIOD, DOSE, sep = " - "),
                  interaction_var = factor(interaction_var,
                                           levels = unique(interaction_var)))

  caption <- c(
    if (!is.na(pk_target)) paste0("PK Target (red dashed line): ", pk_target, " ", conc_unit),
    if (!is.na(lloq)) paste0("LLOQ (black dashed line): ", lloq, " ", conc_unit)
  )

  p <- ggplot2::ggplot(data_source, ggplot2::aes(x = NMHR, y = .data[[stat]],
                                                 color = interaction_var)) +
    ggplot2::geom_line(linewidth = 0.6) +
    ggplot2::geom_point(size = 1.5) +
    ggplot2::theme_bw() +
    ggplot2::labs(
      color = paste0("Panel - Period - Dose (", dose_unit, ")"),
      x = if (nzchar(x_title %||% "")) x_title else paste0("Time (", time_unit, ")"),
      y = if (nzchar(y_title %||% "")) y_title else paste0(analyte, " Concentration (", conc_unit, ")"),
      title = if (nzchar(title %||% "")) title else NULL,
      caption = if (length(caption) > 0) paste(caption, collapse = "\n") else NULL
    ) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                   plot.caption = ggplot2::element_text(hjust = 0))

  if (!is.na(pk_target)) {
    p <- p + ggplot2::geom_hline(yintercept = pk_target, linetype = "dashed",
                                 linewidth = 0.6, color = "red")
  }
  if (!is.na(lloq)) {
    p <- p + ggplot2::geom_hline(yintercept = lloq, linetype = "dashed",
                                 linewidth = 0.6, color = "black")
  }

  if (faceted) {
    p <- p + ggplot2::facet_wrap(
      ~DAY, nrow = 1, scales = if (isTRUE(free_x)) "free_x" else "fixed",
      labeller = ggplot2::labeller(DAY = function(x) paste("Day", x))
    )
  }

  x_limits <- suppressWarnings(as.numeric(x_limits))
  x_interval <- suppressWarnings(as.numeric(x_interval))
  if (!(faceted && isTRUE(free_x)) && length(x_limits) == 2 &&
      all(is.finite(x_limits)) && x_limits[2] > x_limits[1]) {
    p <- p + ggplot2::coord_cartesian(xlim = x_limits)
    if (length(x_interval) == 1 && is.finite(x_interval) && x_interval > 0) {
      p <- p + ggplot2::scale_x_continuous(
        breaks = seq(x_limits[1], x_limits[2], by = x_interval)
      )
    }
  }

  if (isTRUE(log_y)) {
    breaks <- log10_breaks(max_y, from = log_breaks_from)
    p <- p +
      ggplot2::scale_y_log10(breaks = breaks, labels = scales::label_comma()) +
      ggplot2::expand_limits(y = breaks) +
      ggplot2::annotation_logticks(sides = "lr", linewidth = 0.3) +
      ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
  }

  if (isTRUE(error_bar)) {
    if (stat == "AM") {
      p <- p + ggplot2::geom_errorbar(
        ggplot2::aes(ymin = .data[[stat]] - .data[[sd_col]],
                     ymax = .data[[stat]] + .data[[sd_col]]), width = 0.6)
    } else {
      p <- p + ggplot2::geom_errorbar(
        ggplot2::aes(ymin = .data[[stat]] / .data[[sd_col]],
                     ymax = .data[[stat]] * .data[[sd_col]]), width = 0.6)
    }
  }

  p
}

`%||%` <- function(x, y) if (is.null(x)) y else x
