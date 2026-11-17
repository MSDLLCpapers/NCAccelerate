#' Dose proportionality (power model)
#'
#' Functions implementing the power model `log(PK) = a + b * log(Dose)` used
#' in the Dose Proportionality tab.
#'
#' * `prepare_dose_proportionality()` drops zero/missing values and excluded
#'   subjects, and adds `LNY = log(parameter)` and `LNX = log(Dose)`.
#' * `summarise_dose_groups()` returns the geometric mean and 95% CI per dose.
#' * `fit_dose_proportionality()` fits the linear model on the log scale.
#' * `predict_dose_proportionality()` returns predictions (with 95% CI) on the
#'   original scale.
#' * `assess_dose_proportionality()` interprets the 95% CI of the slope.
#'
#' @param data Output of [transform_pk_parameters()].
#' @param parameter Name of the PK parameter column.
#' @param exclude_subjects Subject IDs (`PKEXTSUBJID`) to exclude.
#' @name dose_proportionality
#' @examples
#' path <- system.file("extdata", "example_pk_parameters.csv",
#'                     package = "NCAcelerate")
#' pk <- transform_pk_parameters(read_pk_parameter_data(path))
#' dp <- prepare_dose_proportionality(pk, "Cmax")
#' model <- fit_dose_proportionality(dp)
#' assess_dose_proportionality(model)
#' summarise_dose_groups(dp, "Cmax")
#' project_pk_parameters(model, summarise_dose_groups(dp, "Cmax"),
#'                       doses = c(200, 400), noael = 5000, parameter = "Cmax")
NULL

#' @rdname dose_proportionality
#' @export
prepare_dose_proportionality <- function(data, parameter, exclude_subjects = NULL) {
  if (!parameter %in% names(data)) {
    stop("Parameter '", parameter, "' not found.", call. = FALSE)
  }
  data <- data %>%
    dplyr::filter(.data[[parameter]] != 0,
                  !is.na(.data[[parameter]]),
                  !PKEXTSUBJID %in% exclude_subjects)
  data$LNY <- log(data[[parameter]])
  data$LNX <- log(data[["Dose"]])
  data
}

#' @rdname dose_proportionality
#' @param dp_data Output of `prepare_dose_proportionality()`.
#' @export
summarise_dose_groups <- function(dp_data, parameter) {
  out <- dp_data %>%
    dplyr::group_by(Dose) %>%
    dplyr::summarise(
      gm = geo_mean(.data[[parameter]]),
      CIL = geo_ci_lower(.data[[parameter]]),
      CIU = geo_ci_upper(.data[[parameter]])
    )
  signif(as.data.frame(out), 3)
}

#' @rdname dose_proportionality
#' @export
fit_dose_proportionality <- function(dp_data) {
  stats::lm(LNY ~ LNX, na.action = "na.exclude", data = dp_data)
}

#' @rdname dose_proportionality
#' @param model Output of `fit_dose_proportionality()`.
#' @param doses Numeric vector of doses to predict at.
#' @export
predict_dose_proportionality <- function(model, doses) {
  pred <- as.data.frame(exp(stats::predict(
    model, newdata = data.frame(LNX = log(doses)), interval = "confidence"
  )))
  pred$Dose <- doses
  pred
}

#' @rdname dose_proportionality
#' @param level Confidence level for the slope.
#' @return `assess_dose_proportionality()`: a sentence describing the slope,
#'   its CI, r-squared and the conclusion.
#' @export
assess_dose_proportionality <- function(model, level = 0.95) {
  ci <- signif(stats::confint(model, "LNX", level = level), 3)
  est <- signif(stats::coef(model)[["LNX"]], 3)
  r2 <- signif(summary(model)$r.squared, 2)

  conclusion <- if (anyNA(ci)) {
    "Could not assess dose proportionality."
  } else if (ci[1] < 1 && ci[2] < 1) {
    "assume parameter is less than dose proportional."
  } else if (ci[1] > 1 && ci[2] > 1) {
    "assume parameter is more than dose proportional."
  } else {
    "assume parameter is dose proportional."
  }
  paste0("Parameter slope is ", est, " with 95% CI [", ci[1], ", ", ci[2],
         "] and r-squared of ", r2, "; ", conclusion)
}

#' Parameter projections and exposure margins
#'
#' Combines the observed geometric means (with 95% CI) and model predictions
#' at the requested doses, and computes exposure margins as
#' `NOAEL exposure / parameter`.
#'
#' @inheritParams dose_proportionality
#' @param dose_summary Output of [summarise_dose_groups()].
#' @param noael PK parameter value at the NOAEL (exposure margin numerator).
#' @param dose_unit,parameter_unit Optional units for column headers.
#' @return A data frame with columns Parameter, Dose, Observed, Observed EM,
#'   Prediction, Prediction EM.
#' @export
project_pk_parameters <- function(model, dose_summary, doses, noael = NA,
                                  parameter, dose_unit = "",
                                  parameter_unit = "") {
  noael <- parse_number_input(noael)
  pred <- signif(predict_dose_proportionality(model, doses), 3)

  out <- dplyr::full_join(dose_summary, pred, by = "Dose") %>%
    dplyr::arrange(Dose) %>%
    dplyr::mutate(
      OBMPK = signif(noael / gm, 3),
      OBMPKLB = signif(noael / CIL, 3),
      OBMPKUB = signif(noael / CIU, 3),
      EMPK = signif(noael / fit, 3),
      EMPKLB = signif(noael / lwr, 3),
      EMPKUB = signif(noael / upr, 3),
      Observed = paste0(gm, " (", CIL, ", ", CIU, ")"),
      ObservedEM = paste0(OBMPK, " (", OBMPKLB, ", ", OBMPKUB, ")"),
      Prediction = paste0(fit, " (", lwr, ", ", upr, ")"),
      PredictedEM = paste0(EMPK, " (", EMPKLB, ", ", EMPKUB, ")"),
      Parameter = parameter
    ) %>%
    dplyr::select(Parameter, Dose, Observed, ObservedEM, Prediction, PredictedEM) %>%
    as.data.frame()

  names(out) <- c("Parameter",
                  label_with_unit("Dose", dose_unit),
                  label_with_unit("Observed", parameter_unit),
                  "Observed EM",
                  label_with_unit("Prediction", parameter_unit),
                  "Prediction EM")
  out
}

#' Dose proportionality plot
#'
#' Observed geometric means with 95% CI error bars, the fitted power model
#' and (optionally) its 95% confidence band.
#'
#' @inheritParams dose_proportionality
#' @param dp_data Output of [prepare_dose_proportionality()].
#' @param x_limits Optional `c(lower, upper)` dose range; `NA` for automatic.
#' @param log_x,log_y Use log10 axes?
#' @param error_bar_width Error bar width; `NA` for automatic (range / 60).
#' @param show_interval Draw the confidence band of the fit?
#' @param dose_unit,parameter_unit Optional axis units.
#' @return A `ggplot` object.
#' @export
plot_dose_proportionality <- function(dp_data, parameter, x_limits = c(NA, NA),
                                      log_x = FALSE, log_y = FALSE,
                                      error_bar_width = NA,
                                      show_interval = TRUE, dose_unit = "",
                                      parameter_unit = "") {
  dose_summary <- summarise_dose_groups(dp_data, parameter)
  model <- fit_dose_proportionality(dp_data)
  pred_line <- predict_dose_proportionality(model, dp_data$Dose)

  x_limits <- suppressWarnings(as.numeric(x_limits))
  if (length(x_limits) != 2) x_limits <- c(NA, NA)
  if (!is.na(x_limits[1])) {
    dose_summary <- dplyr::filter(dose_summary, Dose >= x_limits[1])
    pred_line <- dplyr::filter(pred_line, Dose >= x_limits[1])
  }
  if (!is.na(x_limits[2])) {
    dose_summary <- dplyr::filter(dose_summary, Dose <= x_limits[2])
    pred_line <- dplyr::filter(pred_line, Dose <= x_limits[2])
  }
  if (nrow(dose_summary) == 0) return(message_plot("No doses in the selected range"))

  error_bar_width <- parse_number_input(error_bar_width)
  if (is.na(error_bar_width)) {
    error_bar_width <- as.integer(diff(range(dose_summary$Dose)) / 60)
  }

  p <- ggplot2::ggplot(dose_summary) +
    ggplot2::geom_point(ggplot2::aes(x = Dose, y = gm)) +
    ggplot2::geom_errorbar(ggplot2::aes(x = Dose, ymin = CIL, ymax = CIU),
                           width = error_bar_width) +
    ggplot2::geom_line(data = pred_line, ggplot2::aes(x = Dose, y = fit)) +
    ggplot2::labs(x = label_with_unit("Dose", dose_unit),
                  y = label_with_unit(parameter, parameter_unit)) +
    ggplot2::theme_bw() +
    ggplot2::theme(text = ggplot2::element_text(size = 18))

  if (!log_x) p <- p + ggplot2::xlim(x_limits[1], x_limits[2])

  if (isTRUE(show_interval)) {
    p <- p + ggplot2::geom_ribbon(data = pred_line,
                                  ggplot2::aes(x = Dose, ymin = lwr, ymax = upr),
                                  alpha = 0.2)
  }

  if (isTRUE(log_y)) {
    p <- p +
      ggplot2::scale_y_log10(breaks = log10_breaks(max(dose_summary$gm), from = -2),
                             labels = scales::label_comma()) +
      ggplot2::annotation_logticks(sides = "l", linewidth = 0.3) +
      ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
  }
  if (isTRUE(log_x)) {
    p <- p +
      ggplot2::scale_x_log10(breaks = log10_breaks(max(dose_summary$Dose), from = -2),
                             labels = scales::label_comma()) +
      ggplot2::annotation_logticks(sides = "b", linewidth = 0.3) +
      ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
  }
  p
}
