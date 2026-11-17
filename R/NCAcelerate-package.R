#' NCAcelerate: automated NCA reporting for FIH SAD/MAD studies
#'
#' The package bundles a 'shiny' application ([run_app()]) together with the
#' pure, testable functions that power it:
#'
#' * Data import: [read_concentration_data()], [read_pk_parameter_data()]
#' * Concentration data: [convert_concentration()], [summarise_concentration()],
#'   [plot_concentration_time()]
#' * PK parameters: [transform_pk_parameters()], [add_accumulation_ratio()],
#'   [summarise_pk_parameters()]
#' * Dose proportionality: [fit_dose_proportionality()],
#'   [assess_dose_proportionality()], [project_pk_parameters()],
#'   [plot_dose_proportionality()]
#'
#' @keywords internal
#' @importFrom rlang .data :=
#' @importFrom dplyr %>%
#' @importFrom knitr kable
"_PACKAGE"

utils::globalVariables(c(
  ".", "ANALYTE", "MATRIX", "PANEL", "PERIOD", "DOSE", "DOSEUNIT", "NMHR",
  "DAY", "CONCENTRATION", "CONCENTRATION_GM", "CONC_UNIT", "NMHR_UNIT",
  "BLQ_EXCLUDED", "GM", "SD", "N", "below_lloq_pct", "mean_concentration", "key",
  "tmax", "interaction_var", "PKEXTSTUDYID", "PKEXTSUBJID", "PKEXTPKSEQ",
  "PKEXTPERIOD", "PKEXTSTDYDY", "PKEXTNMHR", "NMHRU", "PKEXTORRES", "ORRESU",
  "DOSE", "PKEXTDOSE", "PKEXTSRCANL", "PKEXTSPECAB", "Parameter", "Units",
  "Estimate", "Day", "Dose", "Panel", "Period", "max_day", "min_day",
  "min_day_value", "max_day_value", "ar_value", "gm", "CIL", "CIU", "fit",
  "lwr", "upr", "OBMPK", "OBMPKLB", "OBMPKUB", "EMPK", "EMPKLB", "EMPKUB",
  "Observed", "ObservedEM", "Prediction", "PredictedEM"
))
