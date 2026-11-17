#' Summarise concentrations by nominal time
#'
#' Renames the Phoenix columns for plotting, optionally applies BLQ rules and
#' imputes zero concentrations for the geometric mean, then summarises by
#' analyte, matrix, panel, period, dose, nominal time and day.
#'
#' **BLQ rules** (`apply_blq = TRUE`): after Tmax of the group mean profile,
#' a time point is flagged (`BLQ_EXCLUDED = "Y"`) when more than 50% of
#' subjects are below LLOQ or the arithmetic mean is below LLOQ. Pre-dose
#' (`NMHR == 0`) is never flagged.
#'
#' **GM imputation** (`impute_gm = TRUE`): post-dose zero concentrations are
#' replaced with LLOQ / 2 for the geometric statistics only. Without
#' imputation, groups containing zeros report `GM = "NC"`.
#'
#' @param data Filtered concentration data from [read_concentration_data()].
#' @param lloq Lower limit of quantification (numeric or text). `NA` disables
#'   BLQ rules and imputation.
#' @param apply_blq Apply BLQ rules?
#' @param impute_gm Impute post-dose zeros with LLOQ / 2 for GM?
#' @return A data frame with one row per group and nominal time containing
#'   `N`, `GM` (character, may be `"NC"`), `GCV`, `GSD`, `AM`, `SD`,
#'   `BLQ_EXCLUDED`, units and grouping columns. Groups with a single subject
#'   are dropped.
#' @export
#' @examples
#' path <- system.file("extdata", "example_concentration.csv",
#'                     package = "NCAcelerate")
#' conc <- read_concentration_data(path)
#' head(summarise_concentration(conc, lloq = 1, apply_blq = TRUE))
summarise_concentration <- function(data, lloq = NA, apply_blq = FALSE,
                                    impute_gm = FALSE) {
  lloq <- parse_number_input(lloq)
  group_vars <- c("ANALYTE", "MATRIX", "PANEL", "PERIOD", "DOSE", "DOSEUNIT",
                  "NMHR", "DAY")

  prepared <- data %>%
    dplyr::rename_with(~ gsub("PKEXT", "", .x)) %>%
    dplyr::rename(
      ANALYTE = "SRCANL",
      MATRIX = "SPECAB",
      PANEL = "PKSEQ",
      CONCENTRATION = "ORRES",
      CONC_UNIT = "ORRESU",
      NMHR_UNIT = "NMHRU",
      DAY = "STDYDY"
    ) %>%
    dplyr::mutate(CONCENTRATION_GM = CONCENTRATION)

  if (isTRUE(apply_blq) && !is.na(lloq)) {
    exclude_keys <- prepared %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) %>%
      dplyr::summarise(
        below_lloq_pct = mean(CONCENTRATION < lloq, na.rm = TRUE) * 100,
        mean_concentration = mean(CONCENTRATION, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::filter(below_lloq_pct > 50 | mean_concentration < lloq) %>%
      dplyr::filter(NMHR != 0) %>%
      dplyr::mutate(key = paste(ANALYTE, MATRIX, PANEL, PERIOD, DOSE,
                                DOSEUNIT, NMHR, DAY, sep = "_")) %>%
      dplyr::pull(key)

    tmax_tbl <- prepared %>%
      dplyr::group_by(ANALYTE, MATRIX, PANEL, PERIOD, DOSE, DOSEUNIT, DAY) %>%
      dplyr::summarise(tmax = NMHR[which.max(CONCENTRATION)], .groups = "drop")

    # BLQ exclusion only applies to time points after Tmax
    prepared <- prepared %>%
      dplyr::left_join(tmax_tbl, by = c("ANALYTE", "MATRIX", "PANEL", "PERIOD",
                                        "DOSE", "DOSEUNIT", "DAY")) %>%
      dplyr::mutate(
        key = paste(ANALYTE, MATRIX, PANEL, PERIOD, DOSE, DOSEUNIT, NMHR, DAY,
                    sep = "_"),
        BLQ_EXCLUDED = dplyr::if_else(key %in% exclude_keys & NMHR > tmax,
                                      "Y", "N", missing = "N")
      ) %>%
      dplyr::select(-key, -tmax)
  } else {
    prepared$BLQ_EXCLUDED <- rep("N", nrow(prepared))
  }

  if (isTRUE(impute_gm) && !is.na(lloq)) {
    prepared <- prepared %>%
      dplyr::mutate(CONCENTRATION_GM = dplyr::if_else(
        NMHR != 0 & CONCENTRATION_GM == 0, lloq / 2, CONCENTRATION_GM
      ))
  }

  prepared %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) %>%
    dplyr::summarise(
      BLQ_EXCLUDED = dplyr::first(BLQ_EXCLUDED),
      GM = geo_mean(CONCENTRATION_GM),
      GCV = geo_cv(CONCENTRATION_GM),
      GSD = geo_sd(CONCENTRATION_GM),
      AM = mean(CONCENTRATION),
      SD = stats::sd(CONCENTRATION),
      CONC_UNIT = dplyr::first(CONC_UNIT),
      NMHR_UNIT = dplyr::first(NMHR_UNIT),
      N = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::filter(!is.na(SD)) %>%
    dplyr::mutate(GM = ifelse(NMHR != 0 & GM == 0, "NC", GM)) %>%
    dplyr::select(ANALYTE, MATRIX, PANEL, PERIOD, DAY, NMHR, DOSE, DOSEUNIT,
                  BLQ_EXCLUDED, N, dplyr::everything()) %>%
    dplyr::arrange(DOSE, PERIOD, DAY, NMHR) %>%
    unique() %>%
    as.data.frame()
}

#' Round the numeric columns of a data frame for display
#' @keywords internal
round_numeric_columns <- function(data, digits = 3) {
  numeric_cols <- vapply(data, is.numeric, logical(1))
  data[numeric_cols] <- lapply(data[numeric_cols], round, digits = digits)
  data
}
