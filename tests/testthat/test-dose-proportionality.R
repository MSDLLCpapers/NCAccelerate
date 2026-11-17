pk_wide_example <- function() {
  transform_pk_parameters(read_pk_parameter_data(example_path("example_pk_parameters.csv")))
}

proportional_data <- function(slope = 1) {
  doses <- rep(c(10, 30, 100, 300), each = 4)
  noise <- rep(c(0.9, 1.1, 0.95, 1.05), 4)
  data.frame(PKEXTSUBJID = seq_along(doses), Dose = doses,
             Cmax = 2 * doses^slope * noise)
}

test_that("prepare drops zeros, NAs and excluded subjects", {
  d <- proportional_data()
  d$Cmax[1] <- 0
  d$Cmax[2] <- NA
  dp <- prepare_dose_proportionality(d, "Cmax", exclude_subjects = 3)
  expect_equal(nrow(dp), nrow(d) - 3)
  expect_equal(dp$LNY, log(dp$Cmax))
  expect_equal(dp$LNX, log(dp$Dose))
  expect_error(prepare_dose_proportionality(d, "AUC"), "not found")
})

test_that("dose group summary returns GM and 95% CI per dose", {
  dp <- prepare_dose_proportionality(proportional_data(), "Cmax")
  s <- summarise_dose_groups(dp, "Cmax")
  expect_equal(s$Dose, c(10, 30, 100, 300))
  expect_equal(s$gm[1], signif(geo_mean(dp$Cmax[dp$Dose == 10]), 3))
  expect_true(all(s$CIL < s$gm & s$gm < s$CIU))
})

test_that("power model recovers the slope and assessment categories", {
  for (case in list(list(slope = 1, text = "assume parameter is dose proportional"),
                    list(slope = 0.5, text = "less than dose proportional"),
                    list(slope = 1.5, text = "more than dose proportional"))) {
    model <- fit_dose_proportionality(
      prepare_dose_proportionality(proportional_data(case$slope), "Cmax"))
    expect_equal(unname(coef(model)[2]), case$slope, tolerance = 0.02)
    expect_match(assess_dose_proportionality(model), case$text)
  }
  model <- fit_dose_proportionality(prepare_dose_proportionality(proportional_data(), "Cmax"))
  expect_match(assess_dose_proportionality(model),
               "^Parameter slope is 1 with 95% CI \\[[0-9.]+, [0-9.]+\\] and r-squared of")
})

test_that("assessment handles an unidentifiable slope", {
  d <- data.frame(PKEXTSUBJID = 1:3, Dose = 10, Cmax = c(1, 2, 3))
  model <- suppressWarnings(fit_dose_proportionality(prepare_dose_proportionality(d, "Cmax")))
  expect_match(assess_dose_proportionality(model), "Could not assess")
})

test_that("predictions are on the original scale", {
  model <- fit_dose_proportionality(prepare_dose_proportionality(proportional_data(), "Cmax"))
  pred <- predict_dose_proportionality(model, c(100, 1000))
  expect_equal(names(pred), c("fit", "lwr", "upr", "Dose"))
  expect_equal(pred$fit, c(200, 2000), tolerance = 0.05)
  expect_true(all(pred$lwr < pred$fit & pred$fit < pred$upr))
})

test_that("projections combine observed and predicted values with exposure margins", {
  dp <- prepare_dose_proportionality(proportional_data(), "Cmax")
  model <- fit_dose_proportionality(dp)
  proj <- project_pk_parameters(model, summarise_dose_groups(dp, "Cmax"),
                                doses = c(1000), noael = 4000, parameter = "Cmax",
                                dose_unit = "mg", parameter_unit = "ng/mL")
  expect_equal(names(proj), c("Parameter", "Dose (mg)", "Observed (ng/mL)",
                              "Observed EM", "Prediction (ng/mL)", "Prediction EM"))
  expect_equal(proj[["Dose (mg)"]], c(10, 30, 100, 300, 1000))
  expect_equal(unique(proj$Parameter), "Cmax")
  expect_equal(proj[["Observed (ng/mL)"]][5], "NA (NA, NA)")
  # NOAEL 4000 / predicted ~2000 at 1000 mg -> exposure margin ~2
  expect_match(proj[["Prediction EM"]][5], "^(1\\.9|2)[0-9.]* \\(")

  no_units <- project_pk_parameters(model, summarise_dose_groups(dp, "Cmax"),
                                    doses = 1000, parameter = "Cmax")
  expect_equal(names(no_units)[2:3], c("Dose", "Observed"))
  expect_match(no_units[["Observed EM"]][1], "^NA")
})

test_that("dose proportionality plot builds with all options", {
  dp <- prepare_dose_proportionality(pk_wide_example(), "Cmax")
  p <- plot_dose_proportionality(dp, "Cmax", dose_unit = "mg", parameter_unit = "ng/mL")
  expect_s3_class(p, "ggplot")
  expect_equal(p$labels$x, "Dose (mg)")
  expect_equal(p$labels$y, "Cmax (ng/mL)")
  expect_true(any(vapply(p$layers, function(l) inherits(l$geom, "GeomRibbon"), logical(1))))
  expect_no_error(ggplot2::ggplot_build(p))

  p_log <- plot_dose_proportionality(dp, "Cmax", log_x = TRUE, log_y = TRUE,
                                     show_interval = FALSE, error_bar_width = 2)
  expect_false(any(vapply(p_log$layers, function(l) inherits(l$geom, "GeomRibbon"), logical(1))))
  expect_equal(p_log$scales$get_scales("y")$trans$name, "log-10")
  expect_no_error(ggplot2::ggplot_build(p_log))

  p_lim <- plot_dose_proportionality(dp, "Cmax", x_limits = c(20, 100))
  expect_equal(p_lim$data$Dose, 50)
  expect_equal(plot_dose_proportionality(dp, "Cmax", x_limits = c(1000, NA))$labels$title,
               "No doses in the selected range")
})
