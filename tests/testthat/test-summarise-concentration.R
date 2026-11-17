conc_values <- c(0, 10, 5, 0.5,
                 0, 12, 6, 0.4,
                 0, 8, 4, 2,
                 0, 14, 7, 0.3)

test_that("summary renames columns and computes AM/SD/GM by time point", {
  s <- summarise_concentration(make_conc_data(conc_values))
  expect_equal(s$NMHR, c(0, 1, 2, 4))
  expect_true(all(c("ANALYTE", "MATRIX", "PANEL", "PERIOD", "DAY", "DOSE",
                    "DOSEUNIT", "BLQ_EXCLUDED", "N", "GM", "GCV", "GSD", "AM",
                    "SD", "CONC_UNIT", "NMHR_UNIT") %in% names(s)))
  expect_equal(s$N, rep(4, 4))
  expect_equal(s$AM[2], mean(c(10, 12, 8, 14)))
  expect_equal(s$SD[2], sd(c(10, 12, 8, 14)))
  expect_equal(as.numeric(s$GM[2]), geo_mean(c(10, 12, 8, 14)))
  expect_equal(s$BLQ_EXCLUDED, rep("N", 4))
})

test_that("BLQ rules flag time points after Tmax only", {
  s <- summarise_concentration(make_conc_data(conc_values), lloq = 1,
                               apply_blq = TRUE)
  # NMHR 4: 3 of 4 subjects < LLOQ -> excluded; pre-dose never excluded
  expect_equal(s$BLQ_EXCLUDED, c("N", "N", "N", "Y"))
})

test_that("BLQ rules are not applied before Tmax", {
  conc <- c(0, 0.5, 10, 0.2,
            0, 0.4, 12, 0.1,
            0, 0.3, 8, 0.2,
            0, 2, 14, 0.3)
  s <- summarise_concentration(make_conc_data(conc), lloq = 1, apply_blq = TRUE)
  # NMHR 1 is BLQ but before Tmax (2 h) so kept; NMHR 4 after Tmax is flagged
  expect_equal(s$BLQ_EXCLUDED, c("N", "N", "N", "Y"))
})

test_that("BLQ rules are skipped when LLOQ is missing", {
  s <- summarise_concentration(make_conc_data(conc_values), lloq = "",
                               apply_blq = TRUE)
  expect_equal(s$BLQ_EXCLUDED, rep("N", 4))
})

test_that("post-dose zeros give NC unless imputed with LLOQ / 2", {
  conc <- conc_values
  conc[3] <- 0 # subject 1, NMHR 2
  s <- summarise_concentration(make_conc_data(conc))
  expect_equal(s$GM[s$NMHR == 2], "NC")
  expect_equal(s$GM[s$NMHR == 0], "0")

  imputed <- summarise_concentration(make_conc_data(conc), lloq = 1, impute_gm = TRUE)
  expect_equal(as.numeric(imputed$GM[imputed$NMHR == 2]), geo_mean(c(0.5, 6, 4, 7)))
  # AM is never imputed
  expect_equal(imputed$AM[imputed$NMHR == 2], mean(c(0, 6, 4, 7)))
})

test_that("single-subject groups are dropped", {
  two_groups <- rbind(
    make_conc_data(conc_values),
    make_conc_data(c(0, 1, 2, 3), subj = rep(99, 4), dose = 20)
  )
  s <- summarise_concentration(two_groups)
  expect_false(20 %in% s$DOSE)
})

test_that("summary works on the example file", {
  conc <- read_concentration_data(example_path("example_concentration.csv"))
  s <- summarise_concentration(conc, lloq = 1, apply_blq = TRUE, impute_gm = TRUE)
  expect_equal(sort(unique(s$DOSE)), c(10, 50, 200))
  expect_true(all(s$N == 6))
})

test_that("round_numeric_columns rounds only numeric columns", {
  df <- round_numeric_columns(data.frame(a = 1.23456, b = "x"), 2)
  expect_equal(df$a, 1.23)
  expect_equal(df$b, "x")
})
