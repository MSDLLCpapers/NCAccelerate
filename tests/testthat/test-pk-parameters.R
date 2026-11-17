test_that("Watson PK parameter file drops the units row and appends dose unit", {
  raw <- read_pk_parameter_data(example_path("example_pk_parameters.csv"))
  expect_false(any(raw$Parameter == "" | is.na(raw$Parameter)))
  expect_true(all(grepl(" mg$", raw$PKEXTDOSE)))
  expect_type(raw$Estimate, "double")
})

test_that("Ardena PK parameter file is read unchanged", {
  raw <- read_pk_parameter_data(example_path("example_pk_parameters_mad.csv"))
  expect_true(all(grepl(" mg$", raw$PKEXTDOSE)))
  expect_equal(nrow(raw), 18 * 2 * 6)
})

test_that("PK parameter reader validates input", {
  expect_error(read_pk_parameter_data("file.txt"), "Unsupported file format")
  path <- withr::local_tempfile(fileext = ".csv")
  writeLines(c("Parameter,Estimate", "Cmax,1"), path)
  expect_error(read_pk_parameter_data(path), "Missing required column")
})

test_that("transform pivots to one row per subject and day", {
  wide <- transform_pk_parameters(read_pk_parameter_data(example_path("example_pk_parameters.csv")))
  expect_equal(nrow(wide), 18)
  expect_true(all(c("Analyte", "Matrix", "Dose", "Period", "Panel", "Day",
                    "PKEXTSUBJID", "Cmax", "Tmax", "AUClast", "AUCINF_pred",
                    "HL_Lambda_z") %in% names(wide)))
  expect_false("Flag_AUC_%Ext_pred" %in% names(wide))
  expect_type(wide$Dose, "double")
  expect_false(is.unsorted(wide$Dose))
  expect_equal(unique(wide$Period), 1)
  expect_equal(pk_parameter_names(wide),
               sort(c("AUCINF_pred", "AUClast", "Cmax", "HL_Lambda_z", "Tmax")))
})

test_that("unit labels follow the original renaming map", {
  labels <- pk_parameter_labels(read_pk_parameter_data(example_path("example_pk_parameters.csv")))
  expect_equal(labels[["Cmax"]], "Cmax (ng/mL)")
  expect_equal(labels[["t1/2"]], "t1/2 (h)")
  expect_equal(labels[["AUC0-inf"]], "AUC0-inf (h*ng/mL)")
  expect_false("Flag_AUC_%Ext_pred" %in% names(labels))
})

test_that("accumulation ratio is last day / first day on the last-day row", {
  wide <- add_accumulation_ratio(make_pk_wide(), "Cmax")
  expect_equal(wide$Cmax_AR[wide$Day == 7], c(2, 2, 2, 1.5, 1.5, 1.5))
  expect_true(all(is.na(wide$Cmax_AR[wide$Day == 1])))
})

test_that("accumulation ratio is skipped for single-day data", {
  wide <- make_pk_wide()
  wide <- wide[wide$Day == 1, ]
  expect_identical(add_accumulation_ratio(wide, "Cmax"), wide)
  expect_identical(add_accumulation_ratio(make_pk_wide(), NULL), make_pk_wide())
})

test_that("summary reports GM (CV%) and median (min-max)", {
  wide <- make_pk_wide()
  wide <- wide[wide$Day == 1, ]
  s <- summarise_pk_parameters(wide, parameters = c("Cmax", "Tmax"),
                               strata = "Dose", time_vars = "Tmax")
  expect_equal(names(s), c("Dose", "N", "Cmax", "Tmax"))
  expect_equal(s$N, c(3, 3))
  expect_equal(s$Cmax[1], paste0(table1::signif_pad(geo_mean(c(10, 12, 8)), 3), " (",
                                 table1::signif_pad(geo_cv(c(10, 12, 8)), 3), ")"))
  expect_equal(s$Tmax[1], "1.00 (1.00-2.00)")
})

test_that("summary reports NC for zeros and '-' for missing values", {
  wide <- make_pk_wide()
  wide$Cmax[1] <- 0
  wide$Tmax[3] <- NA
  s <- summarise_pk_parameters(wide[wide$Day == 1, ], c("Cmax", "Tmax"), "Dose")
  expect_equal(s$Cmax[1], "NC")
  expect_equal(s$Tmax[1], "-")
})

test_that("summary includes AR columns and applies unit labels", {
  wide <- add_accumulation_ratio(make_pk_wide(), "Cmax")
  s <- summarise_pk_parameters(wide, parameters = "Cmax", strata = c("Dose", "Day"),
                               ar_parameters = "Cmax",
                               labels = c(Cmax = "Cmax (ng/mL)"))
  expect_equal(names(s), c("Dose", "Day", "N", "Cmax (ng/mL)", "Cmax_AR"))
  expect_equal(s$Cmax_AR[s$Day == 1], c("-", "-"))
  expect_equal(s$Cmax_AR[s$Day == 7 & s$Dose == 10], "2.00 (0)")
})

test_that("summary errors on unknown columns", {
  expect_error(summarise_pk_parameters(make_pk_wide(), "AUC", "Dose"), "not found")
})

test_that("summary works end to end on the MAD example", {
  raw <- read_pk_parameter_data(example_path("example_pk_parameters_mad.csv"))
  wide <- add_accumulation_ratio(transform_pk_parameters(raw), c("Cmax", "AUClast"))
  s <- summarise_pk_parameters(wide, c("Cmax", "Tmax", "AUClast"),
                               strata = c("Panel", "Dose", "Day"),
                               time_vars = "Tmax", ar_parameters = c("Cmax", "AUClast"),
                               labels = pk_parameter_labels(raw))
  expect_equal(nrow(s), 6)
  expect_true(all(c("Cmax (ng/mL)", "Tmax (h)", "Cmax_AR", "AUClast_AR") %in% names(s)))
})
