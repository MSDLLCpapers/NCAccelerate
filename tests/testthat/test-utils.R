test_that("geometric statistics match their definitions", {
  x <- c(2, 8)
  expect_equal(geo_mean(x), 4)
  expect_equal(geo_sd(x), exp(sd(log(x))))
  expect_equal(geo_cv(x), 100 * sqrt(exp(sd(log(x))^2) - 1))
  expect_equal(geo_mean(c(1, NA, 100)), 10)
})

test_that("geometric mean CI brackets the geometric mean", {
  x <- c(10, 12, 15, 9, 11)
  expect_lt(geo_ci_lower(x), geo_mean(x))
  expect_gt(geo_ci_upper(x), geo_mean(x))
  expect_equal(log(geo_ci_upper(x)) - log(geo_mean(x)),
               log(geo_mean(x)) - log(geo_ci_lower(x)))
})

test_that("geometric mean of a group containing zero is zero", {
  expect_equal(geo_mean(c(0, 5, 10)), 0)
})

test_that("parse_number_input handles blanks and text", {
  expect_true(is.na(parse_number_input(NULL)))
  expect_true(is.na(parse_number_input("")))
  expect_true(is.na(parse_number_input("  ")))
  expect_true(is.na(parse_number_input("abc")))
  expect_equal(parse_number_input("2.5"), 2.5)
  expect_equal(parse_number_input(3), 3)
})

test_that("parse_number_list splits comma separated values", {
  expect_equal(parse_number_list("200, 400,800"), c(200, 400, 800))
  expect_equal(parse_number_list(""), numeric(0))
  expect_equal(parse_number_list("200, x"), 200)
})

test_that("fill_period fills blanks with 1", {
  expect_equal(fill_period(c(NA, "", "2")), c(1, 1, 2))
  expect_equal(fill_period(c(1, NA)), c(1, 1))
  expect_equal(fill_period(c("P1", NA)), c("P1", "1"))
})

test_that("timestamp_digits produces a 12 digit string", {
  ts <- timestamp_digits(as.POSIXct("2026-07-01 14:30:00"))
  expect_equal(ts, "202607011430")
})

test_that("label_with_unit only adds non-empty units", {
  expect_equal(label_with_unit("Dose", "mg"), "Dose (mg)")
  expect_equal(label_with_unit("Dose", ""), "Dose")
  expect_equal(label_with_unit("Dose", NULL), "Dose")
})

test_that("render_table_docx writes a Word document", {
  skip_if_not(rmarkdown::pandoc_available(), "pandoc not available")
  out <- withr::local_tempfile(fileext = ".docx")
  render_table_docx(data.frame(a = 1:2, b = c("x", "y")), out,
                    title = "Test", footnotes = c("Note 1", "Note 2"))
  expect_true(file.exists(out))
  expect_gt(file.size(out), 0)
})
