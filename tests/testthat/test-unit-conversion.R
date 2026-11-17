test_that("mass to molar conversion uses MW in kDa", {
  # 150 ng/mL of a 150 kDa protein = 1 nM
  expect_equal(convert_concentration(150, "ng/mL", "nM", mw = 150), 1)
  expect_equal(convert_concentration(1, "nM", "ng/mL", mw = "150"), 150)
  expect_equal(convert_concentration(1, "uM", "ug/mL", mw = 2), 2)
})

test_that("same-dimension conversions ignore MW", {
  expect_equal(convert_concentration(1, "nM", "pM"), 1000)
  expect_equal(convert_concentration(1, "nM", "pM", mw = 150), 1000)
  expect_equal(convert_concentration(1000, "ng/mL", "ug/mL", mw = 150), 1)
})

test_that("blank MW is treated as 1 and vectors are supported", {
  expect_equal(convert_concentration(c(1, 2), "ng/mL", "nM", mw = ""), c(1, 2))
  expect_equal(convert_concentration(c(1, NA), "ng/mL", "pg/mL"), c(1000, NA))
})

test_that("unknown units error", {
  expect_error(convert_concentration(1, "g/L", "nM"), "Unsupported unit")
})
