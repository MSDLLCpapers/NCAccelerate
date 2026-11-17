test_that("Watson-styled CSV is read with units from the second header row", {
  df <- read_concentration_data(example_path("example_concentration.csv"))
  expect_equal(names(df)[1:11],
               c("PKEXTSTUDYID", "PKEXTSUBJID", "PKEXTPKSEQ", "PKEXTPERIOD",
                 "PKEXTSTDYDY", "PKEXTNMHR", "NMHRU", "PKEXTORRES", "ORRESU",
                 "DOSE", "DOSEUNIT"))
  expect_equal(unique(df$ORRESU), "ng/mL")
  expect_equal(unique(df$NMHRU), "h")
  expect_equal(unique(df$DOSEUNIT), "mg")
  expect_equal(sort(unique(df$DOSE)), c(10, 50, 200))
  expect_type(df$PKEXTORRES, "double")
  expect_false(is.unsorted(df$PKEXTPKSEQ))
})

test_that("Ardena-styled CSV splits dose/unit, parses 'Day1' and fills period", {
  df <- read_concentration_data(example_path("example_concentration_mad.csv"))
  expect_equal(sort(unique(df$DOSE)), c(10, 50, 200))
  expect_equal(unique(df$DOSEUNIT), "mg")
  expect_equal(sort(unique(df$PKEXTSTDYDY)), c(1, 7))
  expect_equal(unique(df$PKEXTPERIOD), 1)
  expect_false("PKEXTDOSE" %in% names(df))
})

test_that("Ardena decimal doses are kept", {
  path <- withr::local_tempfile(fileext = ".csv")
  writeLines(c(
    "PKEXTNAM,PKEXTSTUDYID,PKEXTSUBJID,PKEXTSRCANL,PKEXTSPECAB,PKEXTPKSEQ,PKEXTPERIOD,PKEXTSTDYDY,PKEXTNMHR,PKEXTORRES,PKEXTDOSE",
    ",,,,,,,,h,nM,",
    "ARDENA,S1,1,DRUG,Plasma,A,,1,0,0,0.5 mg",
    "ARDENA,S1,1,DRUG,Plasma,A,,1,1,3.2,0.5mg"
  ), path)
  df <- read_concentration_data(path)
  expect_equal(df$DOSE, c(0.5, 0.5))
  expect_equal(unique(df$DOSEUNIT), "mg")
  expect_equal(unique(df$ORRESU), "nM")
})

test_that("Watson data without PKEXTPERIOD gets period 1", {
  path <- withr::local_tempfile(fileext = ".csv")
  writeLines(c(
    "PKEXTSTUDYID,PKEXTSUBJID,PKEXTSRCANL,PKEXTSPECAB,PKEXTPKSEQ,PKEXTSTDYDY,PKEXTNMHR,PKEXTORRES,PKEXTDOSE",
    ",,,,,,h,ng/mL,mg",
    "S1,1,DRUG,Plasma,A,1,0,0,5"
  ), path)
  df <- read_concentration_data(path)
  expect_equal(df$PKEXTPERIOD, 1)
  expect_equal(df$DOSE, 5)
})

test_that("Excel files are supported", {
  skip_if_not_installed("openxlsx")
  csv <- utils::read.csv(example_path("example_concentration.csv"),
                         header = FALSE, colClasses = "character")
  path <- withr::local_tempfile(fileext = ".xlsx")
  # Header + units rows as text, data rows with native numeric cells
  body <- utils::type.convert(csv[-(1:2), ], as.is = TRUE)
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "data")
  openxlsx::writeData(wb, "data", csv[1:2, ], colNames = FALSE)
  openxlsx::writeData(wb, "data", body, startRow = 3, colNames = FALSE)
  openxlsx::saveWorkbook(wb, path)

  from_xlsx <- read_concentration_data(path)
  from_csv <- read_concentration_data(example_path("example_concentration.csv"))
  expect_equal(nrow(from_xlsx), nrow(from_csv))
  expect_equal(from_xlsx$PKEXTORRES, from_csv$PKEXTORRES)
  expect_equal(unique(from_xlsx$ORRESU), "ng/mL")
})

test_that("upload file name decides the format", {
  path <- example_path("example_concentration.csv")
  tmp <- withr::local_tempfile()
  file.copy(path, tmp)
  df <- read_concentration_data(tmp, file_name = "upload.csv")
  expect_gt(nrow(df), 0)
})

test_that("unsupported formats and missing columns give clear errors", {
  expect_error(read_concentration_data("data.txt"), "Unsupported file format")
  path <- withr::local_tempfile(fileext = ".csv")
  writeLines(c("PKEXTSTUDYID,PKEXTORRES", ",ng/mL", "S1,1"), path)
  expect_error(read_concentration_data(path), "Missing required column")
})

test_that("filter_concentration_data filters and excludes groups", {
  df <- read_concentration_data(example_path("example_concentration.csv"))
  only_a <- filter_concentration_data(df, panel = "A")
  expect_equal(unique(only_a$PKEXTPKSEQ), "A")

  keys <- unique(concentration_group_key(df))
  expect_equal(keys[1], "DRUG-X_Plasma_A_1_10")
  dropped <- filter_concentration_data(df, exclude_groups = keys[1])
  expect_false("A" %in% dropped$PKEXTPKSEQ)
  expect_equal(nrow(filter_concentration_data(df)), nrow(df))
  expect_equal(nrow(filter_concentration_data(df, dose = "50")),
               sum(df$DOSE == 50))
})
