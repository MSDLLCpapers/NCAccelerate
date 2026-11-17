upload_value <- function(path, name = basename(path)) {
  data.frame(name = name, size = file.size(path), type = "text/csv",
             datapath = path, stringsAsFactors = FALSE)
}

conc_example <- function() read_concentration_data(example_path("example_concentration.csv"))

test_that("upload module reads both files", {
  shiny::testServer(mod_upload_server, {
    session$setInputs(conc_file = upload_value(example_path("example_concentration.csv")),
                      pk_file = upload_value(example_path("example_pk_parameters.csv")))
    out <- session$returned
    expect_equal(nrow(out$concentration()), nrow(conc_example()))
    expect_true("Parameter" %in% names(out$pk_parameters()))
  })
})

test_that("upload module returns NULL for unreadable files", {
  bad <- withr::local_tempfile(fileext = ".csv")
  writeLines(c("A,B", "1,2", "3,4"), bad)
  shiny::testServer(mod_upload_server, {
    session$setInputs(conc_file = upload_value(bad))
    expect_null(session$returned$concentration())
  })
})

test_that("data check module converts units only when asked", {
  df <- conc_example()
  shiny::testServer(mod_data_check_server, args = list(data_original = shiny::reactive(df)), {
    session$flushReact()
    expect_equal(session$returned()$PKEXTORRES, df$PKEXTORRES)

    session$setInputs(from_unit = "ng/mL", to_unit = "nM", mw = "150", convert = 1)
    converted <- session$returned()
    expect_equal(converted$PKEXTORRES, df$PKEXTORRES / 150)
    expect_equal(unique(converted$ORRESU), "nM")

    # A second conversion always starts from the original data
    session$setInputs(to_unit = "pg/mL", convert = 2)
    expect_equal(session$returned()$PKEXTORRES, df$PKEXTORRES * 1000)
  })
})

test_that("filter module applies cascading filters and group exclusion", {
  df <- conc_example()
  shiny::testServer(mod_filter_server, args = list(data = shiny::reactive(df)), {
    session$setInputs(analyte = "DRUG-X", matrix = "Plasma", panel = c("A", "B"),
                      period = "1", dose = c("10", "50", "200"), exclude_check = FALSE,
                      exclude_group = "DRUG-X_Plasma_A_1_10")
    expect_equal(sort(unique(session$returned()$PKEXTPKSEQ)), c("A", "B"))

    session$setInputs(exclude_check = TRUE)
    expect_equal(unique(session$returned()$PKEXTPKSEQ), "B")

    session$setInputs(dose = "200")
    expect_equal(nrow(session$returned()), 0)
  })
})

test_that("mean concentration module applies BLQ options", {
  df <- conc_example()
  shiny::testServer(mod_mean_conc_server, args = list(data = shiny::reactive(df)), {
    session$setInputs(lloq = "", apply_blq = FALSE, impute_gm = FALSE)
    expect_true(all(session$returned()$BLQ_EXCLUDED == "N"))

    session$setInputs(lloq = "100", apply_blq = TRUE)
    expect_true(any(session$returned()$BLQ_EXCLUDED == "Y"))
    expect_true(inherits(output$data_summary, "json"))
  })
})

test_that("SAD plot module applies submitted options and downloads a PNG", {
  s <- summarise_concentration(conc_example())
  shiny::testServer(mod_conc_plot_server,
                    args = list(summary_data = shiny::reactive(s), type = "SAD"), {
    session$setInputs(am_gm = "GM", error_bar = "On", axis_scale = "Linear",
                      start_time = "0", end_time = "24", x_interval = 6,
                      x_axis_title = "", y_axis_title = "",
                      pk_target = "50", lloq = "1", plot_title = "")
    p <- session$returned()
    expect_null(p$labels$caption) # reference lines need "Submit Changes"

    session$setInputs(plot_title = "SAD plot", submit = 1)
    p <- session$returned()
    expect_equal(p$labels$title, "SAD plot")
    expect_match(p$labels$caption, "PK Target")
    expect_null(p$facet$params$facets)

    png <- output$download_plot
    expect_true(file.exists(png))
    expect_gt(file.size(png), 0)
  })
})

test_that("MAD plot module facets by the selected days", {
  s <- summarise_concentration(read_concentration_data(example_path("example_concentration_mad.csv")))
  shiny::testServer(mod_conc_plot_server,
                    args = list(summary_data = shiny::reactive(s), type = "MAD"), {
    session$setInputs(am_gm = "AM", error_bar = "Off", axis_scale = "Linear",
                      start_time = "0", end_time = "24", x_interval = 6,
                      x_axis_title = "", y_axis_title = "", free_x = FALSE)
    expect_equal(session$returned()$labels$title, "Select day(s) to plot")

    session$setInputs(select_day = c("1", "7"))
    p <- session$returned()
    expect_s3_class(p$facet, "FacetWrap")
    expect_equal(sort(unique(p$data$DAY)), c(1, 7))
  })
})

test_that("PK table module summarises selected parameters", {
  raw <- read_pk_parameter_data(example_path("example_pk_parameters_mad.csv"))
  wide <- transform_pk_parameters(raw)
  shiny::testServer(mod_pk_table_server,
                    args = list(pk_raw = shiny::reactive(raw), pk_wide = shiny::reactive(wide)), {
    session$setInputs(select_pk_para = c("Cmax", "Tmax"), select_stratify = c("Dose", "Day"),
                      select_time_var = "Tmax", select_ar = "Cmax", mask_n = TRUE)
    s <- session$returned()
    expect_equal(names(s), c("Dose", "Day", "N", "Cmax (ng/mL)", "Tmax (h)", "Cmax_AR"))
    expect_equal(nrow(s), 6)
    expect_true(inherits(output$summary_table, "json"))

    skip_if_not(rmarkdown::pandoc_available(), "pandoc not available")
    docx <- output$download_table
    expect_true(file.exists(docx))
  })
})

test_that("dose proportionality module fits, assesses and projects", {
  wide <- transform_pk_parameters(read_pk_parameter_data(example_path("example_pk_parameters.csv")))
  shiny::testServer(mod_dose_prop_server, args = list(pk_wide = shiny::reactive(wide)), {
    session$setInputs(pk_param = "Cmax", subjid_exclude = NULL, x_auto = TRUE,
                      x_lower = NA, x_upper = NA, x_axis_scale = "Linear",
                      y_axis_scale = "Linear", error_bar_auto = TRUE,
                      error_bar_width = NA, remove_pi = FALSE, dose_unit = "mg",
                      pk_unit = "ng/mL", project_doses = "400, 800", noael = "5000")
    out <- session$returned
    expect_s3_class(out$model(), "lm")
    expect_s3_class(out$plot(), "ggplot")
    expect_equal(out$projections()[["Dose (mg)"]], c(10, 50, 200, 400, 800))
    expect_match(as.character(output$assessment$html), "Assessment:")

    png <- output$download_plot
    expect_true(file.exists(png))
  })
})
