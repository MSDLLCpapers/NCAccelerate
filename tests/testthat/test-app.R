test_that("UI contains every tab and namespaced module inputs", {
  html <- as.character(app_ui(NULL))
  for (tab in c("Instructions", "Data Check", "Mean Concentration Data",
                "Time-Concentration Plot", "PK Parameter Table",
                "Dose Proportionality")) {
    expect_match(html, tab, fixed = TRUE)
  }
  for (id in c("upload-conc_file", "upload-pk_file", "filter-analyte",
               "data_check-convert", "mean_conc-lloq", "sad_plot-plot",
               "mad_plot-select_day", "pk_table-select_pk_para",
               "dose_prop-dp_plot")) {
    expect_match(html, paste0('id="', id, '"'), fixed = TRUE)
  }
  # MAD x-axis controls are hidden when the free x scale is selected
  expect_match(html, 'data-display-if="input.free_x == false" data-ns-prefix="mad_plot-"',
               fixed = TRUE)
})

test_that("run_app returns a Shiny app object", {
  expect_s3_class(run_app(), "shiny.appobj")
})

test_that("app server wires uploads through to the summary outputs", {
  upload <- function(path) {
    data.frame(name = basename(path), size = file.size(path), type = "text/csv",
               datapath = path, stringsAsFactors = FALSE)
  }
  shiny::testServer(app_server, {
    session$setInputs(
      `upload-conc_file` = upload(example_path("example_concentration.csv")),
      `upload-pk_file` = upload(example_path("example_pk_parameters.csv")),
      `filter-analyte` = "DRUG-X", `filter-matrix` = "Plasma",
      `filter-panel` = c("A", "B", "C"), `filter-period` = "1",
      `filter-dose` = c("10", "50", "200"), `filter-exclude_check` = FALSE,
      `mean_conc-lloq` = "", `mean_conc-apply_blq` = FALSE,
      `mean_conc-impute_gm` = FALSE,
      `pk_table-select_pk_para` = c("Cmax", "AUClast"),
      `pk_table-select_stratify` = "Dose", `pk_table-mask_n` = FALSE
    )
    expect_true(inherits(output[["mean_conc-data_summary"]], "json"))
    expect_true(inherits(output[["pk_table-summary_table"]], "json"))
    expect_equal(nrow(pk_wide()), 18)
  })
})
