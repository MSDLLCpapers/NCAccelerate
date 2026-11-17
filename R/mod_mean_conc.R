#' Mean concentration data module
#'
#' Summary-level concentrations with optional BLQ rules and GM imputation.
#'
#' @param id Module id.
#' @param data Reactive returning filtered concentration data.
#' @return `mod_mean_conc_server()` returns a reactive with the output of
#'   [summarise_concentration()].
#' @name mod_mean_conc
#' @keywords internal
mod_mean_conc_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    class = "well",
    shiny::HTML('<ul class="help-text">
      <li>Columns are renamed for plotting purpose</li>
      <li>Data grouped by ANALYTE, MATRIX, PANEL, PERIOD, DOSE, NMHR</li>
      <li>Column N shows the number of subjects within each group</li>
      </ul>'),
    shiny::fluidRow(shiny::column(3, shiny::textInput(ns("lloq"), "Enter LLOQ value", value = ""))),
    shiny::fluidRow(shiny::column(6, shiny::checkboxInput(ns("apply_blq"), "Apply BLQ rules", value = FALSE))),
    shiny::HTML('<ul class="help-text">
      <li>Time point with >50% of subjects that have concentrations lower than LLOQ will not be reported</li>
      <li>Time point with a mean concentration lower than LLOQ will not be reported</li>
      <li>BLQ rules are only applied to time points after Tmax</li>
      </ul>'),
    shiny::fluidRow(shiny::column(6, shiny::checkboxInput(ns("impute_gm"), "Impute 0 values with 1/2 LLOQ for GM", value = FALSE))),
    shiny::HTML('<ul class="help-text">
      <li>Replace 0 values with 1/2 LLOQ.</li>
      <li>If not checked, GM shows "NC" for groups with 0 as concentration values.</li>
      </ul>'),
    DT::DTOutput(ns("data_summary"))
  )
}

#' @rdname mod_mean_conc
mod_mean_conc_server <- function(id, data) {
  shiny::moduleServer(id, function(input, output, session) {
    summary_data <- shiny::reactive({
      shiny::req(data())
      summarise_concentration(data(), lloq = input$lloq,
                              apply_blq = isTRUE(input$apply_blq),
                              impute_gm = isTRUE(input$impute_gm))
    })

    output$data_summary <- DT::renderDT({
      DT::datatable(round_numeric_columns(summary_data(), 3), rownames = FALSE,
                    options = list(pageLength = 10, scrollX = TRUE,
                                   searchHighlight = TRUE))
    })

    summary_data
  })
}
