#' Data check module
#'
#' Shows the uploaded time-concentration data and offers an optional
#' concentration unit conversion.
#'
#' @param id Module id.
#' @param data_original Reactive returning the uploaded concentration data.
#' @return `mod_data_check_server()` returns a reactive with the (possibly
#'   unit-converted) data.
#' @name mod_data_check
#' @keywords internal
mod_data_check_ui <- function(id) {
  ns <- shiny::NS(id)
  units <- names(concentration_units)
  shiny::div(
    class = "well",
    shiny::HTML('<ul class="help-text">
      <li>This tab shows the uploaded time-concentration data</li>
      <li>Concentration unit conversion is optional:</li>
      <ol>
        <li>Choose an "Original unit", which should be the same as what is shown in ORRESU upon uploading</li>
        <li>PKEXTORRES (concentration value) and ORRESU (concentration unit) will show converted values</li>
        <li>Choose a different "Convert to" unit if needed, <strong>do not</strong> change "Original unit"</li>
      </ol>
      </ul>'),
    shiny::strong("The concentration unit should have been converted in Phoenix workflow, use only when needed."),
    shiny::fluidRow(
      shiny::column(4, shiny::selectInput(ns("from_unit"), "Original unit",
                                          choices = units, selected = "nM")),
      shiny::column(4, shiny::selectInput(ns("to_unit"), "Convert to",
                                          choices = units, selected = "ng/mL")),
      shiny::column(4, shiny::textInput(ns("mw"), "Molecular weight in KD",
                                        placeholder = "Enter a number here"))
    ),
    shiny::actionButton(ns("convert"), "Convert"),
    shiny::br(), shiny::br(), shiny::br(),
    DT::DTOutput(ns("raw_data"))
  )
}

#' @rdname mod_data_check
mod_data_check_server <- function(id, data_original) {
  shiny::moduleServer(id, function(input, output, session) {
    data_raw <- shiny::reactiveVal()

    shiny::observeEvent(data_original(), data_raw(data_original()))

    shiny::observeEvent(input$convert, {
      df <- data_original()
      shiny::req(df)
      df$PKEXTORRES <- convert_concentration(df$PKEXTORRES, input$from_unit,
                                             input$to_unit, input$mw)
      df$ORRESU <- input$to_unit
      data_raw(df)
    })

    output$raw_data <- DT::renderDT({
      shiny::req(data_raw())
      DT::datatable(data_raw(), class = "display",
                    caption = "Scroll right for more columns", filter = "top",
                    options = list(pageLength = 10, scrollX = TRUE,
                                   searchHighlight = TRUE))
    })

    shiny::reactive(data_raw())
  })
}
