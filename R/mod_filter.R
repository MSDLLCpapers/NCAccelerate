#' Concentration data filter module
#'
#' Cascading filters (analyte, matrix, panel, period, dose) and optional
#' exclusion of individual groups.
#'
#' @param id Module id.
#' @param data Reactive returning concentration data (after unit conversion).
#' @return `mod_filter_server()` returns a reactive with the filtered data.
#' @name mod_filter
#' @keywords internal
mod_filter_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h6("Filter Concentration Data",
              style = "font-weight: bold; margin-bottom: 2px;"),
    shiny::div("Use backspace to de-select",
               style = "font-size: 12px; color: #888; margin-bottom: 2px;"),
    shiny::selectInput(ns("analyte"), "Analyte", choices = NULL),
    shiny::selectInput(ns("matrix"), "Matrix", choices = NULL, multiple = TRUE),
    shiny::selectInput(ns("panel"), "Panel", choices = NULL, multiple = TRUE),
    shiny::selectInput(ns("period"), "Period", choices = NULL, multiple = TRUE),
    shiny::selectInput(ns("dose"), "Dose", choices = NULL, multiple = TRUE),
    shiny::selectInput(ns("exclude_group"), "Groups to exclude", choices = NULL,
                       multiple = TRUE),
    shiny::checkboxInput(ns("exclude_check"),
                         "Exclude group(s) above from the plot", value = FALSE)
  )
}

#' @rdname mod_filter
mod_filter_server <- function(id, data) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observe({
      shiny::req(data())
      choices <- unique(data()$PKEXTSRCANL)
      shiny::updateSelectInput(session, "analyte", choices = choices,
                               selected = choices[1])
    })
    by_analyte <- shiny::reactive({
      shiny::req(data(), input$analyte)
      filter_concentration_data(data(), analyte = input$analyte)
    })

    shiny::observe({
      choices <- unique(by_analyte()$PKEXTSPECAB)
      shiny::updateSelectInput(session, "matrix", choices = choices, selected = choices)
    })
    by_matrix <- shiny::reactive({
      shiny::req(by_analyte(), input$matrix)
      filter_concentration_data(by_analyte(), matrix = input$matrix)
    })

    shiny::observe({
      choices <- unique(by_matrix()$PKEXTPKSEQ)
      shiny::updateSelectInput(session, "panel", choices = choices, selected = choices)
    })
    by_panel <- shiny::reactive({
      shiny::req(by_matrix(), input$panel)
      filter_concentration_data(by_matrix(), panel = input$panel)
    })

    shiny::observe({
      choices <- sort(unique(by_panel()$PKEXTPERIOD))
      shiny::updateSelectInput(session, "period", choices = choices, selected = choices)
    })
    by_period <- shiny::reactive({
      shiny::req(by_panel(), input$period)
      filter_concentration_data(by_panel(), period = input$period)
    })

    shiny::observe({
      choices <- sort(as.numeric(unique(by_period()$DOSE)))
      shiny::updateSelectInput(session, "dose", choices = choices, selected = choices)
    })
    by_dose <- shiny::reactive({
      shiny::req(by_period(), input$dose)
      filter_concentration_data(by_period(), dose = input$dose)
    })

    shiny::observe({
      shiny::updateSelectInput(session, "exclude_group",
                               choices = unique(concentration_group_key(by_dose())),
                               selected = shiny::isolate(input$exclude_group))
    })

    shiny::reactive({
      shiny::req(by_dose())
      if (isTRUE(input$exclude_check) && length(input$exclude_group) > 0) {
        filter_concentration_data(by_dose(), exclude_groups = input$exclude_group)
      } else {
        by_dose()
      }
    })
  })
}
