#' Dose proportionality module
#'
#' Power-model dose proportionality plot, assessment and parameter
#' projections with exposure margins.
#'
#' @param id Module id.
#' @param pk_wide Reactive returning [transform_pk_parameters()] output.
#' @return `mod_dose_prop_server()` returns a list of reactives: `plot`,
#'   `model` and `projections`.
#' @name mod_dose_prop
#' @keywords internal
mod_dose_prop_ui <- function(id) {
  ns <- shiny::NS(id)
  inline <- function(...) shiny::div(class = "inline-input", ...)
  shiny::div(
    class = "well",
    shiny::HTML('<ul class="help-text"><li>Upload final PK parameters data before proceed</li></ul>'),
    shiny::fluidRow(
      shiny::column(4, shiny::selectInput(ns("pk_param"), "Select a PK parameter", choices = NULL)),
      shiny::column(4, shiny::textInput(ns("pk_unit"), "Parameter unit", value = "")),
      shiny::column(4, shiny::textInput(ns("dose_unit"), "Dose unit", value = ""))
    ),
    shiny::fluidRow(
      shiny::column(4, shiny::selectInput(ns("subjid_exclude"), "Subject ID to exclude",
                                          choices = NULL, multiple = TRUE))
    ),
    shiny::tags$hr(),
    shiny::br(),
    shiny::h5("Dose Proportionality", style = "text-align: center; font-weight: bold;"),
    shiny::plotOutput(ns("dp_plot")),
    shiny::br(),
    shiny::htmlOutput(ns("assessment")),
    shiny::tags$hr(),
    shiny::h6("Plot customization", style = "font-weight: bold;"),
    shiny::br(),
    shiny::fluidRow(shiny::column(8,
      shiny::p(class = "inline-label", "X-axis limits"),
      inline(shiny::numericInput(ns("x_lower"), "Lower", NA, width = "100%")),
      inline(shiny::numericInput(ns("x_upper"), "Upper", NA, width = "100%")),
      inline(shiny::checkboxInput(ns("x_auto"), "Auto", value = TRUE))
    )),
    shiny::fluidRow(
      shiny::column(3, shiny::radioButtons(ns("y_axis_scale"), "Y-axis scale", choices = c("Linear", "Log"))),
      shiny::column(3, shiny::radioButtons(ns("x_axis_scale"), "X-axis scale", choices = c("Linear", "Log")))
    ),
    shiny::fluidRow(shiny::column(8,
      shiny::p(class = "inline-label", "Error bar width"),
      inline(shiny::numericInput(ns("error_bar_width"), NULL, NA, width = "100%")),
      inline(shiny::checkboxInput(ns("error_bar_auto"), "Auto", TRUE))
    )),
    shiny::fluidRow(shiny::column(8,
      shiny::p(class = "inline-label", "Prediction interval"),
      inline(shiny::checkboxInput(ns("remove_pi"), "Remove"))
    )),
    shiny::br(),
    shiny::downloadButton(ns("download_plot"), "Download Plot", class = "btn btn-primary"),
    shiny::br(),
    shiny::tags$hr(),
    shiny::br(),
    shiny::h5("Parameter Projections", style = "text-align: center; font-weight: bold;"),
    shiny::fluidRow(shiny::column(4, shiny::textInput(ns("project_doses"), "Doses to project",
                                                      placeholder = "e.g. 200, 400, 800"))),
    shiny::fluidRow(shiny::column(4, shiny::textInput(ns("noael"), "PK parameter NOAELs",
                                                      placeholder = "Enter a value here"))),
    shiny::tableOutput(ns("projections")),
    shiny::downloadButton(ns("download_table"), "Download Table", class = "btn btn-primary")
  )
}

#' @rdname mod_dose_prop
mod_dose_prop_server <- function(id, pk_wide) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(pk_wide(), {
      shiny::updateSelectInput(session, "pk_param", choices = pk_parameter_names(pk_wide()))
      shiny::updateSelectInput(session, "subjid_exclude",
                               choices = sort(unique(pk_wide()$PKEXTSUBJID)))
    })

    dp_data <- shiny::reactive({
      shiny::req(pk_wide(), input$pk_param)
      data <- prepare_dose_proportionality(pk_wide(), input$pk_param, input$subjid_exclude)
      shiny::validate(shiny::need(length(unique(data$Dose)) >= 2,
                                  "At least two dose levels with non-zero values are needed."))
      data
    })

    model <- shiny::reactive(fit_dose_proportionality(dp_data()))
    dose_summary <- shiny::reactive(summarise_dose_groups(dp_data(), input$pk_param))

    plot_obj <- shiny::reactive({
      plot_dose_proportionality(
        dp_data(), input$pk_param,
        x_limits = if (isTRUE(input$x_auto)) c(NA, NA) else c(input$x_lower, input$x_upper),
        log_x = identical(input$x_axis_scale, "Log"),
        log_y = identical(input$y_axis_scale, "Log"),
        error_bar_width = if (isTRUE(input$error_bar_auto)) NA else input$error_bar_width,
        show_interval = !isTRUE(input$remove_pi),
        dose_unit = input$dose_unit,
        parameter_unit = input$pk_unit
      )
    })

    output$dp_plot <- shiny::renderPlot(plot_obj())

    output$assessment <- shiny::renderUI({
      shiny::HTML(paste0("<b>Assessment:</b> ", assess_dose_proportionality(model())))
    })

    output$download_plot <- shiny::downloadHandler(
      filename = function() paste0("dose_proportionality_", timestamp_digits(), ".png"),
      content = function(file) {
        size <- plot_size_inches(session, "dp_plot")
        ggplot2::ggsave(file, plot_obj(), width = size$width,
                        height = size$height, device = "png")
      }
    )

    projections <- shiny::reactive({
      doses <- parse_number_list(input$project_doses)
      shiny::req(length(doses) > 0)
      project_pk_parameters(model(), dose_summary(), doses = doses,
                            noael = input$noael, parameter = input$pk_param,
                            dose_unit = input$dose_unit,
                            parameter_unit = input$pk_unit)
    })

    output$projections <- shiny::renderTable(projections())

    output$download_table <- shiny::downloadHandler(
      filename = function() paste0("parameter_projections_", timestamp_digits(), ".docx"),
      content = function(file) {
        render_table_docx(projections(), file, title = "Parameter Projections")
      }
    )

    list(plot = plot_obj, model = model, projections = projections)
  })
}
