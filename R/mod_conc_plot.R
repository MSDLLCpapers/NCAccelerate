#' Time-concentration plot module (SAD or MAD)
#'
#' A single module drives both the SAD plot and the MAD plot (faceted by
#' study day). Reference lines and the title are applied with the
#' "Submit Changes" button; other options update live.
#'
#' @param id Module id.
#' @param type `"SAD"` or `"MAD"`.
#' @param summary_data Reactive returning [summarise_concentration()] output.
#' @return `mod_conc_plot_server()` returns a reactive with the `ggplot`.
#' @name mod_conc_plot
#' @keywords internal
mod_conc_plot_ui <- function(id, type = c("SAD", "MAD")) {
  type <- match.arg(type)
  ns <- shiny::NS(id)
  mad <- type == "MAD"

  x_controls <- list(
    shiny::textInput(ns("start_time"), "Start time", value = 0),
    shiny::textInput(ns("end_time"), "End time", value = ""),
    shiny::numericInput(ns("x_interval"), "Time interval", value = if (mad) 6 else 12)
  )
  if (mad) {
    x_controls <- lapply(x_controls, shiny::conditionalPanel,
                         condition = "input.free_x == false", ns = ns)
  }

  shiny::div(
    class = "well",
    shiny::HTML(paste0('<ul class="help-text">
      <li>For ', type, ' data</li>
      <li>Use "Filter Data" to select data subgroups for plotting</li>
      <li>The plot shows filtered and unit-converted (if applicable) concentration</li>
      <li>Error bar: when plotting AM, SD is used; when plotting GM, geometric SD is used</li>
      <li>Use "Submit Changes" button to apply plot customization</li>
      </ul>')),
    if (mad) shiny::selectInput(ns("select_day"), "Select days to plot",
                                choices = NULL, multiple = TRUE),
    shiny::plotOutput(ns("plot")),
    shiny::br(),
    shiny::tags$hr(),
    shiny::h6("Plot customization", style = "font-weight: bold;"),
    shiny::fluidRow(lapply(x_controls, shiny::column, width = 2)),
    if (mad) shiny::fluidRow(shiny::column(4, shiny::checkboxInput(
      ns("free_x"), "Free X axis scale", value = FALSE))),
    shiny::fluidRow(
      shiny::column(4, shiny::radioButtons(ns("am_gm"), "Summary method", choices = c("AM", "GM"))),
      shiny::column(4, shiny::radioButtons(ns("error_bar"), "Error bar", choices = c("On", "Off"), selected = "Off")),
      shiny::column(4, shiny::radioButtons(ns("axis_scale"), "Y-axis scale", choices = c("Linear", "Log")))
    ),
    shiny::fluidRow(
      shiny::column(6, shiny::textInput(ns("pk_target"), "Add a reference line for PK target", placeholder = "Enter a number here")),
      shiny::column(6, shiny::textInput(ns("lloq"), "Add a reference line for LLOQ", placeholder = "Enter a number here"))
    ),
    shiny::fluidRow(
      shiny::column(6, shiny::textInput(ns("x_axis_title"), "Change X-axis title", placeholder = "Leave blank as default")),
      shiny::column(6, shiny::textInput(ns("y_axis_title"), "Change Y-axis title", placeholder = "Leave blank as default"))
    ),
    shiny::fluidRow(shiny::column(6, shiny::textInput(ns("plot_title"), "Enter plot title"))),
    shiny::actionButton(ns("submit"), "Submit Changes"),
    shiny::br(), shiny::br(),
    shiny::downloadButton(ns("download_plot"), "Download Plot", class = "btn btn-primary")
  )
}

#' @rdname mod_conc_plot
mod_conc_plot_server <- function(id, summary_data, type = c("SAD", "MAD")) {
  type <- match.arg(type)
  mad <- type == "MAD"

  shiny::moduleServer(id, function(input, output, session) {
    submitted <- shiny::reactiveValues(pk_target = NA, lloq = NA, title = "")
    shiny::observeEvent(input$submit, {
      submitted$pk_target <- parse_number_input(input$pk_target)
      submitted$lloq <- parse_number_input(input$lloq)
      submitted$title <- input$plot_title
    })

    if (mad) {
      shiny::observe({
        shiny::req(summary_data())
        days <- sort(unique(summary_data()$DAY))
        shiny::updateSelectInput(session, "select_day", choices = days,
                                 selected = intersect(shiny::isolate(input$select_day), days))
      })
    }

    # Initialise the x-axis range from the data until the user sets one
    shiny::observeEvent(summary_data(), {
      if (!nzchar(input$end_time %||% "")) {
        nmhr <- summary_data()$NMHR[summary_data()$BLQ_EXCLUDED == "N"]
        if (length(nmhr) > 0) {
          shiny::updateTextInput(session, "start_time", value = min(nmhr, na.rm = TRUE))
          shiny::updateTextInput(session, "end_time", value = max(nmhr, na.rm = TRUE))
        }
      }
    })

    plot_obj <- shiny::reactive({
      shiny::req(summary_data(), input$am_gm)
      plot_concentration_time(
        summary_data(),
        stat = input$am_gm,
        error_bar = identical(input$error_bar, "On"),
        log_y = identical(input$axis_scale, "Log"),
        x_limits = c(input$start_time, input$end_time),
        x_interval = input$x_interval,
        pk_target = submitted$pk_target,
        lloq = submitted$lloq,
        title = submitted$title,
        x_title = input$x_axis_title,
        y_title = input$y_axis_title,
        days = if (mad) (input$select_day %||% character(0)) else NULL,
        free_x = mad && isTRUE(input$free_x),
        log_breaks_from = if (mad) -2 else 0
      )
    })

    output$plot <- shiny::renderPlot(plot_obj(), res = 96)

    output$download_plot <- shiny::downloadHandler(
      filename = function() {
        title <- if (nzchar(input$plot_title %||% "")) input$plot_title else "time_concentration"
        paste0(title, "_", timestamp_digits(), ".png")
      },
      content = function(file) {
        size <- plot_size_inches(session, "plot")
        ggplot2::ggsave(file, plot_obj(), width = size$width,
                        height = size$height, device = "png")
      }
    )

    plot_obj
  })
}
