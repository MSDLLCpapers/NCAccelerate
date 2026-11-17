#' PK parameter table module
#'
#' Summary table of PK parameters (GM (CV%) / median (min-max)) with optional
#' accumulation ratios and Word export.
#'
#' @param id Module id.
#' @param pk_raw Reactive returning [read_pk_parameter_data()] output.
#' @param pk_wide Reactive returning [transform_pk_parameters()] output.
#' @return `mod_pk_table_server()` returns a reactive with the summary table.
#' @name mod_pk_table
#' @keywords internal
mod_pk_table_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    class = "well",
    shiny::HTML('<ul class="help-text">
      <li>Upload final PK parameters data before proceed</li>
      <li>Column N shows the number of subjects within each group</li>
      <li>To calculate Accumulation Ratio, stratify by "Day"</li>
      </ul>'),
    shiny::selectInput(ns("select_pk_para"), "Select PK parameters to summarize",
                       choices = NULL, multiple = TRUE),
    shiny::selectInput(ns("select_stratify"), "Select variable(s) to stratify by",
                       choices = c("Analyte", "Matrix", "Panel", "Period", "Dose", "Day"),
                       multiple = TRUE),
    shiny::selectInput(ns("select_time_var"),
                       "Select time variable(s) to be reported in a 'Median (Min-Max)' format",
                       choices = NULL, multiple = TRUE),
    shiny::selectInput(ns("select_ar"),
                       "Select variable(s) to calculate Accumulation Ratio (MAD data only)",
                       choices = NULL, multiple = TRUE),
    shiny::br(),
    shiny::h5("Summary PK Parameter Table - GM (CV%)",
              style = "text-align: center; font-weight: bold;"),
    shiny::br(),
    shiny::HTML('<strong>Please be sure this box is <span style="color: red;">checked</span> before downloading.</strong>'),
    shiny::checkboxInput(ns("mask_n"), "Mask N from the table", value = TRUE),
    DT::DTOutput(ns("summary_table")),
    shiny::br(),
    shiny::HTML(paste0("<div class='footnote'>", pk_table_footnotes, "</div>", collapse = "")),
    shiny::br(),
    shiny::downloadButton(ns("download_table"), "Download Table", class = "btn btn-primary")
  )
}

#' @rdname mod_pk_table
mod_pk_table_server <- function(id, pk_raw, pk_wide) {
  shiny::moduleServer(id, function(input, output, session) {
    default_parameters <- c("AUC0-inf", "AUC0-last", "Cmax", "Tmax", "t1/2")

    shiny::observeEvent(pk_wide(), {
      choices <- pk_parameter_names(pk_wide())
      shiny::updateSelectInput(session, "select_pk_para", choices = choices,
                               selected = intersect(default_parameters, choices))
    })

    shiny::observeEvent(input$select_pk_para, {
      selected <- input$select_pk_para
      shiny::updateSelectInput(session, "select_time_var", choices = selected,
                               selected = intersect(input$select_time_var, selected))
      shiny::updateSelectInput(session, "select_ar", choices = selected,
                               selected = intersect(input$select_ar, selected))
    }, ignoreNULL = FALSE)

    summary_data <- shiny::reactive({
      shiny::req(pk_wide(), pk_raw(), input$select_pk_para, input$select_stratify)
      data <- add_accumulation_ratio(pk_wide(), input$select_ar)
      summarise_pk_parameters(
        data,
        parameters = input$select_pk_para,
        strata = input$select_stratify,
        time_vars = input$select_time_var,
        ar_parameters = input$select_ar,
        labels = pk_parameter_labels(pk_raw())
      )
    })

    output$summary_table <- DT::renderDT({
      df <- summary_data()
      options <- list(paging = FALSE, scrollX = TRUE, searchHighlight = TRUE)
      if (isTRUE(input$mask_n)) {
        options$columnDefs <- list(list(visible = FALSE,
                                        targets = match("N", names(df)) - 1))
      }
      DT::datatable(df, rownames = FALSE, options = options)
    })

    output$download_table <- shiny::downloadHandler(
      filename = function() paste0("pk_parameter_table_", timestamp_digits(), ".docx"),
      content = function(file) {
        df <- summary_data()
        if (isTRUE(input$mask_n)) df$N <- NULL
        render_table_docx(df, file,
                          title = "Summary PK Parameters - Geometric Mean (CV% Geometric Mean)",
                          footnotes = pk_table_footnotes)
      }
    )

    summary_data
  })
}
