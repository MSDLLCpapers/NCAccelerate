#' Upload module
#'
#' File inputs for the time-concentration data and the final PK parameter
#' data. Read errors are shown as notifications.
#'
#' @param id Module id.
#' @return `mod_upload_server()` returns a list of reactives:
#'   `concentration` ([read_concentration_data()] output) and `pk_parameters`
#'   ([read_pk_parameter_data()] output).
#' @name mod_upload
#' @keywords internal
mod_upload_ui <- function(id) {
  ns <- shiny::NS(id)
  accept <- c(".csv", ".xlsx", ".xls")
  shiny::tagList(
    shiny::h5("Upload Data", style = "font-weight: bold;"),
    shiny::fileInput(ns("conc_file"), "Upload Concentration Data",
                     buttonLabel = "Browse...", placeholder = "Select a file",
                     accept = accept),
    shiny::fileInput(ns("pk_file"), "Upload Final Parameters Data",
                     buttonLabel = "Browse...", placeholder = "Select a file",
                     accept = accept)
  )
}

#' @rdname mod_upload
mod_upload_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    safe_read <- function(reader, file) {
      tryCatch(
        reader(file$datapath, file_name = file$name),
        error = function(e) {
          shiny::showNotification(paste("Could not read", file$name, "-",
                                        conditionMessage(e)),
                                  type = "error", duration = NULL)
          NULL
        }
      )
    }

    concentration <- shiny::reactive({
      shiny::req(input$conc_file)
      safe_read(read_concentration_data, input$conc_file)
    })

    pk_parameters <- shiny::reactive({
      shiny::req(input$pk_file)
      safe_read(read_pk_parameter_data, input$pk_file)
    })

    list(concentration = concentration, pk_parameters = pk_parameters)
  })
}
