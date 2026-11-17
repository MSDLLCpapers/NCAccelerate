#' Run the NCAcelerate Shiny application
#'
#' @param ... Arguments passed to [shiny::shinyApp()] (e.g. `options =
#'   list(port = 3838)`).
#' @return A `shiny.appobj`. Printing it (or calling it at the console)
#'   launches the app.
#' @export
#' @examples
#' if (interactive()) {
#'   run_app()
#' }
run_app <- function(...) {
  shiny::shinyApp(ui = app_ui, server = app_server, ...)
}
