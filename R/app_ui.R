#' Path to a file installed with the package
#' @keywords internal
app_sys <- function(...) {
  system.file(..., package = "NCAcelerate", mustWork = TRUE)
}

#' Application UI
#'
#' @param request Internal parameter for bookmarking.
#' @return A Shiny UI definition.
#' @export
app_ui <- function(request) {
  shiny::fluidPage(
    theme = bslib::bs_theme(version = 4, bootswatch = "minty"),
    title = "NCAcelerate",
    shiny::tags$head(shiny::includeCSS(app_sys("app", "www", "styles.css"))),
    shiny::div(
      class = "container",
      shiny::div(class = "page-header", shiny::h1("NCAcelerate")),
      shiny::fluidRow(
        shiny::column(
          width = 3,
          shiny::br(),
          mod_upload_ui("upload"),
          shiny::tags$hr(),
          mod_filter_ui("filter")
        ),
        shiny::column(
          width = 9,
          shiny::br(),
          shiny::tabsetPanel(
            id = "main_tabs",
            shiny::tabPanel("Instructions", shiny::div(
              class = "well",
              shiny::includeHTML(app_sys("app", "www", "instructions.html"))
            )),
            shiny::tabPanel("Data Check", mod_data_check_ui("data_check")),
            shiny::tabPanel("Mean Concentration Data", mod_mean_conc_ui("mean_conc")),
            shiny::navbarMenu(
              "Time-Concentration Plot",
              shiny::tabPanel("SAD", mod_conc_plot_ui("sad_plot", type = "SAD")),
              shiny::tabPanel("MAD", mod_conc_plot_ui("mad_plot", type = "MAD"))
            ),
            shiny::tabPanel("PK Parameter Table", mod_pk_table_ui("pk_table")),
            shiny::tabPanel("Dose Proportionality", mod_dose_prop_ui("dose_prop"))
          )
        )
      )
    ),
    shiny::br()
  )
}
