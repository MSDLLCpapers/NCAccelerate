#' Application server
#'
#' Wires the modules together:
#' upload -> data check (unit conversion) -> filter -> mean concentration ->
#' SAD / MAD plots, and upload -> PK parameter table / dose proportionality.
#'
#' @param input,output,session Internal parameters for Shiny.
#' @export
app_server <- function(input, output, session) {
  uploads <- mod_upload_server("upload")

  conc_data <- mod_data_check_server("data_check", uploads$concentration)
  conc_filtered <- mod_filter_server("filter", conc_data)
  conc_summary <- mod_mean_conc_server("mean_conc", conc_filtered)
  mod_conc_plot_server("sad_plot", conc_summary, type = "SAD")
  mod_conc_plot_server("mad_plot", conc_summary, type = "MAD")

  pk_wide <- shiny::reactive({
    shiny::req(uploads$pk_parameters())
    transform_pk_parameters(uploads$pk_parameters())
  })
  mod_pk_table_server("pk_table", uploads$pk_parameters, pk_wide)
  mod_dose_prop_server("dose_prop", pk_wide)
}
