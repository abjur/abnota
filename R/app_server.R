#' The application server-side
#'
#' @param input, output, session Internal parameters for {shiny}.
#'
#' @noRd
app_server <- function(input, output, session) {
  mod_info_server("mod_info1")
  mod_emenda_server("mod_emenda1")
  mod_pericia_server("mod_pericia1")
  mod_litisconsorcio_server("mod_litisconsorcio1")
  mod_deferimento_server("mod_deferimento1")
  mod_aj_server("mod_aj1")
  mod_credores_server("mod_credores1")
  mod_partes_server("mod_partes1")


}