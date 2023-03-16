#' The application server-side
#'
#' @param input, output, session Internal parameters for {shiny}.
#'
#' @noRd
app_server <- function(input, output, session) {
  id_processo <- mod_processo_server("mod_processo1")
  mod_litisconsorcio_server("mod_litisconsorcio1")
  mod_deferimento_server("mod_deferimento1")
  mod_aj_server("mod_aj1")
  mod_credores_server("mod_credores1")
  mod_partes_server("mod_partes1", id_processo)

}