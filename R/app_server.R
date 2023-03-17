#' The application server-side
#'
#' @param input, output, session Internal parameters for {shiny}.
#'
#' @noRd
app_server <- function(input, output, session) {
  id_processo <- mod_processo_server("mod_processo1")
  mod_credores_server("mod_credores1")
  mod_partes_server("mod_partes1", id_processo)

}