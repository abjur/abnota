#' The application User-Interface
#'
#' @noRd
app_ui <- function() {

  shiny::fluidPage(
    shiny::navbarPage(
      theme = bslib::bs_theme(bootswatch = "minty"),
      title = "ABJ | Classificação",
      bslib::nav_panel("Processo", mod_processo_ui("mod_processo1")),
      bslib::nav_panel("Adm Judicial", mod_aj_ui("mod_aj1")),
      bslib::nav_panel("Credores", mod_credores_ui("mod_credores1")),
      bslib::nav_panel("AGC", mod_agc_ui("mod_agc1")),
      bslib::nav_panel("Partes", mod_partes_ui("mod_partes1"))
    )
  )
}