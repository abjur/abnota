#' The application User-Interface
#'
#' @noRd
app_ui <- function() {

  shiny::fluidPage(
    shiny::navbarPage(
      theme = bslib::bs_theme(bootswatch = "minty"),
      title = "ABJ | Classificação",
      bslib::nav("Processo", mod_processo_ui("mod_processo1")),
      bslib::nav("Partes", mod_partes_ui("mod_partes1"))
    )
  )
}