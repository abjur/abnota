#' Módulo de perguntas sobre processo
#' Escopo e informações sobre o processo
#'
#' @param id id
#'
#' @export
mod_info_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::uiOutput(ns("questoes_processo"))

  )
}

#' Módulo de perguntas sobre processo
#' Escopo e informações sobre o processo
#'
#' @param id id
#'
#' @export
mod_info_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {

    ns <- session$ns

    # Questões preliminares

    # Informações sobre o processo

    output$questoes_processo <- shiny::renderUI({
      shiny::tagList(

        shiny::dateInput(ns("data_distribuicao"), "Data de distribuição")
      )
    })

  })
}