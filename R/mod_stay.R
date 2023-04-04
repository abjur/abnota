#' Módulo de perguntas sobre stay period
#' Informações sobre stay period
#'
#' @param id id
#'
#' @export
mod_stay_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::selectInput(
      ns("stay_prorrogacao"), "O stay period foi prorrogado?",
      choices = c("Sim", "Não")
    ),
    shiny::dateInput(
      ns("stay_data_fim"), "Data do fim do stay period"
    )
  )
}

#' Módulo de perguntas sobre stay period
#' Informações sobre stay period
#'
#' @param id id
#'
#' @export
mod_stay_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {

    shiny::reactive(tibble::tibble(
      stay_prorrogacao = input$stay_prorrogacao,
      stay_data_fim = input$stay_data_fim
    ))

  })
}

