#' Módulo de perguntas sobre litisconsorcio
#' Informações sobre litisconsorcio
#'
#' @param id id
#'
#' @export
mod_litisconsorcio_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::selectInput(
      ns("litisconsorcio"), "Litisconsórcio ativo?",
      choices = c("Sim", "Não")
    ),
    shiny::conditionalPanel(
      "input.litisconsorcio == 'Sim'",
      shiny::textInput(
        ns("litisconsorcio_nomes"), paste(
          "Nome do grupo (Se não encontrar o nome do grupo, escrever os",
          "nomes das empresas concatenadas.)"
        )
      ), ns = ns
    )
  )
}

#' Módulo de perguntas sobre litisconsorcio
#' Informações sobre litisconsorcio
#'
#' @param id id
#'
#' @export
mod_litisconsorcio_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {

    ns <- session$ns

  })
}
