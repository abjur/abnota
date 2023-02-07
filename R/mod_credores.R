#' Módulo de perguntas sobre credores
#' Informações sobre credores
#'
#' @param id id
#'
#' @export
mod_credores_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::selectInput(
      ns("credores_requerente"), "A requerente apresentou lista de credores?",
      choices = c("Sim", "Não")
    ),
    shiny::conditionalPanel(
      "input.credores_requerente == 'Sim'",
      shiny::dateInput(
        ns("data_listacred_requerente"),
        "Data de apresentação da lista de credores do requerente"
      ),
      shiny::numericInput(
        ns("valor_requerente"),
        "Valor total dos créditos apresentados na lista da requerente",
        value = 0, min = 0
      ),
      shiny::fileInput(
        ns("lista_credores_requerente"),
        "Upload da lista de credores apresentada pela requerente",
        accept = c(".pdf")
      ),
      ns = ns
    ),
    shiny::selectInput(
      ns("credores_aj"), "O AJ apresentou lista de credores?",
      choices = c("Sim", "Não")
    ),
    shiny::conditionalPanel(
      "input.credores_aj == 'Sim'",
      shiny::dateInput(
        ns("data_listacred_aj"),
        "Data de apresentação da lista de credores do AJ"
      ),
      shiny::numericInput(
        ns("valor_aj"),
        "Valor total dos créditos apresentados na lista do AJ",
        value = 0, min = 0
      ),
      shiny::fileInput(
        ns("lista_credores_aj"),
        "Upload da lista de credores apresentada pelo AJ",
        accept = c(".pdf")
      ),
      ns = ns
    )
  )
}

#' Módulo de perguntas sobre credores
#' Informações sobre credores
#'
#' @param id id
#'
#' @export
mod_credores_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {

    ns <- session$ns

  })
}