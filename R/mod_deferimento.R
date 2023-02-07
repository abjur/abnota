#' Módulo de perguntas sobre deferimento
#' Informações sobre deferimento
#'
#' @param id id
#'
#' @export
mod_deferimento_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::selectInput(
      ns("deferimento"), "O processo foi deferido para todas as empresas?",
      choices = c(
        "Sim", "Não",
        "Ainda não houve decisão de deferimento/indeferimento",
        "Houve desistência do pedido de recuperação judicial"
      )
    ),
    shiny::conditionalPanel(
      "input.deferimento == 'Sim'",
      shiny::dateInput("data_deferimento", "Data do deferimento"),
      shiny::selectInput(
        ns("visto_mp"),
        "Foi aberto visto ao Ministério Público antes do deferimento?",
        choices = c("Sim", "Não")
      ),
      shiny::selectInput(
        ns("certidao"),
        "Houve apresentação de certidão negativa de débitos?",
        choices = c("Sim", "Não", "O juiz dispensou a apresentação")
      ),
      ns = ns
    ),
    shiny::conditionalPanel(
      "input.deferimento == 'Não'",
      shiny::dateInput(ns("data_indeferimento"), "Data do indeferimento"),
      ns = ns
    ),
    shiny::conditionalPanel(
      paste(
        "input.deferimento ==",
        "'Houve desistência do pedido de recuperação judicial'"
      ),
      shiny::dateInput(ns("data_extincao"), "Data de extinção do processo"),
      ns = ns
    )

  )
}

#' Módulo de perguntas sobre deferimento
#' Informações sobre deferimento
#'
#' @param id id
#'
#' @export
mod_deferimento_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {

    ns <- session$ns

  })
}