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
      shiny::dateInput(
        ns("data_deferimento"), "Data do deferimento", format = "dd/mm/yyyy"
      ),
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
      shiny::dateInput(
        ns("data_indeferimento"), "Data do indeferimento", format = "dd/mm/yyyy"
      ),
      ns = ns
    ),
    shiny::conditionalPanel(
      paste(
        "input.deferimento ==",
        "'Houve desistência do pedido de recuperação judicial'"
      ),
      shiny::dateInput(
        ns("data_extincao"), "Data de extinção do processo",
        format = "dd/mm/yyyy"
      ),
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

    shiny::reactive(tibble::tibble(
      deferimento = input$deferimento,
      data_deferimento = ifelse(
        input$deferimento == "Sim", input$data_deferimento, NA
      ),
      data_indeferimento = ifelse(
        input$deferimento == "Não", input$data_indeferimento, NA
      ),
      data_extincao = ifelse(
        input$deferimento == "Houve desistência do pedido de recuperação judicial",
        input$data_extincao, NA
      ),
      visto_mp = ifelse(input$deferimento == "Sim", input$visto_mp, NA),
      certidao = ifelse(input$deferimento == "Sim", input$certidao, NA)
    ))

  })
}