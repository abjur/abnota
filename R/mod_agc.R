#' Módulo de perguntas sobre AGC
#' Informações sobre AGC
#'
#' @param id id
#'
#' @export
mod_agc_ui <- function(id) {
  ns <- shiny::NS(id)

  perguntas <- shiny::tagList(
    shiny::selectInput(
      ns("agc_litisconsorcio"), "AGC em um litisconsórcio?",
      choices = c("Sim", "Não")
    ),
    shiny::conditionalPanel(
      "input.agc_litisconsorcio == 'Sim'",
      shiny::selectInput(
        ns("plano_uno"),
        "Houve plano único e AGC única para todas as recuperandas?",
        choices = c(
          "Sim, por determinação do juiz",
          "Sim, por determinação da AGC",
          "Houve plano único, mas grupo faliu antes da primeira AGC",
          "Houve plano único e mais de uma AGC",
          "Houve mais de um plano e AGC única",
          "Não, houve mais de um plano e mais de uma AGC"
        )
      ),
      shiny::selectInput(
        ns("faliu_antes_agc"),
        "A empresa ou grupo faliu antes da primeira AGC?",
        choices = c(
          "Sim", "Não", "Ainda não houve primeira AGC e ainda não faliu",
          "Como não houve impugnações ao plano de RJ, não houve AGC"
        )
      ),
      shiny::selectInput(
        ns("desfecho_plano"), "Desfecho do plano",
        choices = c(
          "O plano foi aprovado",
          "O plano foi reprovado (a empresa/grupo faliu)",
          "Ainda não foi aprovado nem reprovado", "Cram down"
        )
      ),
      ns = ns
    ),
    shiny::conditionalPanel(
      paste0(
        "input.desfecho_plano == 'O plano foi reprovado ",
        "(a empresa/grupo faliu)' || ",
        "input.faliu_antes_agc == 'Sim' || ",
        "input.plano_uno == 'Houve plano único, mas grupo faliu antes da ",
        "primeira AGC'"
      ),
      shiny::dateInput(
        ns("data_falencia"), "Data da falência", format = "dd/mm/yyyy"
      ),
      ns = ns
    ),
    shiny::conditionalPanel(
      "input.desfecho_plano == 'O plano foi aprovado'",
      shiny::dateInput(
        ns("data_aprovacao_plano"), "Data da aprovação do plano",
        format = "dd/mm/yyyy"
      ),
      shiny::dateInput(
        ns("data_homologacao_plano"), "Data da homologação do plano"
      ),
      shiny::selectInput(
        ns("houve_renuncia"), paste(
          "Houve renúncia, suspensão ou exoneração",
          "dos coobrigados, fiadores e avalistas?"
        ),
        choices = c("Sim", "Não")
      ),
      shiny::selectInput(
        ns("venda_upi"), "Houve previsão de venda de UPI?",
        choices = c("Sim", "Não")
      ),
      shiny::selectInput(
        ns("homologacao_credores"),
        "Houve homologação do quadro geral de credores?",
        choices = c("Sim", "Não")
      ),
      shiny::selectInput(
        ns("desfecho_rj"), "Desfecho da recuperação judicial",
        choices = c(
          "Faliu cumprindo o plano",
          "Cumprimento do plano encerrado",
          "Ainda não encerrou o cumprimento do plano"
        )
      ),
      shiny::selectInput(
        ns("prorrogacao"),
        "Houve pedido de prorrogação do período de dois anos pelo juiz?",
        choices = c("Sim", "Não")
      ),
      ns = ns
    )
  )

  tabelas <- shinydashboard::tabBox(
    shiny::tabPanel("Planilha de AGCs"),
    shiny::tabPanel("Lista de credores"),
    shiny::tabPanel(
      "Upload do plano de RJ homologado",
      shiny::fileInput(ns("plano_rj"), "Upload", accept = c(".pdf"))
    ),
    shiny::tabPanel(
      "Upload do Quadro Geral de Credores homologado",
      shiny::fileInput(ns("quadro_credores"), "Upload", accept = c(".pdf"))
    )
  )

  bslib::layout_column_wrap(
    width = "400px",
    perguntas,
    tabelas
  )

}

#' Módulo de perguntas sobre AGC
#' Informações sobre AGC
#'
#' @param id id
#'
#' @export
mod_agc_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::reactive(tibble::tibble(
      agc_litisconsorcio = input$agc_litisconsorcio,
      plano_uno = input$plano_uno,
      faliu_antes_agc = input$faliu_antes_agc,
      desfecho_plano = input$desfecho_plano,
      data_falencia = ifelse(
        input$desfecho_plano == "O plano foi reprovado (a empresa/grupo faliu)" |
          input$faliu_antes_agc == "Sim" |
          input$plano_uno == paste(
            "Houve plano único, mas grupo faliu antes da primeira AGC"
          ),
        input$data_falencia, NA
      ),
      data_aprovacao_plano = ifelse(
        input$desfecho_plano == "O plano foi aprovado",
        input$data_aprovacao_plano, NA
      ),
      data_homologacao_plano = ifelse(
        input$desfecho_plano == "O plano foi aprovado",
        input$data_homologacao_plano, NA
      ),
      houve_renuncia = input$houve_renuncia,
      venda_upi = input$venda_upi,
      homologacao_credores = input$homologacao_credores,
      desfecho_rj = input$desfecho_rj,
      prorrogacao = input$prorrogacao
    ))
  })
}
