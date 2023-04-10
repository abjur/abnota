#' Módulo de perguntas sobre o AJ
#' Informações sobre o administrador judicial
#'
#' @param id id
#'
#' @export
mod_aj_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::selectInput(
      ns("pfpj"), "O AJ é pessoa física ou jurídica?",
      choices = c("Física", "Jurídica")
    ),
    shiny::textInput(ns("nome_aj"), "Nome do AJ"),
    shiny::textInput(ns("doc_aj"), "CPF/CNPJ/OAB"),
    shiny::selectInput(ns("tipo_documento"), "Tipo de documento", choices = c(
      "CPF", "CNPJ", "OAB"
    )),
    shiny::dateInput(
      ns("data_tc_1"), "Data da primeira assinatura do termo de compromisso",
      format = "dd/mm/yyyy"
    ),
    shiny::dateInput(
      ns("data_tc_n"),
      "Data da última assinatura do termo de compromisso (se houver)",
      format = "dd/mm/yyyy"
    ),
    shiny::selectInput(
      ns("tipo_remuneracao"),
      "A remuneração do AJ é baseada em percentual ou valor fixo?",
      choices = c("Percentual", "Valor fixo", "Não foi definido")
    ),
    shiny::selectInput(
      ns("periodicidade_remuneracao"),
      "Periodicidade da remuneração do AJ",
      choices = c("Mensal", "Valor total fixado")
    ),
    shiny::numericInput(
      ns("valor_remuneracao"),
      "Qual é o valor da remuneração do AJ?",
      value = 0, min = 0
    ),
    shiny::selectInput(
      ns("provisoria_remuneracao"),
      "A remuneração do AJ é provisória ou definitiva?",
      choices = c("Provisória", "Definitiva")
    )

  )
}

#' Módulo de perguntas sobre o AJ
#' Informações sobre o administrador judicial
#'
#' @param id id
#'
#' @export
mod_aj_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {

    shiny::reactive(tibble::tibble(
      aj_pfpj = input$pfpj,
      aj_nome = input$nome_aj,
      aj_doc = input$doc_aj,
      aj_tipo_doc = input$tipo_documento,
      data_tc_1 = input$data_tc_1,
      data_tc_n = input$data_tc_n,
      aj_tipo_remuneracao = input$tipo_remuneracao,
      aj_periodicidade_remuneracao = input$periodicidade_remuneracao,
      aj_valor_remuneracao = input$valor_remuneracao,
      aj_provisoria_remuneracao = input$provisoria_remuneracao
    ))

  })
}