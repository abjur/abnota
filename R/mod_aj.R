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
    shiny::textInput(ns("cpf_cnpj"), "CPF ou CNPJ"),
    shiny::dateInput(
      ns("data_tc"), "Data de assinatura do termo de compromisso"
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
      aj_cpf_cnpj = input$cpf_cnpj,
      data_tc = input$data_tc,
      aj_tipo_remuneracao = input$tipo_remuneracao,
      aj_periodicidade_remuneracao = input$periodicidade_remuneracao,
      aj_valor_remuneracao = input$valor_remuneracao,
      aj_provisoria_remuneracao = input$provisoria_remuneracao
    ))

  })
}