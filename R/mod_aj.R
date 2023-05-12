#' Módulo de perguntas sobre o AJ
#' Informações sobre o administrador judicial
#'
#' @param id id
#'
#' @export
mod_aj_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    bslib::accordion(
      id = "aj",
      bslib::accordion_panel(
        "Primeiro AJ",
        shiny::selectInput(
          ns("pfpj_1"), "O AJ é pessoa física ou jurídica?",
          choices = c("Física", "Jurídica")
        ),
        shiny::textInput(ns("nome_aj_1"), "Nome do AJ"),
        shiny::selectInput(
          ns("tipo_documento_1"), "Tipo de documento",
          choices = c("CPF", "CNPJ", "OAB")
        ),
        shiny::textInput(
          ns("doc_aj_1"),
          "CNPJ/CPF/OAB (somente números; no caso de OAB 0000000UF)"
        ),
      ),
      bslib::accordion_panel(
        "Último AJ (caso haja)",
        shiny::selectInput(
          ns("pfpj_n"), "O AJ é pessoa física ou jurídica?",
          choices = c("Física", "Jurídica")
        ),
        shiny::textInput(ns("nome_aj_n"), "Nome do AJ"),
        shiny::selectInput(
          ns("tipo_documento_n"), "Tipo de documento",
          choices = c("CPF", "CNPJ", "OAB")
        ),
        shiny::textInput(
          ns("doc_aj_n"),
          "CNPJ/CPF/OAB (somente números; no caso de OAB 0000000UF)"
        ),
      ),
      bslib::accordion_panel(
        "Termo de compromisso",
        shiny::dateInput(
          ns("data_tc_1"),
          "Data da primeira assinatura do termo de compromisso",
          format = "dd/mm/yyyy"
        ),
        shiny::dateInput(
          ns("data_tc_n"),
          "Data da última assinatura do termo de compromisso (se houver)",
          format = "dd/mm/yyyy"
        )
      ),
      bslib::accordion_panel(
        "Remuneração",
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
      aj_pfpj_1 = input$pfpj_1,
      aj_nome_1 = input$nome_aj_1,
      aj_tipo_doc_1 = input$tipo_documento_1,
      aj_doc_1 = input$doc_aj_1,
      aj_pfpj_n = input$pfpj_n,
      aj_nome_n = input$nome_aj_n,
      aj_tipo_doc_n = input$tipo_documento_n,
      aj_doc_n = input$doc_aj_n,
      data_tc_1 = input$data_tc_1,
      data_tc_n = input$data_tc_n,
      aj_tipo_remuneracao = input$tipo_remuneracao,
      aj_periodicidade_remuneracao = input$periodicidade_remuneracao,
      aj_valor_remuneracao = input$valor_remuneracao,
      aj_provisoria_remuneracao = input$provisoria_remuneracao
    ))

  })
}