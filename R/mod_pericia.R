#' Módulo de perguntas sobre perícia
#' Informações sobre perícia
#'
#' @param id id
#'
#' @export
mod_pericia_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::selectInput(
      ns("pericia"), "Houve pedido de perícia prévia?",
      choices = c("Sim", "Não"), selected = "Não"
    ),
    shiny::conditionalPanel(
      "input.pericia == 'Sim'",
      shiny::dateInput(
        ns("data_pericia_pedido"), "Data do pedido de perícia prévia",
        format = "dd/mm/yyyy"
      ),
      shiny::dateInput(
        ns("data_pericia"), "Data da perícia prévia",
        format = "dd/mm/yyyy"
      ),
      shiny::textInput(ns("nome_perito"), "Nome do perito"),
      shiny::selectInput(
        ns("tipo_doc_perito"), "Tipo de documento do perito",
        choices = c("CNPJ", "CPF", "OAB")
      ),
      shiny::textInput(
        ns("doc_perito"),
        "CNPJ/CPF/OAB do perito (somente números; no caso de OAB 0000000UF)"
      ),
      shiny::selectInput(
        ns("parecer_perito"), "Parecer do perito",
        choices = c(
          "Deferimento da RJ", "Indeferimento da RJ", "Emenda inicial do pedido"
        )
      ),
      ns = ns
    )
  )
}

#' Módulo de perguntas sobre perícia
#' Informações sobre perícia
#'
#' @param id id
#'
#' @export
mod_pericia_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {

    shiny::reactive(tibble::tibble(
      houve_pericia = input$pericia,
      data_pericia = ifelse(input$pericia == "Não", NA, input$data_pericia)
    ))

  })
}
