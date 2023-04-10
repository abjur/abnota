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
        "Data de apresentação da lista de credores do requerente",
        format = "dd/mm/yyyy"
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
        "Data de apresentação da lista de credores do AJ", format = "dd/mm/yyyy"
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

    tbl_credores <- shiny::reactive(tibble::tibble(
      credores_requerente = input$credores_requerente,
      data_listacred_requerente = ifelse(
        input$credores_requerente == "Sim", input$data_listacred_requerente, NA
      ),
      valor_requerente = ifelse(
        input$credores_requerente == "Sim", input$valor_requerente, NA
      ),
      credores_aj = input$credores_aj,
      data_listacred_aj = ifelse(
        input$credores_aj == "Sim", input$data_listacred_aj, NA
      ),
      valor_aj = ifelse(input$credores_aj == "Sim", input$valor_aj, NA)
    ))

    list(
      credores = tbl_credores,
      pdf_credores_requerente = shiny::reactive(input$lista_credores_requerente),
      pdf_credores_aj = shiny::reactive(input$lista_credores_aj)
    )

  })
}