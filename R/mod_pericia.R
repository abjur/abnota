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
      choices = c("Sim", "Não")
    ),
    shiny::conditionalPanel(
      "input.pericia == 'Sim'",
      shiny::dateInput(
        ns("data_pericia"), "Data do pedido de perícia prévia"
      ), ns = ns
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
