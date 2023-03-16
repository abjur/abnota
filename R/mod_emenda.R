#' Módulo de perguntas sobre emenda
#' Informações sobre emenda
#'
#' @param id id
#'
#' @export
mod_emenda_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::selectInput(
      ns("emenda"), "Houve emenda da petição inicial?",
      choices = c("Sim", "Não")
    ),
    shiny::conditionalPanel(
      "input.emenda == 'Sim'",
      shiny::dateInput(
        ns("data_emenda"), "Data da emenda da petição inicial"
      ), ns = ns
    )
  )
}

#' Módulo de perguntas sobre emenda
#' Informações sobre emenda
#'
#' @param id id
#'
#' @export
mod_emenda_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {

    shiny::reactive(tibble::tibble(
      houve_emenda = input$emenda,
      data_emenda = ifelse(input$emenda == "Não", NA, input$data_emenda)
    ))

  })
}

