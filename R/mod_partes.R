# Partes

mod_partes_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::div(paste(
      "Clique na tabela com o botão direito e selecione [Insert row below]",
      "para adicionar uma nova linha."
    )),
    shiny::hr(),
    shiny::div(
      rhandsontable::rHandsontableOutput(ns("tbl_partes"))
    )


  )
}

mod_partes_server <- function(id) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      partes <- tibble::tibble(
        cpf_cnpj = character(),
        nome_parte = character()
      )


      output$tbl_partes <- rhandsontable::renderRHandsontable(
        rhandsontable::rhandsontable(partes)
      )


    }
  )
}