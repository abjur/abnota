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
    ),
    shiny::actionButton(ns("upload_partes"), "Upload de dados de partes")

  )
}

mod_partes_server <- function(id, input_mod_processo) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      partes <- tibble::tibble(
        id_processo = character(),
        id_parte = bit64::integer64(),
        nome_parte = character(),
        cpf_cnpj = character()
      )


      output$tbl_partes <- rhandsontable::renderRHandsontable(
        rhandsontable::rhandsontable(partes)
      )

      shiny::observeEvent(input$upload_partes, {

        id_processo <- function(x) {
          input_mod_processo()
        }

        cols <- input$tbl_partes$params$colHeaders
        partes <- input$tbl_partes$data |>
          purrr::transpose() |>
          purrr::set_names(cols) |>
          tibble::as_tibble() |>
          tidyr::unnest(
            c(id_processo, id_parte, nome_parte, cpf_cnpj)
          ) |>
          dplyr::distinct()

        con <- bq_connect()
        bigrquery::dbWriteTable(
          con, "partes",
          partes,
          append = TRUE
        )
        bigrquery::dbDisconnect(con)

        shinyWidgets::sendSweetAlert(
          title = "Upload feito",
          text = "Preencha os dados de leilões"
        )

      })


    }
  )
}
