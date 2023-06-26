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
    shiny::hr(),
    shiny::actionButton(ns("upload_partes"), "Upload de dados de partes")

  )
}

mod_partes_server <- function(id, input_mod_processo) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      shiny::observe({
        id_processo <- function(x) {
          input_mod_processo()$id_processo
        }
        partes <- tibble::tibble(
          id_processo = id_processo(),
          nome_parte = "",
          cpf_cnpj = "",
          deferiu = FALSE,
          balanco_unificado = FALSE,
          faturamento = 0,
          patrimonio_liquido = 0,
          passivo = 0,
          ativo = 0,
          passivo_tem_pl = FALSE
        )
        output$tbl_partes <- rhandsontable::renderRHandsontable(
          partes |>
            rhandsontable::rhandsontable() |>
            rhandsontable::hot_col(
              col = "id_processo", default = id_processo(), readOnly = TRUE
            )
        )
      })

      shiny::observeEvent(input$upload_partes, {

        cols <- input$tbl_partes$params$colHeaders
        partes <- input$tbl_partes$data |>
          purrr::transpose() |>
          purrr::set_names(cols) |>
          tibble::as_tibble() |>
          tidyr::unnest(
            c(id_processo, nome_parte, cpf_cnpj)
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
