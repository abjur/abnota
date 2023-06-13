# Processo

mod_processo_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    bslib::accordion(
      id = "processo",
      bslib::accordion_panel(
        "Questões preliminares",
        shinycssloaders::withSpinner(
          shiny::uiOutput(ns("id_processo_ui")),
          caption = "Pegando processo de lista"
        ),
        shiny::selectInput(
          ns("processo_principal_rj"),
          "É o processo principal de uma recuperação judicial?",
          choices = c("Sim", "Não")
        )
      )
    ),
    shiny::conditionalPanel(
      paste0("input['", ns("processo_principal_rj"), "'] == 'Não'"),
      bslib::accordion(
        id = "nao_rj",
        bslib::accordion_panel(
          "Assunto",
          shiny::selectInput(
            ns("nao_rj"), "Sobre o que é o processo?", choices = c(
            "Habilitação de crédito", "Falência", "Impugnação de crédito",
            "Outros"
            )
          )
        )
      )
    ),
    shiny::conditionalPanel(
      paste0("input['", ns("processo_principal_rj"), "'] == 'Sim'"),
      bslib::accordion(
        id = "sim_rj",
        bslib::accordion_panel(
          "Informações gerais",
          shiny::textInput(ns("classe"), "Classe"),
          shiny::textInput(ns("assunto"), "Assunto"),
          shiny::dateInput(
            ns("data_distribuicao"), "Data de distribuição",
            format = "dd/mm/yyyy"
          ),
          shiny::h4("Emenda"),
          mod_emenda_ui(ns("mod_emenda1")),
          shiny::h4("Perícia"),
          mod_pericia_ui(ns("mod_pericia1")),
          mod_litisconsorcio_ui(ns("mod_litisconsorcio1"))
        ),
        bslib::accordion_panel(
          "Deferimento",
          mod_deferimento_ui(ns("mod_deferimento1"))
        ),
        bslib::accordion_panel(
          "Stay period",
          mod_stay_ui(ns("mod_stay1"))
        )
      )
    ),
    shiny::hr(),
    shiny::actionButton(ns("upload_processo"), "Upload de dados de processo"),
    shiny::hr(),
    shiny::dataTableOutput(ns("teste"))
  )
}

mod_processo_server <- function(id) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      output$id_processo_ui <- shiny::renderUI({
        con <- bq_connect()
        id_anotar <- con |>
          dplyr::tbl("listagem_teste") |>
          dplyr::filter(!classificado) |>
          head(1) |>
          dplyr::pull(id_processo)
        bigrquery::dbDisconnect(con)
        shiny::textInput(
          ns("id_processo"), "Número do processo",
          value = id_anotar
        )
      })


      shiny::observeEvent(input$upload_processo, {

        check_id <- verificar_processo(input$id_processo)
        if (!check_id) {
          shinyWidgets::sendSweetAlert(
            title = "ID inválido", text = "Verifique o número do processo",
            type = "error"
          )
        }

        if (check_id) {
          if (input$processo_principal_rj == "Sim") {

            text <- "Preencha os dados de partes"

            emenda <- mod_emenda_server("mod_emenda1")
            pericia <- mod_pericia_server("mod_pericia1")
            litisconsorcio <- mod_litisconsorcio_server("mod_litisconsorcio1")
            deferimento <- mod_deferimento_server("mod_deferimento1")
            aj <- mod_aj_server("mod_aj1")
            credores <- mod_credores_server("mod_credores1")
            stay <- mod_stay_server("mod_stay1")

            tbl_processo <- dplyr::bind_cols(
              emenda(), pericia(), litisconsorcio(), deferimento(), stay()
            ) |>
              dplyr::mutate(
                id_processo = input$id_processo,
                classe = input$classe,
                assunto = ifelse(
                  input$processo_principal_rj == "Sim", input$assunto,
                  input$nao_rj
                ),
                processo_principal_rj = input$processo_principal_rj
              ) |>
              dplyr::relocate(
                id_processo, classe, assunto, processo_principal_rj
              ) |>
              dplyr::mutate(dplyr::across(
                dplyr::starts_with("data"),
                ~as.character(as.Date(.x, origin = "1970-01-01"))
              ))

            # upload pdfs no drive

            obsutils::autenticar_gsheets()
            url_cred_requerente <- credores$pdf_credores_requerente() |>
              dplyr::pull(datapath) |>
              googledrive::drive_upload(
                path = googledrive::as_dribble("observatorios/teste"),
                name = paste0(input$id_processo, "_cred_requerente.pdf")
              ) |>
              dplyr::pull(id) |>
              paste0("https://drive.google.com/file/d/", ... = _)
            url_cred_aj <- credores$pdf_credores_aj() |>
              dplyr::pull(datapath) |>
              googledrive::drive_upload(
                path = googledrive::as_dribble("observatorios/teste"),
                name = paste0(input$id_processo, "_cred_aj.pdf")
              ) |>
              dplyr::pull(id) |>
              paste0("https://drive.google.com/file/d/", ... = _)

            # upload tabela de credores

            credores <- credores$credores() |>
              dplyr::mutate(
                dplyr::across(
                  dplyr::starts_with("data"),
                  ~as.character(as.Date(.x, origin = "1970-01-01"))
                ),
                id_processo = input$id_processo,
                lista_credores_requerente = url_cred_requerente,
                lista_credores_aj = url_cred_aj,
              )

            con <- bq_connect()
            bigrquery::dbWriteTable(
              con, "processo",
              tbl_processo,
              append = TRUE
            )
            bigrquery::dbWriteTable(
              con, "credores",
              credores,
              append = TRUE
            )
            bigrquery::dbDisconnect(con)


          } else {

            text <- ""
            tbl_processo <- tibble::tibble()

          }
        }



        # con <- bq_connect()
        # bigrquery::dbWriteTable(
        #   con, "processo",
        #   processo,
        #   append = TRUE
        # )
        # bigrquery::dbDisconnect(con)

        shinyWidgets::sendSweetAlert(
          title = "Upload feito", type = "success", text = text
        )

      })

      id_processo <- shiny::reactive(input$id_processo)
      return(id_processo)
    }
  )
}