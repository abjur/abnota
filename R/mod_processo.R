# Processo

mod_processo_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    bslib::accordion(
      id = "processo",
      bslib::accordion_panel(
        "Questões preliminares",
        shiny::textInput(ns("id_processo"), "Número do processo"),
        shiny::textInput(ns("classe"), "Classe"),
        shiny::textInput(ns("assunto"), "Assunto"),
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
          shiny::dateInput(ns("data_distribuicao"), "Data de distribuição"),
          shiny::h4("Emenda"),
          mod_emenda_ui("mod_emenda1"),
          shiny::verbatimTextOutput(ns("emenda_output"))
          # shiny::h4("Perícia"),
          # mod_pericia_ui("mod_pericia1"),
          # mod_litisconsorcio_ui("mod_litisconsorcio1")
        )
        # bslib::accordion_panel(
        #   "Deferimento",
        #   mod_deferimento_ui("mod_deferimento1")
        # ),
        # bslib::accordion_panel(
        #   "Administrador Judicial",
        #   mod_aj_ui("mod_aj1")
        # ),
        # bslib::accordion_panel(
        #   "Credores",
        #   mod_credores_ui("mod_credores1")
        # )
      )
    ),
    shiny::hr(),
    shiny::actionButton(ns("upload_processo"), "Upload de dados de processo"),
    shiny::hr(),
    shiny::verbatimTextOutput("example")
  )
}

mod_processo_server <- function(id) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      shiny::observeEvent(input$upload_processo, {

        if (input$processo_principal_rj == "Sim") {
          text <- "Preencha os dados de partes"
          processo <- tibble::tibble()
        } else {
          text <- ""
          processo <- tibble::tibble()
        }

        con <- bq_connect()
        bigrquery::dbWriteTable(
          con, "processo",
          processo,
          append = TRUE
        )
        bigrquery::dbDisconnect(con)

        shinyWidgets::sendSweetAlert(
          title = "Upload feito", type = "success", text = text
        )

      })

      teste <- mod_emenda_server("mod_emenda1")
      output$emenda_output <- shiny::renderPrint(teste$emenda())


      id_processo <- shiny::reactive(input$id_processo)
      return(id_processo)
    }
  )
}