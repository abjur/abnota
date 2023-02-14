#' The application User-Interface
#'
#' @noRd
app_ui <- function() {



  shiny::fluidPage(
    shiny::navbarPage(
      theme = bslib::bs_theme(bootswatch = "minty"),
      title = "ABJ | Classificação",
      bslib::nav(
        "Processo",
        shiny::tagList(
          bslib::accordion(
            id = "processo",
            bslib::accordion_panel(
              "Questões preliminares",
              shiny::textInput("id_processo", "Número do processo"),
              shiny::textInput("classe", "Classe"),
              shiny::textInput("assunto", "Assunto"),
              shiny::selectInput(
                "processo_principal_rj",
                "É o processo principal de uma recuperação judicial?",
                choices = c("Sim", "Não")
              )
            )
          ),
          shiny::conditionalPanel(
            "input.processo_principal_rj == 'Não'",
            bslib::accordion(
              id = "nao_rj",
              bslib::accordion_panel(
                "Assunto",
                shiny::selectInput(
                  "nao_rj", "Sobre o que é o processo?", choices = c(
                  "Habilitação de crédito", "Falência", "Impugnação de crédito",
                  "Outros"
                  )
                )
              )
            )
          ),
          shiny::conditionalPanel(
            "input.processo_principal_rj == 'Sim'",
            bslib::accordion(
              id = "sim_rj",
              bslib::accordion_panel(
                "Informações gerais",
                mod_info_ui("mod_info1"),
                shiny::h4("Emenda"),
                mod_emenda_ui("mod_emenda1"),
                shiny::h4("Perícia"),
                mod_pericia_ui("mod_pericia1"),
                mod_litisconsorcio_ui("mod_litisconsorcio1")
              ),
              bslib::accordion_panel(
                "Deferimento",
                mod_deferimento_ui("mod_deferimento1")
              ),
              bslib::accordion_panel(
                "Administrador Judicial",
                mod_aj_ui("mod_daj11")
              ),
              bslib::accordion_panel(
                "Credores",
                mod_credores_ui("mod_credores1")
              ),
            )
          )
        )
      ),
      bslib::nav("Partes", mod_partes_ui("mod_partes1"))
    )
  )
}