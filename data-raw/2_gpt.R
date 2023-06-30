# Deferimento

con <- abnota:::bq_connect()
classificar <- con |>
  dplyr::tbl("listagem_teste") |>
  dplyr::filter(!classificado) |>
  head(1) |>
  dplyr::pull(id_processo)
classificar_movs <- con |>
  dplyr::tbl("movs_teste") |>
  dplyr::filter(id_processo == teste_processo) |>
  head(30) |>
  dplyr::collect()
bigrquery::dbDisconnect(con)


# funções ----

new_proc <- function(...) {
  processo <- R6::R6Class(
    "processo",
    public = list(
      id = NULL,
      movs = NULL,
      gpt = NULL,
      conditions = NULL,
      initialize = function(id = NA, movs = NA, gpt_result = NA, conditions = NA) {
        self$id <- id
        self$movs <- movs
        self$gpt <- gpt_result
        self$conditions <- conditions
        self$show()
      },
      set_gpt_result = function(val) {
        self$gpt <- val
      },
      add_gpt_result = function(val) {
        self$gpt <- c(self$gpt, val)
      },
      set_conditions = function(val) {
        self$conditions <- val
      },
      add_conditions = function(val) {
        self$conditions <- paste(self$conditions, val, sep = " & ")
      },
      show = function() {
        cat(paste0("Processo ", self$id, "\n"))
        print(head(self$movs))
        print(self$gpt)
      },
      show_conditions = function() {
        cat(self$conditions)
      }
    )
  )
  processo$new(...)
}

get_info <- function(id, df, prompt, conditions = NA, n_movs = 10) {
  if (!is.na(conditions)) {
    df <- df |>
      dplyr::filter(!!rlang::parse_expr(conditions)) |>
      dplyr::slice_min(data, n = n_movs)
  } else {
    df <- df |>
      dplyr::slice_min(data, n = n_movs)
  }
  json <- jsonlite::toJSON(df)
  result <- openai::create_chat_completion(
    "gpt-4",
    messages = list(
      list(
        role = "system",
        content = readr::read_file(prompt)
      ),
      list(
        role = "user",
        content = paste0(json, "\n\nResposta:\n")
      )
    ),
    temperature = 0
  )
  gpt_info <- result |>
    purrr::pluck("choices", "message.content") |>
    jsonlite::fromJSON() |>
    tibble::as_tibble()
  gpt_info
}


upload_processo <- function(processo) {
  tbl <- tibble::as_tibble(processo$gpt)
  con <- bq_connect()
  bigrquery::dbWriteTable(con, "processo", tbl, append = TRUE)
  bigrquery::dbDisconnect(con)
}



# fluxo ----

cpopg <- readr::read_rds("data-raw/rds/p04_da_cpopg.rds")
cpopg <- obsFase2::da_relatorio |>
  dplyr::inner_join(cpopg, "n_processo")

cpopg |>
  dplyr::filter(!is.na(tipo_remuneracao_arrumado)) |>
  dplyr::sample_n(1) |>
  dplyr::glimpse()

# Movimentações que podem ser retiradas

movs_rm <- c(
  "(Ato ordinat[oó]rio)",
  "(Cartas)",
  "(Certid[aã]o)",
  "(Embargo)",
  "(Expedi[cç][aã]o de documento)",
  "(Mandado)",
  "(Of[ií]cio Expedido)",
  "(Termo Expedido)"
) |>
  paste0(collapse = "|") |>
  stringr::regex(ignore_case = TRUE)


teste_processo <- "00012132220158260291"
teste_movs <- "data-raw/rds/p04_da_cpopg.rds" |>
  readr::read_rds() |>
  dplyr::filter(n_processo == teste_processo) |>
  dplyr::transmute(id_processo = n_processo, movimentacoes) |>
  tidyr::unnest(movimentacoes) |>
  dplyr::filter(tem_anexo, !stringr::str_detect(movimento, movs_rm))


teste_r6 <- new_proc(id = teste_processo, movs = teste_movs)
# exemplo <- readr::read_rds("data-raw/list_resultados.rds")

## deferimento
info <- get_info(
  teste_r6$id, teste_r6$movs, "data-raw/txt/prompt_deferimento.txt",
  teste_r6$conditions
)
teste_r6$set_gpt_result(info)
teste_r6$gpt

## emenda e perícia
teste_r6$set_conditions(paste0("data <= '", teste_r6$gpt$dt_def_indef, "'"))
info <- get_info(
  teste_r6$id, teste_r6$movs, "data-raw/txt/prompt_pericia_emenda.txt",
  teste_r6$conditions
)
teste_r6$add_gpt_result(info)
teste_r6$gpt


# CASO SEJA DEFERIDO ----

## AJ e TC
teste_r6$set_conditions(paste0("id_mov >= '", teste_r6$gpt$dt_def_indef, "'"))
info <- get_info(
  teste_r6$id, teste_r6$movs, "data-raw/txt/prompt_aj.txt",
  teste_r6$conditions
)
teste_r6$add_gpt_result(info)

## Aprovação -- precisa melhorar

teste_r6$set_conditions(paste0("id_mov >= '", teste_r6$gpt$dt_def_indef, "'"))
info <- get_info(
  teste_r6$id, teste_r6$movs, "data-raw/txt/prompt_aprovacao.txt",
  teste_r6$conditions, n_movs = 15
)
teste_r6$add_gpt_result(info)

## AJ listcred --

teste_r6$set_conditions(paste0(
  "id_mov > '", teste_r6$gpt$dt_def_indef, "'"
))
info <- get_info(
  teste_r6$id,
  dplyr::filter(teste_r6$movs, stringr::str_detect(movimento, stringr::regex("edital", TRUE))),
  "data-raw/txt/prompt_aj_listcred.txt",
  teste_r6$conditions
)
teste_r6$add_gpt_result(info)

## Req listcred

teste_r6$set_conditions(paste0(
  "id_mov > '", teste_r6$gpt$dt_def_indef, "'"
))
info <- get_info(
  teste_r6$id,
  dplyr::filter(teste_r6$movs, stringr::str_detect(movimento, stringr::regex("edital", TRUE))),
  "data-raw/txt/prompt_req_listcred.txt",
  teste_r6$conditions
)
teste_r6$add_gpt_result(info)

## Stay period
teste_r6$set_conditions(NA)
info <- get_info(
  teste_r6$id, teste_r6$movs, "data-raw/txt/prompt_stay_period.txt",
  teste_r6$conditions, n_movs = 20
)
teste_r6$add_gpt_result(info)

## Falência
teste_r6$set_conditions(paste0(
  "id_mov > '", teste_r6$gpt$dt_def_indef, "'"
))
info <- get_info(
  teste_r6$id, teste_r6$movs, "data-raw/txt/prompt_falencia.txt",
  teste_r6$conditions
)
teste_r6$add_gpt_result(info)

## AGCs
teste_r6$set_conditions(paste0(
  "id_mov > '", teste_r6$gpt$dt_def_indef, "'"
))
info <- get_info(
  teste_r6$id, teste_r6$movs, "data-raw/txt/prompt_agcs.txt",
  teste_r6$conditions
)
teste_r6$add_gpt_result(info)



# SE TIVER APROVAÇÃO DO PLANO ----

## Cram down
teste_r6$set_conditions(paste0(
  "id_mov == '", teste_r6$gpt$dt_homologacao, "'"
))
info <- get_info(
  teste_r6$id, teste_r6$movs, "data-raw/txt/prompt_cram_down.txt",
  teste_r6$conditions
)
teste_r6$add_gpt_result(info)

cols_lgl <- c(
  "deferido", "teve_emenda", "teve_pericia", "tc", "homologado",
  "aj_apresentou_lista", "req_apresentou_lista", "stay_period_teve", "faliu",
  "cram_down"
)

cols_int <- c("stay_period_n", "n_agcs")

tbl_processo <- tibble::as_tibble(teste_r6$gpt) |>
  dplyr::mutate(
    dplyr::across(.fns = ~stringr::str_replace(.x, "NA", NA_character_)),
    dplyr::across(c(dplyr::starts_with("id"), dplyr::all_of(cols_int)), as.integer),
    dplyr::across(dplyr::starts_with("dt"), lubridate::as_date),
    dplyr::across(dplyr::all_of(cols_lgl), as.logical)
  )

readr::write_rds(tbl_processo, "data-raw/rds/tbl_processo.rds")

