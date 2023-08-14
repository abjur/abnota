amostra <- readr::read_rds("data-raw/rds/validacao_tjrs_resumido.rds")


processo <- abnota::new_proc(unique(amostra$id_processo), amostra)
n_movs <- 10
n_tokens <- processo$movs |>
  dplyr::slice_min(data, n = n_movs) |>
  jsonlite::toJSON() |>
  n_token()
while (n_movs > 1 && n_tokens > 8000) {
  n_movs <- n_movs - 1
  n_tokens <- processo$movs |>
    dplyr::slice_min(data, n = n_movs) |>
    jsonlite::toJSON() |>
    n_token()
}

info_def <- abnota::get_info(
  df = processo$movs, prompt = "data-raw/txt/prompt_deferimento.txt", n_movs = n_movs
)
info_def

obsRJRS::da_processo_tidy |>
  dplyr::filter(id_processo == unique(amostra$id_processo)) |>
  dplyr::select(dplyr::contains("def"))




classificar_gpt <- function(classificar, movs) {

  processo <- abnota::new_proc(classificar, movs)

  ## deferimento
  info_def <- abnota::get_info(
    df = processo$movs, prompt = "data-raw/txt/prompt_deferimento.txt"
  )
  cat("Deferimento OK\n")

  ## emenda e perícia
  processo$set_conditions(paste0("data <= '", info_def$dt_def_indef, "'"))
  info_emenda_pericia <- abnota::get_info(
    processo$movs, "data-raw/txt/prompt_pericia_emenda.txt",
    processo$conditions
  )
  cat("Emenda e perícia OK\n")

  cols_lgl <- c("deferido", "teve_emenda", "teve_pericia")

  info <- dplyr::bind_cols(info_def, info_emenda_pericia) |>
    dplyr::mutate(
      dplyr::across(nome_perito, .fns = ~ stringr::str_replace(.x, "NA", NA_character_)),
      dplyr::across(dplyr::starts_with("id"), as.integer),
      dplyr::across(dplyr::starts_with("dt"), lubridate::as_date),
      dplyr::across(dplyr::all_of(cols_lgl), as.logical)
    )

  processo$set_gpt_result(info)
  cat("set_gpt_result OK\n")
  print(processo$gpt)

  # CASO SEJA DEFERIDO ----

  if (processo$gpt$deferido && processo$gpt$deferido != "" && !is.na(processo$gpt$deferido)) {
    cat("## foi deferido\n")
    processo <- info_pos_deferido(processo)
    cat("info_pos_deferido OK\n")
    # SE TIVER APROVAÇÃO DO PLANO
    if (processo$gpt$homologado) {
      cat("Houve homologação\n")
      processo <- info_pos_homologado(processo)
      cat("info_pos_homologado OK\n")
    }
  } else {
    # info_pos_indeferido <- abnota::info_pos_indeferido(processo)
  }

  tbl_processo <- processo$gpt |>
    tibble::as_tibble() |>
    dplyr::mutate(id_processo = classificar, .before = 1)
  cat("=============================================\n")
  tbl_processo
}



classificado <- classificar_gpt(amostra$id_processo, amostra)



dplyr::glimpse(classificado)


validacao <- dplyr::rename_with(validacao, ~paste0(.x, "_validacao"), -id_processo)
classificado <- dplyr::rename_with(classificado, ~paste0(.x, "_gpt"), -id_processo)

# validação deferimento
validacao |>
  dplyr::select(id_processo, dplyr::matches("(dt_)?def")) |>
  dplyr::select(id_processo, sort(tidyselect::peek_vars())) |>
  dplyr::count(deferido_gpt, deferido_validacao) |>
  janitor::adorn_totals()

# validação emenda
validacao |>
  dplyr::inner_join(classificado, "id_processo") |>
  dplyr::select(id_processo, dplyr::matches("emenda")) |>
  dplyr::select(id_processo, sort(tidyselect::peek_vars()))

# validação perícia
validacao |>
  dplyr::inner_join(classificado, "id_processo") |>
  dplyr::select(id_processo, dplyr::matches("pericia")) |>
  dplyr::select(id_processo, sort(tidyselect::peek_vars()))

# validação AJ
validacao |>
  dplyr::inner_join(classificado, "id_processo") |>
  dplyr::select(id_processo, dplyr::matches("aj")) |>
  dplyr::select(id_processo, sort(tidyselect::peek_vars())) |>
  dplyr::glimpse()

# validação TC
validacao |>
  dplyr::inner_join(classificado, "id_processo") |>
  dplyr::select(id_processo, dplyr::matches("dt_tc")) |>
  dplyr::select(id_processo, sort(tidyselect::peek_vars())) |>
  dplyr::glimpse()

# validação lista requerencte
validacao |>
  dplyr::inner_join(classificado, "id_processo") |>
  dplyr::select(id_processo, dplyr::matches("req")) |>
  dplyr::select(id_processo, sort(tidyselect::peek_vars())) |>
  dplyr::glimpse()

# validação lista aj
validacao |>
  dplyr::inner_join(classificado, "id_processo") |>
  dplyr::select(id_processo, dplyr::matches("aj_(list|apresentou)")) |>
  dplyr::select(id_processo, sort(tidyselect::peek_vars())) |>
  dplyr::glimpse()

# validação AGC
validacao |>
  dplyr::inner_join(classificado, "id_processo") |>
  dplyr::select(id_processo, dplyr::matches("agc")) |>
  dplyr::select(id_processo, sort(tidyselect::peek_vars())) |>
  dplyr::glimpse()

# validação falência
validacao |>
  dplyr::inner_join(classificado, "id_processo") |>
  dplyr::select(id_processo, dplyr::matches("fal")) |>
  dplyr::select(id_processo, sort(tidyselect::peek_vars())) |>
  dplyr::glimpse()


# Teste só deferimento, perícia e emenda

teste_def_emenda_pericia <- purrr::map(unique(validacao$id_processo), classificar, movs = cpopg)
teste_def_emenda_pericia <- dplyr::bind_rows(teste_def_emenda_pericia)

# validacao <- readr::read_rds("data-raw/rds/validacao_joined.rds")

validacao |>
  dplyr::select(id_processo, dplyr::matches("(dt_)?def|emenda|pericia")) |>
  dplyr::select(id_processo, sort(tidyselect::peek_vars())) |>
  dplyr::filter(dt_def_indef_gpt != dt_def_indef_validacao) |>
  dplyr::select(id_processo, dt_def_indef_gpt, dt_def_indef_validacao, deferido_gpt, deferido_validacao) |>
  dplyr::mutate(diff = abs(as.numeric(dt_def_indef_validacao - dt_def_indef_gpt))) |>
  dplyr::arrange(dplyr::desc(diff)) |>
  knitr::kable() |>
  clipr::write_clip()
# deferido:

validacao |>
  dplyr::select(id_processo, dplyr::contains("deferido")) |>
  dplyr::mutate(deferido_validacao = dplyr::case_when(
    deferido_validacao == "Sim" ~ TRUE,
    deferido_validacao == "Não" ~ FALSE,
    deferido_validacao == "Desistência" ~ FALSE,
    TRUE ~ NA
  )) |>
  dplyr::filter(deferido_validacao != deferido_gpt)

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

movs <- readr::read_rds("data-raw/rds/cpopg_tjrs.rds")
classificar <- "00012723320158210102"
classificar_movs <- movs |>
  dplyr::filter(
    id_processo == classificar,
    !stringr::str_detect(movimento, movs_rm)
  ) |>
  dplyr::arrange(data) |>
  dplyr::mutate(id_mov = dplyr::row_number())

classificar_movs |>
  dplyr::slice_min(data) |>
  dplyr::pull(data)
# fluxo ----


teste <- tjrs_cpopg_download("00012723320158210102")
movs_classificar <- teste |>
  dplyr::mutate(data = lubridate::dmy(data)) |>
  dplyr::arrange(data) |>
  dplyr::mutate(
    movimento = descricao,
    descricao = descricaoDespacho,
    id_mov = dplyr::row_number()
  ) |>
  dplyr::filter(
    !is.na(descricaoDespacho), !stringr::str_detect(movimento, movs_rm)
  )
processo <- abnota::new_proc(classificar, movs_classificar)

## resumo
info_resumo <- abnota::get_info(
  df = processo$movs, prompt = "data-raw/txt/prompt_resumo.txt", n_movs = 5
)

## deferimento
info_def <- abnota::get_info(
  df = info_resumo, prompt = "data-raw/txt/prompt_deferimento.txt", n_movs = 1
)

