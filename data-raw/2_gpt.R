# Processos listados

con <- abnota:::bq_connect()

# Processo para classificar
classificar <- con |>
  dplyr::tbl("listagem_teste") |>
  dplyr::filter(!classificado) |>
  head(1) |>
  dplyr::pull(id_processo)

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

# Movimentações
classificar_movs <- con |>
  dplyr::tbl("movs_teste") |>
  dplyr::filter(
    id_processo == classificar,
    !stringr::str_detect(movimento, movs_rm)
  ) |>
  dplyr::collect()
bigrquery::dbDisconnect(con)


# fluxo ----

processo <- abnota::new_proc(classificar, classificar_movs)

## deferimento
info_def <- abnota::get_info(
  processo$movs, "data-raw/txt/prompt_deferimento.txt"
)

## emenda e perícia
processo$set_conditions(paste0("data <= '", info_def$dt_def_indef, "'"))
info_emenda_pericia <- abnota::get_info(
  processo$movs, "data-raw/txt/prompt_pericia_emenda.txt",
  processo$conditions
)

cols_lgl <- c("deferido", "teve_emenda", "teve_pericia")

info <- dplyr::bind_cols(info_def, info_emenda_pericia) |>
  dplyr::mutate(
    dplyr::across(nome_perito, .fns = ~ stringr::str_replace(.x, "NA", NA_character_)),
    dplyr::across(dplyr::starts_with("id"), as.integer),
    dplyr::across(dplyr::starts_with("dt"), lubridate::as_date),
    dplyr::across(dplyr::all_of(cols_lgl), as.logical)
  )

processo$set_gpt_result(info)

# CASO SEJA DEFERIDO ----

if (processo$gpt$deferido) {
  processo <- abnota::info_pos_deferido(processo)
  # SE TIVER APROVAÇÃO DO PLANO
  if (processo$gpt$homologado) {
    processo <- abnota::info_pos_homologado(processo)
  }
} else {
  # info_pos_indeferido <- abnota::info_pos_indeferido(processo)
}

tbl_processo <- proc$gpt |>
  tibble::as_tibble() |>
  dplyr::mutate(id_processo = classificar, .before = 1)

abnota::upload_processo(tbl_processo)

readr::write_rds(tbl_processo, "data-raw/rds/tbl_processo.rds")

