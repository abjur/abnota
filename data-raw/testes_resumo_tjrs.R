cpopg <- readr::read_rds("data-raw/rds/cpopg_tjrs.rds")

# Retira movimentações que não são usadas
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

cpopg <- cpopg |>
  dplyr::filter(!stringr::str_detect(descricao, movs_rm))


# classifica um processo
set.seed(23)
id_teste_1 <- sample(cpopg$id_processo, 1)
amostra_1 <- cpopg |>
  dplyr::filter(id_processo == id_teste_1, !is.na(descricao)) |>
  dplyr::mutate(data = lubridate::dmy(data)) |>
  dplyr::arrange(data) |>
  dplyr::mutate(id_mov = dplyr::row_number())


# Resume movimentações grandes (> 8000 tokens)

resumir_tbl <- function(tbl) {
  resumido <- tbl |>
    dplyr::mutate(resumo = purrr::map(
      chunked_text,
      ~ purrr::map_vec(.x, summary_gpt)
    ))
  resumido <- resumido |>
    dplyr::mutate(
      resumo = purrr::map(resumo, purrr::discard, .p = is.na),
      resumo = purrr::map_vec(resumo, paste, collapse = ""),
      n_tokens = n_token(resumo)
    )
  resumido
}

needs_chunking <- amostra_1 |>
  dplyr::mutate(n_tokens = n_token(descricao)) |>
  dplyr::filter(n_tokens > 8000) |>
  dplyr::mutate(
    chunked_text = purrr::map_vec(
      descricao,
      ~ stringr::str_extract_all(.x, "[\\s\\S]{1,20800}")
    )
  )

if (nrow(needs_chunking) > 0) {
  i <- 1
  while (nrow(needs_chunking) > 0 && i < 3) {
    resumido <- resumir_tbl(needs_chunking)
    amostra_1 <- amostra_1 |>
      dplyr::filter(!id_mov %in% resumido$id_mov) |>
      dplyr::bind_rows()
    i <- i + 1
    needs_chunking <- resumido |>
      dplyr::filter(n_tokens > 8000) |>
      dplyr::mutate(
        chunked_text = purrr::map_vec(
          descricao,
          ~ stringr::str_extract_all(.x, "[\\s\\S]{1,20800}")
        )
      )
  }

  amostra_1 <- dplyr::bind_rows(amostra_1, resumido)
}

amostra_1 <- dplyr::arrange(amostra_1, id_mov)
if (!"resumo" %in% names(amostra_1)) {
  amostra_1 <- dplyr::mutate(amostra_1, resumo = NA)
}

# Resume todas as movimentações
resumido <- amostra_1 |>
  dplyr::mutate(resumo_final = ifelse(
    is.na(resumo),
    purrr::map_vec(descricao, summary_gpt),
    resumo
  ))

readr::write_rds(resumido, "data-raw/rds/validacao_tjrs_resumido.rds")
