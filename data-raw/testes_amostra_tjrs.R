# testes com processos do obsRJRS
validacao <- obsRJRS::da_processo_tidy |>
  dplyr::transmute(
    id_processo, deferido,
    dt_def_indef = data_decisao_deferimento,
    teve_emenda = emenda_pedido_teve, dt_emenda = data_emenda,
    teve_pericia = pericia, dt_pericia = data_pericia,
    aj_nome, aj_tipo, aj_remu_tipo, aj_remu_periodicidade, aj_remu_valor,
    dt_tc = assinatura_compromisso,
    req_apresentou_lista = requerente_listcred_teve,
    req_listcred_data = requerente_listcred_data,
    req_listcred_valor = requerente_listcred_valor,
    aj_apresentou_lista, aj_listcred_data, aj_listcred_valor,
    stay_period_teve = stay_prorrogado,
    nome_perito = perito_nome, n_agcs = n_agc, dt_agc_n = data_agcn,
    agc_res_n = agc_res, faliu = resultado_final, dt_falencia = data_falencia,
    homologado = !is.na(data_aprovacao), dt_homologacao = data_aprovacao
  )

set.seed(768)
validacao <- validacao |>
  dplyr::sample_n(50)

tjrs_cpopg_download <- function(id) {
  url <- "https://services.tjrs.jus.br/gwapptjrsmobile/rest/consultaMovimentacao"
  # url <- "https://consulta.tjrs.jus.br/consulta-processual/processo/movimentos"
  comarca <- as.numeric(stringr::str_extract(id, "[0-9]{4}$"))
  id <- abjutils::clean_cnj(id)
  query <- list(
    "numeroProcesso" = id,
    "codComarca" = comarca
  )
  r <- httr::GET(
    url,
    query = query,
    httr::add_headers("Authorization" = "Basic YXBwY29uc3Byb2Nlc3N1YWw6cGozM3VTZXJ0eQ==")
  )
  r |>
    httr::content(as = "text") |>
    jsonlite::fromJSON() |>
    purrr::pluck("data") |>
    tidyr::unnest(conclusaoAoJuiz) |>
    dplyr::mutate(id_processo = id, .before = 1)
}

cpopg <- purrr::map(
  validacao$id_processo,
  # teste,
  tjrs_cpopg_download
)
cpopg <- dplyr::bind_rows(cpopg)
cpopg <- cpopg |>
  dplyr::rename(
    movimento = descricao,
    descricao = descricaoDespacho
  ) |>
  dplyr::filter(!is.na(descricao))

readr::write_rds(cpopg, "data-raw/rds/cpopg_tjrs.rds")

