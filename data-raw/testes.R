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

set.seed(2348)
validacao <- validacao |>
  dplyr::sample_n(10)

cpopg <- purrr::map(
  validacao$id_processo,
  lex::tjrs_cpopg_download,
  dir = "data-raw/cpopg-rs"
)
