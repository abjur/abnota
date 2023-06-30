
# Testes com GPT baseado nos documentos dos processos ---------------------

# Base de dados -----------------------------------------------------------

file_list <- piggyback::pb_list(repo = "abjur/obsFase2", tag = "processos")
fs::dir_create("data-raw/html")
piggyback::pb_download(file_list$file_name, "data-raw/html/", repo = "abjur/obsFase2", tag = "processos")
zip::unzip("data-raw/html/processos.zip")

safe <- purrr::possibly(lex::tjsp_cpopg_parse)

# vamos analisar uma amostra de 100 casos
set.seed(1)
list_cpopg <- fs::dir_ls("data-raw/html/processos") |>
  sample(100) |>
  purrr::map(safe, .progress = TRUE)

da_cpopg <- list_cpopg |>
  purrr::compact() |>
  purrr::list_rbind(names_to = "file") |>
  dplyr::mutate(
    n_processo = basename(tools::file_path_sans_ext(file)),
    .before = 1
  ) |>
  dplyr::filter(!is.na(id_processo)) |>
  dplyr::mutate(movimentacoes = purrr::map(movimentacoes, \(x) {
    x |>
      dplyr::mutate(data = lubridate::dmy(data)) |>
      dplyr::arrange(data) |>
      dplyr::mutate(id_mov = seq_len(dplyr::n()), .before = 1)
  }))

readr::write_rds(da_cpopg, "data-raw/rds/p04_da_cpopg.rds")

# Base de dados: parte rápida ---------------------------------------------

devtools::load_all()
da_cpopg <- readr::read_rds("data-raw/rds/p04_da_cpopg.rds")
da_validacao <- obsFase2::da_relatorio |>
  dplyr::semi_join(da_cpopg, "n_processo")

# análise do tamanho dos documentos ---------------------------------------

dplyr::glimpse(da_cpopg)

# testes ------------------------------------------------------------------

id_processo <- 50
(processo <- da_cpopg$n_processo[id_processo])


teste <- readr::read_rds("data-raw/list_resultados.rds")

# Task 0: escopo ----------------------------------------------------------

# A partir das informações básicas do processo,
# é possível dizer se é um caso de recuperação judicial de fato?



# Task 1: deferimento -----------------------------------------------------

## TODO
## Melhorar forma de pegar concessão
## Pensar em uma forma de estruturar as chamadas em grafos
## Estudar sobre "system" e "user". Melhores práticas, diferenças

## CHAMADA 1: deferimento e homologação do plano

# apenas as primeiras 30 movs parece suficiente.
json_deferimento <- da_cpopg$movimentacoes[[id_processo]] |>
  dplyr::filter(tem_anexo) |>
  head(28) |>
  jsonlite::toJSON()

result_deferimento <- openai::create_chat_completion(
  "gpt-4",
  messages = list(
    list(
      role = "system",
      content = readr::read_file("data-raw/txt/prompt_deferimento.txt")
    ),
    list(
      role = "user",
      content = paste0(json_deferimento, "\n\nResposta:\n")
    )
  ),
  temperature = 0
)

gpt_deferimento <- result_deferimento |>
  purrr::pluck("choices", "message.content") |>
  jsonlite::fromJSON() |>
  tibble::as_tibble()

# da_cpopg$movimentacoes[[id_processo]] |>
#   dplyr::filter(id_mov <= gpt_deferimento$id_def_indef) |>
#   dplyr::count(tem_anexo)
#   with(descricao)

## CHAMADA 2: PERICIA E EMENDA

json_pericia_emenda <- da_cpopg$movimentacoes[[id_processo]] |>
  dplyr::filter(
    data <= gpt_deferimento$dt_def_indef,
    tem_anexo
  ) |>
  dplyr::arrange(data) |>
  jsonlite::toJSON()

result_pericia_emenda <- openai::create_chat_completion(
  "gpt-4",
  messages = list(
    list(
      role = "system",
      content = readr::read_file("data-raw/txt/prompt_pericia_emenda.txt")
    ),
    list(
      role = "user",
      content = paste0(json_pericia_emenda, "\n\nResposta:\n")
    )
  ),
  temperature = 0
)

gpt_pericia_emenda <- result_pericia_emenda |>
  purrr::pluck("choices", "message.content") |>
  jsonlite::fromJSON() |>
  tibble::as_tibble()

## CHAMADA 3: APROVACAO

aux_aprovacao <- da_cpopg$movimentacoes[[id_processo]] |>
  dplyr::filter(
    tem_anexo,
    data > gpt_deferimento$dt_def_indef,
    stringr::str_detect(
      paste(movimento, descricao),
      stringr::regex("aprov|homolog|conce", TRUE)
    )
  ) |>
  tail(30)

if (gpt_deferimento$deferido == "true" && nrow(aux_aprovacao) > 0) {

  json_aprovacao <- aux_aprovacao |>
    dplyr::arrange(data) |>
    jsonlite::toJSON()

  result_aprovacao <- openai::create_chat_completion(
    "gpt-4",
    messages = list(
      list(
        role = "system",
        content = readr::read_file("data-raw/txt/prompt_aprovacao.txt")
      ),
      list(
        role = "user",
        content = paste0(
          json_aprovacao,
          "\n\nResposta:\n"
        )
      )
    ),
    temperature = 0
  )

  gpt_aprovacao <- result_aprovacao |>
    purrr::pluck("choices", "message.content") |>
    jsonlite::fromJSON() |>
    tibble::as_tibble()

} else {

  gpt_aprovacao <- tibble::tibble(
    id_homologacao = "NA",
    homologado = "NA",
    dt_homologacao = "NA"
  )

}

dplyr::bind_cols(
  gpt_pericia_emenda,
  gpt_deferimento,
  gpt_aprovacao
) |>
  dplyr::glimpse()

da_validacao |>
  dplyr::filter(n_processo %in% processo) |>
  dplyr::select(
    n_processo,
    dplyr::matches("def|emend|peric|resultado_final|data_aprovacao|data_concessao")
  ) |>
  dplyr::select(
    n_processo,
    teve_emenda = emenda,
    dt_emenda = data_emenda,
    teve_pericia = pericia,
    dt_pericia = data_pericia,
    deferido,
    dt_def_indef = data_decisao_deferimento,
    resultado_final,
    data_aprovacao,
    data_concessao
  ) |>
  dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) |>
  dplyr::glimpse()


da_cpopg$movimentacoes[[id_processo]] |>
  dplyr::filter(id_mov == gpt_aprovacao$id_homologacao) |>
  with(descricao)
