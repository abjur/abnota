#' info_pos_deferido
#'
#' @param proc processo
#'
#' @export
#'
info_pos_deferido <- function(proc) {

  ## AJ e TC
  proc$set_conditions(paste0("id_mov >= '", proc$gpt$dt_def_indef, "'"))
  info <- get_info(
    proc$movs, "data-raw/txt/prompt_aj.txt",
    proc$conditions
  )

  ## Aprovação -- precisa melhorar
  movs_aprovacao <- proc$movs |>
    dplyr::filter(stringr::str_detect(
      paste(movimento, descricao),
      stringr::regex("aprov|homolog|conce", TRUE)
    )) |>
    utils::tail(30)
  proc$set_conditions(paste0("id_mov >= '", proc$gpt$dt_def_indef, "'"))
  info <- get_info(
    dplyr::coalesce(movs_aprovacao, proc$movs),
    "data-raw/txt/prompt_aprovacao.txt",
    proc$conditions,
    n_movs = 15
  ) |>
    dplyr::bind_cols(info)

  ## AJ listcred --
  proc$set_conditions(paste0("id_mov > '", proc$gpt$dt_def_indef, "'"))
  info <- get_info(
        dplyr::filter(proc$movs, stringr::str_detect(movimento, stringr::regex("edital", TRUE))),
    "data-raw/txt/prompt_aj_listcred.txt",
    proc$conditions
  ) |>
    dplyr::bind_cols(info)

  ## Req listcred
  proc$set_conditions(paste0("id_mov > '", proc$gpt$dt_def_indef, "'"))
  info <- get_info(
        dplyr::filter(proc$movs, stringr::str_detect(movimento, stringr::regex("edital", TRUE))),
    "data-raw/txt/prompt_req_listcred.txt",
    proc$conditions
  ) |>
    dplyr::bind_cols(info)

  ## Stay period
  proc$set_conditions(NA)
  info <- get_info(
    proc$movs, "data-raw/txt/prompt_stay_period.txt",
    proc$conditions,
    n_movs = 20
  ) |>
    dplyr::bind_cols(info)

  ## Falência
  proc$set_conditions(paste0("id_mov > '", proc$gpt$dt_def_indef, "'"))
  info <- get_info(
    proc$movs, "data-raw/txt/prompt_falencia.txt",
    proc$conditions
  ) |>
    dplyr::bind_cols(info)

  ## AGCs
  proc$set_conditions(paste0("id_mov > '", proc$gpt$dt_def_indef, "'"))
  info <- get_info(
    proc$movs, "data-raw/txt/prompt_agcs.txt",
    proc$conditions
  ) |>
    dplyr::bind_cols(info)

  cols_lgl <- c(
    "tc", "homologado", "aj_apresentou_lista", "req_apresentou_lista",
    "stay_period_teve", "faliu"
  )
  cols_int <- c("stay_period_n", "n_agcs", "stay_period_primeiro_tempo")

  info <- info |>
    dplyr::mutate(
      dplyr::across(.fns = ~ stringr::str_replace(.x, "NA", NA_character_)),
      dplyr::across(c(dplyr::starts_with("id"), dplyr::all_of(cols_int)), as.integer),
      dplyr::across(dplyr::starts_with("dt"), lubridate::as_date),
      dplyr::across(dplyr::all_of(cols_lgl), as.logical)
    )

  proc$add_gpt_result(info)
  proc
}
