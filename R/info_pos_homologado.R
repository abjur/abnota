#' info_pos_homologado
#'
#' @param proc processo
#'
#' @export
#'
info_pos_homologado <- function(proc) {
  ## Cram down
  proc$set_conditions(paste0(
    "id_mov == '", proc$gpt$dt_homologacao, "'"
  ))
  info <- get_info(
    proc$movs, "data-raw/txt/prompt_cram_down.txt", proc$conditions
  ) |>
    dplyr::mutate(
      id_cram_down = as.integer(id_cram_down),
      cram_down = as.logical(cram_down)
    )
  proc$add_gpt_result(info)
  proc
}
