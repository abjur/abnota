verificar_processo <- function(x) {
  stringr::str_detect(
    x, "[0-9]{3,7}-?[0-9]{2}\\.?[0-9]{4}\\.?[0-9]{1}\\.?[0-9]{2}\\.?[0-9]{4}"
  )
}
