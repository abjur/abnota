# Listagem

lista_teste <- tibble::tibble(
  id_processo = c(
    paste0(rep(0, 20), collapse = ""),
    paste0(rep(1, 20), collapse = ""),
    paste0(rep(2, 20), collapse = "")
  ),
  classificado = rep(FALSE, 3)
)

con <- bq_connect()
bigrquery::dbWriteTable(
  con, "listagem_teste", lista_teste
  # overwrite = TRUE
)
bigrquery::dbDisconnect(con)