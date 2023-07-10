upload_processo <- function(processo) {
  tbl <- tibble::as_tibble(processo$gpt)
  con <- bq_connect()
  bigrquery::dbWriteTable(con, "processo", tbl, append = TRUE)
  bigrquery::dbDisconnect(con)
}