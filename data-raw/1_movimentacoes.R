movs <- readr::read_rds("data-raw/movs_teste.rds")

con <- bq_connect()
bigrquery::dbWriteTable(
  con, "movs_teste", movs,
  append = TRUE
  # overwrite = TRUE
)
bigrquery::dbDisconnect(con)


