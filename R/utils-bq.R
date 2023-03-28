bq_connect <- function() {

  path_json <- system.file("bq.json", package = "obsCIEE")
  bigrquery::bq_auth(path = path_json)

  con <- bigrquery::dbConnect(
    bigrquery::bigquery(),
    project = "abj-dev",
    dataset = "abjclassifica_rj",
    billing = "abj-dev"
  )

}