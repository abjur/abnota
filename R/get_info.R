#' Get info
#'
#' get process info from movs using GPT
#'
#' @param df movs tibble
#' @param prompt path to GPT prompt
#' @param conditions conditions
#' @param n_movs lines from `df` used for prompt
#' @param token = OpenAI token
#'
#' @export
#'
get_info <- function(df, prompt, conditions = NA, n_movs = 20, token = Sys.getenv("OPENAI_API_KEY")) {
  if (!is.na(conditions)) {
    df <- dplyr::filter(df, !!rlang::parse_expr(conditions))
  }
  json <- df |>
    dplyr::slice_min(data, n = n_movs) |>
    jsonlite::toJSON()
  prompt_json <- prompt |>
    readr::read_file() |>
    paste(json)
  token_ok <- check_token(prompt_json)
  while (!token_ok && n_movs > 0) {
    message("Limite de token ultrapassado, diminuindo número de movimentações para ", n_movs - 1)
    n_movs <- n_movs - 1
    json <- df |>
      dplyr::slice_min(data, n = n_movs) |>
      jsonlite::toJSON()
    prompt_json <- prompt |>
      readr::read_file() |>
      paste(json)
    token_ok <- check_token(prompt_json)
  }

  result <- get_info_gpt(df, conditions, prompt, n_movs, token)

  gpt_info <- result |>
    purrr::pluck("choices", "message.content") |>
    jsonlite::fromJSON() |>
    tibble::as_tibble()
  gpt_info
}

get_info_gpt <- function(df, conditions, prompt, n_movs, token) {
  if (!is.na(conditions)) {
    df <- dplyr::filter(df, !!rlang::parse_expr(conditions))
  }
  json <- df |>
    dplyr::slice_min(data, n = n_movs) |>
    jsonlite::toJSON()

  result <- openai::create_chat_completion(
    "gpt-4",
    messages = list(
      list(
        role = "system",
        content = readr::read_file(prompt)
      ),
      list(
        role = "user",
        content = paste0(json, "\n\nResposta:\n")
      )
    ),
    temperature = 0,
    openai_api_key = token
  )
  result
}
