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
get_info <- function(df, prompt, conditions = NA, n_movs = 10, token = Sys.getenv("OPENAI_API_TOKEN")) {
  if (!is.na(conditions)) {
    df <- df |>
      dplyr::filter(!!rlang::parse_expr(conditions)) |>
      dplyr::slice_min(data, n = n_movs)
  } else {
    df <- df |>
      dplyr::slice_min(data, n = n_movs)
  }
  json <- jsonlite::toJSON(df)
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
    temperature = 0
  )
  gpt_info <- result |>
    purrr::pluck("choices", "message.content") |>
    jsonlite::fromJSON() |>
    tibble::as_tibble()
  gpt_info
}
