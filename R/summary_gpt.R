summary_gpt <- function(txt, prompt = "data-raw/txt/prompt_resumo.txt", token = Sys.getenv("OPENAI_API_KEY")) {
  prompt_ascii <- prompt |>
    readr::read_file() |>
    stringi::stri_trans_general("Latin-ASCII")
  txt_ascii <- stringi::stri_trans_general(txt, "Latin-ASCII")

  n_tokens <- prompt_ascii |>
    paste(txt_ascii) |>
    n_token()

  if (n_tokens < 8000) {
    result <- openai::create_chat_completion(
      "gpt-4",
      messages = list(
        list(
          role = "system",
          content = prompt_ascii
        ),
        list(
          role = "user",
          content = paste0(txt_ascii, "\n\nResposta:\n")
        )
      ),
      temperature = 0,
      openai_api_key = token
    )
    res <- purrr::pluck(result, "choices", "message.content")
  } else {
    res <- NA
  }
  res
}
