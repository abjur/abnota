check_token <- function(prompt) {
  prompt |>
    stringr::str_length() |>
    (`/`)(2.6) |>
    (`<`)(8000)
}

n_token <- function(prompt) {
  prompt |>
    stringr::str_length() |>
    (`/`)(2.6)
}
