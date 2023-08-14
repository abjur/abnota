paste_chunks <- function(chunks) {
  n_chunk <- length(chunks)
  pasted <- c()
  i <- 1
  j <- 1
  pasted[i] <- chunks[i]
  str <- stringr::str_length(chunks[i])
  while (i < n_chunk) {
    while (str < 800 & i < n_chunk) {
      pasted[j] <- paste(pasted[j], chunks[i + 1])
      i <- i + 1
      str <- stringr::str_length(pasted[j])
    }
    j <- j + 1
    pasted[j] <- chunks[i + 1]
    str <- stringr::str_length(chunks[i + 1])
  }
  pasted[1:j-1]
}