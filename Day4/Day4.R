puzzle <- 1

connection <- file(description = "Day4Input.txt", open = "r")
word_search <- readLines(connection)
close(connection)

word_search_m <- do.call(rbind, unname(sapply(word_search, function(x) { str_split(x, "") })))
word_search_m_t <- t(sapply(1:nrow(word_search_m), function(i) { word_search_m[nrow(word_search_m) - i + 1,] }))

search_word <- function(line) {
  word <- "XMAS"
  pattern <- paste("(", word, "|", stringi::stri_reverse(word), ")", sep = "")
  return(length(pracma::refindall(paste(line, collapse = ""), pattern)))
}
search_diagonal <- function(m, i) {
  if (i == 1) {
    return(search_word(m[row(m) == col(m)]))
  } else {
    return(search_word(m[row(m) == col(m) - i + 1]) + search_word(m[row(m) == col(m) + i - 1]))
  }
}

sum(sapply(1:nrow(word_search_m), function(i) { return(search_word(word_search_m[i,]) + search_word(word_search_m[,i]) + search_diagonal(word_search_m, i) + search_diagonal(word_search_m_t, i)) }))
