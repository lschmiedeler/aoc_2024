puzzle <- 2

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
search_diagonal_word <- function(m, i) {
  if (i == 1) { return(search_word(m[row(m) == col(m)])) } else { return(search_word(m[row(m) == col(m) - i + 1]) + search_word(m[row(m) == col(m) + i - 1])) }
}

if (puzzle == 1) {
  sum(sapply(1:nrow(word_search_m), function(i) { 
    return(search_word(word_search_m[i,]) + search_word(word_search_m[,i]) + search_diagonal_word(word_search_m, i) + search_diagonal_word(word_search_m_t, i)) 
  }))
} else {
  a_i <- which(word_search_m == "A", arr.ind = TRUE)
  sum(sapply(1:nrow(a_i), function(i) {
    x <- a_i[i,]["row"]
    y <- a_i[i,]["col"]
    if (x > 1 & x < ncol(word_search_m) & y > 1 & y < nrow(word_search_m)) {
      word_1 <- paste(word_search_m[x-1,y+1], "A", word_search_m[x+1,y-1], sep = "")
      word_2 <- paste(word_search_m[x-1,y-1], "A", word_search_m[x+1,y+1], sep = "")
      if (word_1 %in% c("SAM", "MAS") & word_2 %in% c("SAM", "MAS")) { return(1) } else { return(0) }
    } else { return(0) }
  }))
}
