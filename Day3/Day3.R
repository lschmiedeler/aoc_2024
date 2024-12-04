library(stringr)
library(dplyr)

puzzle <- 2

connection <- file(description = "Day3Input.txt", open = "r")
memory <- readLines(connection)
close(connection)

memory <- paste(memory, collapse = "")
if (puzzle == 2) {
  memory <- paste(str_split(memory, "(do\\(\\)|don't\\(\\))")[[1]][c(T, str_match_all(memory, "do\\(\\)|don't\\(\\)")[[1]][,1] == "do()")], collapse = "")
}

(data.frame(str_match_all(memory, "mul\\((?<X>\\d{1,3}),(?<Y>\\d{1,3})\\)")[[1]]) %>%
  select(X, Y) %>%
  mutate_if(is.character, as.numeric) %>%
  mutate(prod = X * Y) %>%
  summarize(sum = sum(prod)))$sum[1]
