puzzle <- 2

connection <- file(description = "Day1Input.txt", open = "r")
lists <- readLines(connection)
close(connection)

create_list <- function(lists, i) { return(sort(unname(sapply(lists, function(x) { as.integer(strsplit(x, "\\s+")[[1]][i]) })))) }
list1 <- create_list(lists, 1)
list2 <- create_list(lists, 2)

if (puzzle == 1) { sum(abs(list1 - list2)) } else { sum(sapply(list1, function(x) { x * sum(list2 == x) })) }
