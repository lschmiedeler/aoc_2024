puzzle <- 1

connection <- file(description = "Day5Input.txt", open = "r")
pages <- readLines(connection)
close(connection)

split <- which(pages == "")
rules <- pages[1:(split - 1)]
updates <- pages[(split + 1):length(pages)]

rules_m <- unname(t(sapply(rules, function(r) { as.integer(str_split(r, "\\|")[[1]]) })))
unique_values_1 <- unique(rules_m[,1])
names(unique_values_1) <- unique_values_1

rules_l <- lapply(unique_values_1, function(r) {
  values_2 <- rules_m[which(rules_m[,1] == r),]
  if (class(values_2)[1] == "integer") { c(values_2[2]) } else { values_2[,2] }
})

sum(unname(sapply(updates, function(u) {
  new_update <- as.integer(str_split(u, ",")[[1]])
  i <- 2
  while (i <= length(new_update)) {
    test <- new_update[1:i - 1]
    rule_values <- rules_l[[as.character(new_update[i])]]
    if (sum(test %in% rule_values) > 0) { return(0) }
    i <- i + 1
  }
  return(new_update[1 + length(new_update) / 2])
})))
