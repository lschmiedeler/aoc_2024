puzzle <- 2

connection <- file(description = "Day2Input.txt", open = "r")
reports <- readLines(connection)
close(connection)

check_if_report_safe <- function(report) {
  diff_report <- diff(report)
  return(sum(diff_report >= 1 & diff_report <= 3) == length(diff_report) | sum(diff_report >= -3 & diff_report <= -1) == length(diff_report))
}

sum(sapply(reports, function(report) {
  report <- as.integer(strsplit(report, "\\s+")[[1]])
  safe <- check_if_report_safe(report)
  if (puzzle == 2) { 
    i <- 1
    while (i <= length(report)) {
      if (safe) { break }
      else {
        safe <- check_if_report_safe(report[-c(i)])
        i <- i + 1
      }
    }
  }
  safe
}))
