# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Topic : Advent of Code 2024
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Load data ---------------------------------------------------------------
source("Get_data.R")
input <- aoc_get_response(2, year = 2024, session_cookie = keyring::key_get("RStudio Keyring Secrets", "Advent of Code Session Cookie")) |>
  httr::content(encoding = 'UTF-8') |>
  readr::read_lines()

# Part 1 ------------------------------------------------------------------
report <- strsplit(input, " ")
safe_report <- sapply(report, \(x){
  x <- as.numeric(x)
  if(all(x == sort(x)) | all(rev(x) == sort(x))){
    x <- sort(x)
    x <- (x[-1] - x[-length(x)])
    all(x >= 1 & x <= 3)
  }else{FALSE}
})
cat(sum(safe_report), "reports are safe", "\n")

# Part 2 ------------------------------------------------------------------
safe_report <- sapply(report, \(x){
  x <- as.numeric(x)
  safe <- FALSE
  for(i in seq_along(x)){
    x_new <- x[-i]
    if(all(x_new == sort(x_new)) | all(rev(x_new) == sort(x_new))){
      x_new <- sort(x_new)
      x_new <- (x_new[-1] - x_new[-length(x_new)])
      safe <- safe | all(x_new >= 1 & x_new <= 3)
    }
  }
  safe
})
cat(sum(safe_report), "reports are safe", "\n")