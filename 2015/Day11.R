# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Topic : Advent of Code 2015
# Day   : 11
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Load data ---------------------------------------------------------------
source("Get_data.R")
input <- aoc_get_response(11, year = 2015, session_cookie = keyring::key_get("RStudio Keyring Secrets", "Advent of Code Session Cookie")) |>
  httr::content(encoding = 'UTF-8') |>
  readr::read_lines()

# Part 1 ------------------------------------------------------------------
NextPassword <- \(password){
  password <- as.numeric(factor(strsplit(password, "")[[1]], levels = letters))
  forbidden <- as.numeric(factor(c("i", "o", "l"), levels = letters))
  approved <- FALSE
  while(!approved){
    password[8] <- password[8] + 1
    for(i in 8:1){
      if(password[i] > 26){
        password[i] <- 1
        password[i - 1] <- password[i - 1] + 1
      }else if(password[i] %in% forbidden){
        password[i] <- password[i] + 1
      }
    }
    if(stringr::str_count(paste(as.numeric(diff(password) == 1), collapse = ""), pattern = "11") > 0) approved <- TRUE
    if(all(!sapply(forbidden, \(x) x %in% password))) approved <- approved & TRUE else approved <- FALSE
    idx <- diff(password) == 0
    if(sum(idx) > 1){
      pasted_idx <- paste(as.numeric(idx), collapse = "")
      if((sum(idx) - stringr::str_count(pasted_idx, pattern = "11")) > 1){
        approved <- approved & length(unique(password[c(FALSE, idx)])) > 1
      }else{
        approved <- FALSE
      }
    }else{
      approved <- FALSE
    }
  }
  return(paste(factor(password, levels = 1:26, labels = letters), collapse = ""))
}
password <- NextPassword(input)
cat("Santa's next password is", password, "\n")

# Part 2 ------------------------------------------------------------------
cat("Santa's next password is", NextPassword(password), "\n")