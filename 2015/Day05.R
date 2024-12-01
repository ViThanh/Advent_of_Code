# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Topic : Advent of Code 2015
# Day   : 5
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Load data ---------------------------------------------------------------
source("Get_data.R")
input <- aoc_get_response(5, year = 2015, session_cookie = keyring::key_get("RStudio Keyring Secrets", "Advent of Code Session Cookie")) |>
  httr::content(encoding = 'UTF-8') |>
  readr::read_lines()

# Part 1 ------------------------------------------------------------------
nice <- sapply(input, \(x){
  vowels <- stringr::str_count(x, pattern = "[aeiou]") >= 3
  double <- strsplit(x, "")[[1]]
  double <- any(double[-1] == double[-length(double)])
  specific <- grepl("(ab)|(cd)|(pq)|(xy)", x)
  vowels & double & (!specific)
})
cat(sum(nice), "strings are nice")

# Part 2 ------------------------------------------------------------------
nice <- sapply(input, \(x){
  split <- strsplit(x, "")[[1]]
  double <- any(split[-(1:2)] == split[-(length(split) - (0:1))])
  pairs <- paste0(split[-length(split)], split[-1])
  pairs <- any(sapply(pairs, \(y){
    positions <- which(pairs == y)
    (length(positions) > 1) & any(diff(positions) > 1)
  }))
  double & pairs
})
cat(sum(nice), "strings are nice")