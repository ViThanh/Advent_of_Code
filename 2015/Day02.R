# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Topic : Advent of Code 2015
# Day   : 2
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Load data ---------------------------------------------------------------
source("Get_data.R")
input <- aoc_get_response(2, year = 2015, session_cookie = keyring::key_get("RStudio Keyring Secrets", "Advent of Code Session Cookie")) |>
  httr::content(encoding = 'UTF-8') |>
  readr::read_lines()

# Part 1 ------------------------------------------------------------------
presents <- t(sapply(input, \(x) as.numeric(strsplit(x, "x")[[1]])))
area <- t(apply(presents, 1, \(x) x * rep(x, length = 4)[-1]))
total_area <- 2 * sum(area) + sum(apply(area, 1, min))
cat("The elves should order", total_area, "square feet of wrapping paper")

# Part 2 ------------------------------------------------------------------
ribbon <- sum(apply(presents, 1, \(x) 2 * (sum(x) - max(x)) + prod(x)))
cat("The elves should order", ribbon, "feet of ribbon")