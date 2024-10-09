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
AdventCoinFinder <- \(input, start){
  stop_while <- FALSE
  x <- 0
  while(!stop_while){
    x <- x + 1
    md5 <- digest::digest(paste0(input, x), algo = "md5", serialize = FALSE)
    stop_while <- substr(md5, 1, nchar(start)) == start
  }
  return(x)
}
cat("The lowest positive number that Santa needs is", AdventCoinFinder(input, "00000"))

# Part 2 ------------------------------------------------------------------
cat("The lowest positive number that Santa needs is", AdventCoinFinder(input, "000000"))
