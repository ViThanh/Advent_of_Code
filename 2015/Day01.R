# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Topic : Advent of Code 2015
# Day   : 1
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Load data ---------------------------------------------------------------
source("Get_data.R")
input <- aoc_get_response(1, year = 2015, session_cookie = keyring::key_get("RStudio Keyring Secrets", "Advent of Code Session Cookie")) |>
  httr::content(encoding = 'UTF-8')

# Part 1 ------------------------------------------------------------------
apartment_building <- strsplit(input, "")[[1]]
up_and_down <- table(apartment_building)
floor <- -diff(up_and_down, lag = 1)
cat("The instructions take Santa to the", floor, "floor")

# Part 2 ------------------------------------------------------------------
apartment_building <- ifelse(apartment_building == "(", 1, -1)
floor <- cumsum(apartment_building)
basement <- which(floor < 0)[1]
cat("Santa first enters the basement at position", basement)