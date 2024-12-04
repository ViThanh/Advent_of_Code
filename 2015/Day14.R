# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Topic : Advent of Code 2015
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Load data ---------------------------------------------------------------
source("Get_data.R")
input <- aoc_get_response(14, year = 2015, session_cookie = keyring::key_get("RStudio Keyring Secrets", "Advent of Code Session Cookie")) |>
  httr::content(encoding = 'UTF-8') |>
  readr::read_lines()

# Part 1 ------------------------------------------------------------------
reindeers <- lapply(strsplit(input, " "), \(x){
  data.frame(name = x[1], speed = as.numeric(x[4]), time = as.numeric(x[7]), rest = as.numeric(x[14]))
})
travel_time <- 2503
distance <- sapply(reindeers, \(x) (travel_time %/% (x$time + x$rest)) * (x$time * x$speed) + min(x$time, travel_time %% (x$time + x$rest)) * x$speed)
cat("The winning reindeer has travelled", max(distance), "kilometers", "\n")

# Part 2 ------------------------------------------------------------------
distance <- sapply(reindeers, \(x) cumsum(rep(rep(c(x$speed, 0), c(x$time, x$rest)), travel_time %/% (x$time + x$rest) + 1)[seq_len(travel_time)]))
points <- apply(apply(distance, 1, \(x) x == max(x)), 1, sum)
cat("The winning reindeer has", max(points), "points", "\n")
