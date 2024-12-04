# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Topic : Advent of Code 2015
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Load data ---------------------------------------------------------------
source("Get_data.R")
input <- aoc_get_response(13, year = 2015, session_cookie = keyring::key_get("RStudio Keyring Secrets", "Advent of Code Session Cookie")) |>
  httr::content(encoding = 'UTF-8') |>
  readr::read_lines()

# Part 1 ------------------------------------------------------------------
happiness_change <- do.call(rbind, lapply(strsplit(input, " "), \(x){
  data.frame(x[1], sub(".", "", x[length(x)], fixed = TRUE), ifelse(x[3] == "gain", as.numeric(x[4]), -as.numeric(x[4])))
}))
colnames(happiness_change) <- c("guest_1", "guest_2", "happiness_change")
guests <- unique(happiness_change[, 1])
seatings <- combinat::permn(guests)
total_happiness <- sapply(seatings, \(x){
  arrangement <- c(x[length(x)], x, x[1])
  happiness <- 0
  for(i in seq_len(length(x)) + 1){
    happiness <- happiness + happiness_change[, 3][happiness_change[,1] == arrangement[i] & happiness_change[,2] == arrangement[i + 1]] +
      happiness_change[, 3][happiness_change[,1] == arrangement[i] & happiness_change[,2] == arrangement[i - 1]]
  }
  happiness
})
cat("The total change in happiness for the optimal seating arrangement is", max(total_happiness), "\n")

# Part 2 ------------------------------------------------------------------
happiness_change <- rbind(happiness_change, data.frame(guest_1 = "me", guest_2 = guests, happiness_change = 0), data.frame(guest_1 = guests, guest_2 = "me", happiness_change = 0))
guests <- unique(happiness_change[, 1])
seatings <- combinat::permn(guests)
total_happiness <- sapply(seatings, \(x){
  arrangement <- c(x[length(x)], x, x[1])
  happiness <- 0
  for(i in seq_len(length(x)) + 1){
    happiness <- happiness + happiness_change[, 3][happiness_change[,1] == arrangement[i] & happiness_change[,2] == arrangement[i + 1]] +
      happiness_change[, 3][happiness_change[,1] == arrangement[i] & happiness_change[,2] == arrangement[i - 1]]
  }
  happiness
})
cat("The total change in happiness for the optimal seating arrangement is", max(total_happiness), "\n")