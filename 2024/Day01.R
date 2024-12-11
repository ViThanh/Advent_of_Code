# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Topic : Advent of Code 2024
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Load data ---------------------------------------------------------------
source("Get_data.R")
input <- aoc_get_response(1, year = 2024, session_cookie = keyring::key_get("RStudio Keyring Secrets", "Advent of Code Session Cookie")) |>
  httr::content(encoding = 'UTF-8') |>
  readr::read_lines()

# Part 1 ------------------------------------------------------------------
locations_list <- strsplit(input, "  ")
left <- sapply(locations_list, \(x) as.numeric(x[1]))
right <- sapply(locations_list, \(x) as.numeric(x[2]))
left_sorted <- sort(left)
right_sorted <- sort(right)
cat("The total distance between the left and right lists is", sum(abs(left_sorted - right_sorted)), "\n")

# Part 2 ------------------------------------------------------------------
total_distance <- 0
for(i in left){
  total_distance <- total_distance + i * sum(right == i)
}
cat("The total distance between the left and right lists is", sum(total_distance), "\n")