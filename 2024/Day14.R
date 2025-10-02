# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Topic : Advent of Code 2024
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Load data ---------------------------------------------------------------
source("Get_data.R")
options(scipen = 999)
input <- aoc_get_response(14, year = 2024, session_cookie = keyring::key_get("RStudio Keyring Secrets", "Advent of Code Session Cookie")) |>
  httr::content(encoding = 'UTF-8') |>
  readr::read_lines()

# Part 1 ------------------------------------------------------------------
width <- 101
height <- 103
robots_and_velocity <- do.call(rbind, lapply(strsplit(input, "(p=)|(,)|( v=)"), \(x) as.numeric(x[-1])))
robots_and_velocity[, 1:2] <- robots_and_velocity[, 1:2] + 1
seconds <- 100
bathroom <- array(0, dim = c(height, width))
for(i in seq_len(nrow(robots_and_velocity))){
  location <- robots_and_velocity[i, 2:1]
  velocity <- robots_and_velocity[i, 4:3]
  location <- location + seconds * velocity
  location <- location %% dim(bathroom)
  location <- ifelse(location == 0, dim(bathroom), location)
  bathroom[location[1], location[2]] <- bathroom[location[1], location[2]] + 1
}
safety_factor <- numeric(4)
for(i in 1:4){
  safety_factor[i] <- sum(bathroom[1:((dim(bathroom)[1] - 1) / 2), 1:((dim(bathroom)[2] - 1) / 2)])
  bathroom <- t(apply(bathroom, 2, rev))
}
cat("The safety factor after exactly", seconds, "seconds have elapsed will be", prod(safety_factor), "\n")

# Part 2 ------------------------------------------------------------------
safety_factors <- numeric(prod(dim(bathroom)))
for(seconds in 1:prod(dim(bathroom))){
  print(seconds)
  bathroom <- array(0, dim = c(height, width))
  for(i in seq_len(nrow(robots_and_velocity))){
    location <- robots_and_velocity[i, 2:1]
    velocity <- robots_and_velocity[i, 4:3]
    location <- location + seconds * velocity
    location <- location %% dim(bathroom)
    location <- ifelse(location == 0, dim(bathroom), location)
    bathroom[location[1], location[2]] <- bathroom[location[1], location[2]] + 1
  }
  safety_factor <- numeric(4)
  for(i in 1:4){
    safety_factor[i] <- sum(bathroom[1:((dim(bathroom)[1] - 1) / 2), 1:((dim(bathroom)[2] - 1) / 2)])
    bathroom <- t(apply(bathroom, 2, rev))
  }
  safety_factors[seconds] <- prod(safety_factor)
}
seconds <- which(safety_factors == min(safety_factors))[1]
bathroom <- array(0, dim = c(height, width))
for(i in seq_len(nrow(robots_and_velocity))){
  location <- robots_and_velocity[i, 2:1]
  velocity <- robots_and_velocity[i, 4:3]
  location <- location + seconds * velocity
  location <- location %% dim(bathroom)
  location <- ifelse(location == 0, dim(bathroom), location)
  bathroom[location[1], location[2]] <- bathroom[location[1], location[2]] + 1
}
apply(bathroom, 1, \(x) paste(x, collapse = ""))
cat("The Easter egg is displayed after", seconds, "seconds", "\n")