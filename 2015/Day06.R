# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Topic : Advent of Code 2015
# Day   : 6
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Load data ---------------------------------------------------------------
source("Get_data.R")
input <- aoc_get_response(6, year = 2015, session_cookie = keyring::key_get("RStudio Keyring Secrets", "Advent of Code Session Cookie")) |>
  httr::content(encoding = 'UTF-8') |>
  readr::read_lines()

# Part 1 ------------------------------------------------------------------
TurnOn <- \(lights, coordinates){
  x1 <- coordinates[1]
  y1 <- coordinates[2]
  x2 <- coordinates[3]
  y2 <- coordinates[4]
  lights[(x1:x2) + 1, (y1:y2) + 1] <- 1
  lights
}
TurnOff <- \(lights, coordinates){
  x1 <- coordinates[1]
  y1 <- coordinates[2]
  x2 <- coordinates[3]
  y2 <- coordinates[4]
  lights[(x1:x2) + 1, (y1:y2) + 1] <- 0
  lights
}
Toggle <- \(lights, coordinates){
  x1 <- coordinates[1]
  y1 <- coordinates[2]
  x2 <- coordinates[3]
  y2 <- coordinates[4]
  lights[(x1:x2) + 1, (y1:y2) + 1] <- !lights[(x1:x2) + 1, (y1:y2) + 1]
  lights
}
lights <- array(0, dim = c(1000, 1000))
for(x in input){
  action <- ifelse(grepl("turn on", x), TurnOn, ifelse(grepl("turn off", x), TurnOff, Toggle))
  coordinates <- as.numeric(strsplit(gsub("(turn on )|(turn off )|(toggle )|( through)", "", x), "(,)|( )")[[1]])
  lights <- action(lights, coordinates)
}
cat(sum(lights), "are lit")

# Part 2 ------------------------------------------------------------------
TurnOn <- \(lights, coordinates){
  x1 <- coordinates[1]
  y1 <- coordinates[2]
  x2 <- coordinates[3]
  y2 <- coordinates[4]
  lights[(x1:x2) + 1, (y1:y2) + 1] <- lights[(x1:x2) + 1, (y1:y2) + 1] + 1
  lights
}
TurnOff <- \(lights, coordinates){
  x1 <- coordinates[1]
  y1 <- coordinates[2]
  x2 <- coordinates[3]
  y2 <- coordinates[4]
  lights[(x1:x2) + 1, (y1:y2) + 1] <- lights[(x1:x2) + 1, (y1:y2) + 1] - 1
  lights[(x1:x2) + 1, (y1:y2) + 1][lights[(x1:x2) + 1, (y1:y2) + 1] < 0] <- 0
  lights
}
Toggle <- \(lights, coordinates){
  TurnOn(TurnOn(lights, coordinates), coordinates)
}
lights <- array(0, dim = c(1000, 1000))
for(x in input){
  action <- ifelse(grepl("turn on", x), TurnOn, ifelse(grepl("turn off", x), TurnOff, Toggle))
  coordinates <- as.numeric(strsplit(gsub("(turn on )|(turn off )|(toggle )|( through)", "", x), "(,)|( )")[[1]])
  lights <- action(lights, coordinates)
}
cat("The total brightness of all lights is", sum(lights))