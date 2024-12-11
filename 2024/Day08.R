# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Topic : Advent of Code 2024
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Load data ---------------------------------------------------------------
source("Get_data.R")
input <- aoc_get_response(8, year = 2024, session_cookie = keyring::key_get("RStudio Keyring Secrets", "Advent of Code Session Cookie")) |>
  httr::content(encoding = 'UTF-8') |>
  readr::read_lines()

# Part 1 ------------------------------------------------------------------
map <- do.call(rbind, strsplit(input, ""))
dims <- dim(map)
unique_antennas <- setdiff(unique(as.character(map)), ".")
antinodes <- array("", dim = dim(map))
for(i in unique_antennas){
  positions <- which(map == i, arr.ind = TRUE)
  for(j in seq_len(nrow(positions) - 1)){
    for(k in seq_len(nrow(positions) - j) + j){
      a <- positions[j,]
      b <- positions[k,]
      A <- 2 * a - b
      B <- 2 * b - a
      if(all(A > 0) & A[1] <= dims[1] & A[2] <= dims[2]) antinodes[A[1], A[2]] <- "#"
      if(all(B > 0) & B[1] <= dims[1] & B[2] <= dims[2]) antinodes[B[1], B[2]] <- "#"
    }
  }
}
cat(sum(antinodes == "#"), "unique locations within the bounds of the map contain an antinode", "\n")

# Part 2 ------------------------------------------------------------------
for(i in unique_antennas){
  positions <- which(map == i, arr.ind = TRUE)
  for(j in seq_len(nrow(positions) - 1)){
    for(k in seq_len(nrow(positions) - j) + j){
      a <- positions[j,]
      b <- positions[k,]
      antinodes[a[1], a[2]] <- antinodes[b[1], b[2]] <- "#"
      A <- 2 * a - b
      B <- 2 * b - a
      counter <- 0
      while(all(A > 0) & A[1] <= dims[1] & A[2] <= dims[2]){
        antinodes[A[1], A[2]] <- "#"
        counter <- counter + 1
        A <- a + counter * (a - b)
      }
      counter <- 0
      while(all(B > 0) & B[1] <= dims[1] & B[2] <= dims[2]){
        antinodes[B[1], B[2]] <- "#"
        counter <- counter + 1
        B <- b + counter * (b - a)
      }
    }
  }
}
cat(sum(antinodes == "#"), "unique locations within the bounds of the map contain an antinode", "\n")