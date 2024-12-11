# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Topic : Advent of Code 2024
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Load data ---------------------------------------------------------------
source("Get_data.R")
input <- aoc_get_response(9, year = 2024, session_cookie = keyring::key_get("RStudio Keyring Secrets", "Advent of Code Session Cookie")) |>
  httr::content(encoding = 'UTF-8') |>
  readr::read_lines()

# Part 1 ------------------------------------------------------------------
disk_map <- as.numeric(strsplit(input, "")[[1]])
blocks <- numeric(0)
for(i in seq_along(disk_map)){
  if((i %% 2) == 1){
    blocks <- c(blocks, rep(i %/% 2, disk_map[i]))
  }else{
    blocks <- c(blocks, rep(Inf, disk_map[i]))
  }
}
for(i in seq_along(blocks)){
  if(is.infinite(blocks[i])){
    if(i < length(blocks)){
      if(any(!is.infinite(blocks[(i + 1):length(blocks)]))){
        position <- rev(which(!is.infinite(blocks)))[1]
        blocks[i] <- blocks[position]
        blocks[position] <- Inf
      }
    }
  }
}
filesystem <- (seq_along(blocks) - 1) * blocks
filesystem <- filesystem[!is.infinite(filesystem)]
options(scipen = 999)
cat("The resulting filesystem checksum is", sum(filesystem), "\n")

# Part 2 ------------------------------------------------------------------
disk_map <- as.numeric(strsplit(input, "")[[1]])
blocks <- numeric(0)
for(i in seq_along(disk_map)){
  if((i %% 2) == 1){
    blocks <- c(blocks, rep(i %/% 2, disk_map[i]))
  }else{
    blocks <- c(blocks, rep(Inf, disk_map[i]))
  }
}
IDs <- sort(setdiff(unique(blocks), Inf), decreasing = TRUE)
for(i in setdiff(IDs, 0)){
  print(i)
  n <- sum(blocks == i)
  appearance <- which(blocks == i)[1]
  empty <- which(is.infinite(blocks))
  moved <- FALSE
  j <- 1
  while((!moved) & (appearance > empty[j]) & (j <= (length(empty) - n))){
    if(all(diff(empty[j + seq_len(n) - 1]) == 1)){
      blocks[blocks == i] <- Inf
      blocks[empty[j + seq_len(n) - 1]] <- i
      moved <- TRUE
    }
    j <- j + 1
  }
}
filesystem <- (seq_along(blocks) - 1) * blocks
filesystem <- filesystem[!is.infinite(filesystem)]
options(scipen = 999)
cat("The resulting filesystem checksum is", sum(filesystem), "\n")


space <- disk_map[seq_len(length(disk_map) / 2) * 2]
for(i in setdiff(IDs, 0)){
  n <- sum(blocks == i)
  position <- which(space[seq_len(i - 1)] >= n)
  if(length(position) > 0){
    position <- position[1]
    fill <- sum(disk_map[seq_len(position * 2) - 1])
    blocks[blocks == i] <- Inf
    blocks[fill + seq_len(n)] <- i
    space[position] <- space[position] - n
    disk_map[position * 2 - 1] <- disk_map[position * 2 - 1] + n
    disk_map[position * 2] <- disk_map[position * 2] - n
  }
}