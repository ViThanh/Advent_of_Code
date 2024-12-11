# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Topic : Advent of Code 2024
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Load data ---------------------------------------------------------------
source("Get_data.R")
input <- aoc_get_response(10, year = 2024, session_cookie = keyring::key_get("RStudio Keyring Secrets", "Advent of Code Session Cookie")) |>
  httr::content(encoding = 'UTF-8') |>
  readr::read_lines()

# Part 1 ------------------------------------------------------------------
map <- do.call(rbind, lapply(strsplit(input, ""), \(x) as.numeric(x)))
padded_map <- array(Inf, dim = dim(map) + 16)
padded_map[1:dim(map)[1] + 8, 1:dim(map)[2] + 8] <- map
trail_heads <- which(padded_map == 0, arr.ind = TRUE)
Trail <- \(padded_map, position, head_position){
  if(padded_map[position[1], position[2]] == 1){
    if(0 %in% c(padded_map[position[1], position[2] + c(-1,1)], padded_map[position[1] + c(-1,1), position[2]])){
      correct_head <- head_position == t(rbind(data.frame(row = position[1], col = position[2] + c(-1,1), row.names = NULL),
                                               data.frame(row = position[1] + c(-1,1), col = position[2], row.names = NULL)))
      if(any(apply(correct_head, 2, \(x) all(x)))){
        TRUE
      }else{
        FALSE
      }
    }else{
      FALSE
    }
  }else{
    area <- padded_map[position[1] + (-1:1), position[2] + (-1:1)]
    area[as.matrix(data.frame(c(1, 3, 1, 3), c(1, 1, 3, 3)))] <- Inf
    route <- which(area == padded_map[position[1], position[2]] - 1, arr.ind = TRUE)
    route[,1] <- route[,1] + (position[1] - 2)
    route[,2] <- route[,2] + (position[2] - 2)
    if(nrow(route) == 0){
      FALSE
    }else{
      any(apply(route, 1, \(x) Trail(padded_map, x, head_position)))
    }
  }
}
routes <- numeric(nrow(trail_heads))
for(i in seq_len(nrow(trail_heads))){
  h <- trail_heads[i,]
  area <- padded_map[h[1] + (-8:8), h[2] + (-8:8)]
  tops <- which(area == 9, arr.ind = TRUE)
  tops[,1] <- tops[,1] + (h[1] - 9)
  tops[,2] <- tops[,2] + (h[2] - 9)
  routes[i] <- sum(apply(tops, 1, \(x) Trail(padded_map, x, h)))
}
cat("The sum of the scores of all trailheads on the topographic map is", sum(routes), "\n")

# Part 2 ------------------------------------------------------------------
padded_map <- array(Inf, dim = dim(map) + 2)
padded_map[1:dim(map)[1] + 1, 1:dim(map)[2] + 1] <- map
trail_heads <- which(padded_map == 0, arr.ind = TRUE)
Rating <- \(padded_map, position){
  if(padded_map[position[1], position[2]] == 8){
    sum(c(padded_map[position[1], position[2] + c(-1,1)], padded_map[position[1] + c(-1,1), position[2]]) == 9)
  }else{
    area <- padded_map[position[1] + (-1:1), position[2] + (-1:1)]
    area[as.matrix(data.frame(c(1, 3, 1, 3), c(1, 1, 3, 3)))] <- Inf
    route <- which(area == padded_map[position[1], position[2]] + 1, arr.ind = TRUE)
    route[,1] <- route[,1] + (position[1] - 2)
    route[,2] <- route[,2] + (position[2] - 2)
    if(nrow(route) == 0){
      FALSE
    }else{
      sum(apply(route, 1, \(x) Rating(padded_map, x)))
    }
  }
}
ratings <- numeric(nrow(trail_heads))
for(i in seq_len(nrow(trail_heads))){
  h <- trail_heads[i,]
  ratings[i] <- Rating(padded_map, h)
}
cat("The sum of the ratings of all trailheads on the topographic map is", sum(ratings), "\n")