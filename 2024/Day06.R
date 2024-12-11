# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Topic : Advent of Code 2024
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Load data ---------------------------------------------------------------
source("Get_data.R")
input <- aoc_get_response(6, year = 2024, session_cookie = keyring::key_get("RStudio Keyring Secrets", "Advent of Code Session Cookie")) |>
  httr::content(encoding = 'UTF-8') |>
  readr::read_lines()

# Part 1 ------------------------------------------------------------------
map <- do.call(rbind, strsplit(input, ""))
current_position <- positions <- which(map == "^", arr.ind = TRUE)
while(!any(current_position %in% range(c(1, dim(map))))){
  direction <- map[current_position]
  if(direction == "^"){
    next_position <- current_position + c(-1, 0)
    next_direction <- ">"
  }else if(direction == ">"){
    next_position <- current_position + c(0, 1)
    next_direction <- "v"
  }else if(direction == "v"){
    next_position <- current_position + c(1, 0)
    next_direction <- "<"
  }else{
    next_position <- current_position + c(0, -1)
    next_direction <- "^"
  }
  if(map[next_position] == "#"){
    map[current_position] <- next_direction
  }else{
    map[next_position] <- direction
    current_position <- next_position
    positions <- rbind(positions, current_position)
  }
}
cat("The guard will visit", nrow(unique(positions)), "unique positions before leaving the mapped area", "\n")

# Part 2 ------------------------------------------------------------------
original_map <- do.call(rbind, strsplit(input, ""))
original_position <- which(original_map == "^", arr.ind = TRUE)
original_route <- positions
obstruction_positions <- unique(original_route[apply(original_route == original_position[rep(1, nrow(original_route)),], 1, sum) != 2,, drop = FALSE])
loops <- 0
for(i in seq_len(nrow(obstruction_positions))){
  print(i)
  map <- original_map
  map[obstruction_positions[i,, drop = FALSE]] <- "#"
  current_position <- original_position
  positions <- current_position[0,, drop = FALSE]
  loop <- FALSE
  turns <- 0
  while(!any(current_position %in% range(c(1, dim(map)))) & !loop){
    direction <- map[current_position]
    if(direction == "^"){
      next_position <- current_position + c(-1, 0)
      next_direction <- ">"
    }else if(direction == ">"){
      next_position <- current_position + c(0, 1)
      next_direction <- "v"
    }else if(direction == "v"){
      next_position <- current_position + c(1, 0)
      next_direction <- "<"
    }else{
      next_position <- current_position + c(0, -1)
      next_direction <- "^"
    }
    if(map[next_position] == "#"){
      turns <- turns + 1
      map[current_position] <- next_direction
      if(turns >= i) positions <- rbind(positions, current_position)
    }else{
      map[next_position] <- direction
      current_position <- next_position
    }
    if(turns >= i) loop <- nrow(positions) != nrow(unique(positions))
  }
  loops <- loops + loop
}
cat(loops, "different positions of obstruction would make the guard stuck in a loop", "\n")