# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Topic : Advent of Code 2015
# Day   : 9
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Load data ---------------------------------------------------------------
source("Get_data.R")
input <- aoc_get_response(9, year = 2015, session_cookie = keyring::key_get("RStudio Keyring Secrets", "Advent of Code Session Cookie")) |>
  httr::content(encoding = 'UTF-8') |>
  readr::read_lines()

# Part 1 ------------------------------------------------------------------
distances <- data.frame(t(sapply(strsplit(input, "( to )|( = )"), \(x) x)))
colnames(distances) <- c("A", "B", "distance")
distances$distance <- as.numeric(distances$distance)
locations <- unique(c(distances$A, distances$B))
distances_matrix <- array(0, dim = rep(length(locations), 2))
colnames(distances_matrix) <- rownames(distances_matrix) <- locations
for(i in seq_len(nrow(distances))){
  A <- distances$A[i]
  B <- distances$B[i]
  distance <- distances$distance[i]
  distances_matrix[A == rownames(distances_matrix), B == colnames(distances_matrix)] <- distances_matrix[B == rownames(distances_matrix), A == colnames(distances_matrix)] <- distance
}
D <- \(A, B) distances_matrix[rownames(distances_matrix) == A, colnames(distances_matrix) == B]
Distance <- \(path){
  if(length(path) == 0){
    distance <- 0
  }else if(length(path) == 2){
    distance <- D(path[1], path[2])
  }else{
    distance <- D(path[length(path)], path[(length(path) - 1)])
    path <- path[-length(path)]
    distance <- distance + Distance(path)
  }
  return(distance)
}
Distance(c("AlphaCentauri", "Snowdin"))
routes <- do.call(rbind, combinat::permn(locations))
route_length <- numeric(nrow(routes))
for(i in seq_len(nrow(routes))){
  print(i)
  route_length[i] <- Distance(routes[i,])  
}
cat("The shortest distance between locations is", min(route_length), "\n")

# Part 2 ------------------------------------------------------------------
cat("The longest distance between locations is", max(route_length), "\n")