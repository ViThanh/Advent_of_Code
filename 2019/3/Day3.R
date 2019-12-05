# ============================================================================ #
# Topic : Advent of Code
# --- Day 3: Crossed Wires ---
# ============================================================================ #
library(rlist)
setwd("~/Advent of Code/2019/3")
input <- data.frame(t(read.csv("input.txt", header = F)), row.names = NULL)
colnames(input) <- paste0("W", 1:2)

calculateCoordinates <- function(W) {
  coordinates <- list()
  x <- 0
  y <- 0
  for (i in seq_along(W)){
    direction <- substr(W[i], 1, 1)
    count <- strtoi(substr(W[i], 2, nchar(as.vector(W[i]))))
    if (direction == "R") {
      coordinates[[i]] <- expand.grid(x + 1:count, y)
      x <- x + count
    } else if (direction == "L") {
      coordinates[[i]] <- expand.grid(x - 1:count, y)
      x <- x - count
    } else if (direction == "U") {
      coordinates[[i]] <- expand.grid(x, y + 1:count)
      y <- y + count
    } else if (direction == "D") {
      coordinates[[i]] <- expand.grid(x, y - 1:count)
      y <- y - count
    }
  }
  coordinates <- list.rbind(coordinates)
  names(coordinates) <-c("x", "y")
  return(coordinates)
}

calculateClosestIntersectionDistance <- function(T1, T2, on = c("x", "y"),
                                                 method = "manhattan") {
  intersections <- match_df(T1, T2, on = on)
  if (method == "manhattan") {
    distance <- min(rowSums(abs(intersections)))
  } else if (method == "step") {
    steps <- matrix(nrow = nrow(intersections), ncol = ncol(intersections))
    n1 <- nrow(T1)
    n2 <- nrow(T2)
    for (i in 1:nrow(intersections)) {
      steps[i, 1] <- min(which(rowSums(T1 == intersections[i, ][rep(1, n1), ]) == 2))
      steps[i, 2] <- min(which(rowSums(T2 == intersections[i, ][rep(1, n2), ]) == 2))
    }
    distance <- min(rowSums(steps))
  }
  return(distance)
}

# EXAMPLES
W1 <- c("R75","D30","R83","U83","L12","D49","R71","U7","L72")
W2 <- c("U62","R66","U55","R34","D71","R55","D58","R83")
T1 <- calculateCoordinates(W1)
T2 <- calculateCoordinates(W2)
# Result should be 159
calculateClosestIntersectionDistance(T1, T2)

W1 <- c("R98","U47","R26","D63","R33","U87","L62","D20","R33","U53","R51")
W2 <- c("U98","R91","D20","R16","D67","R40","U7","R15","U6","R7")
T1 <- calculateCoordinates(W1)
T2 <- calculateCoordinates(W2)
# Result should be 135
calculateClosestIntersectionDistance(T1, T2)

# PUZZLE
coordinates <- apply(input, 2, calculateCoordinates)
calculateClosestIntersectionDistance(coordinates$W1, coordinates$W2)

# ============================================================================ #
# --- Part Two ---
# ============================================================================ #
# EXAMPLES
W1 <- c("R75","D30","R83","U83","L12","D49","R71","U7","L72")
W2 <- c("U62","R66","U55","R34","D71","R55","D58","R83")
T1 <- calculateCoordinates(W1)
T2 <- calculateCoordinates(W2)
# Result should be 610
calculateClosestIntersectionDistance(T1, T2, method = "step")

W1 <- c("R98","U47","R26","D63","R33","U87","L62","D20","R33","U53","R51")
W2 <- c("U98","R91","D20","R16","D67","R40","U7","R15","U6","R7")
T1 <- calculateCoordinates(W1)
T2 <- calculateCoordinates(W2)
# Result should be 410
calculateClosestIntersectionDistance(T1, T2, method = "step")

# PUZZLE
coordinates <- apply(input, 2, calculateCoordinates)
calculateClosestIntersectionDistance(coordinates$W1, coordinates$W2, method = "step")