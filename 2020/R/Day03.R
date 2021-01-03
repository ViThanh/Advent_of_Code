# ============================================================================ #
# Topic : Advent of Code 2020
# Day   : 3
# Theme : Toboggan Trajectory
# ============================================================================ #
input <- read.csv('~/Advent of Code/2020/Assignments/Day03Input.txt', header = F)[[1]]

# ============================================================================ #
# Part One
# ============================================================================ #
# Calculate the number of trees encountered on the way
# Starting at the top-left corner of your map and following a slope of right 3 
# and down 1
EncounteredTrees <- function(slope, map){
  position <- c(1, 1)
  cap <- nchar(map[1])
  trees <- 0
  while(position[1] < length(map)){
    position <- position + slope
    pos <- ifelse(position[2] %% cap == 0, cap, position[2] %% cap)
    trees <- trees + as.numeric(substr(map[position[1]], pos, pos) == '#')
  }
  return(trees)
}
EncounteredTrees(c(1, 3), input)


# ============================================================================ #
# Part Two
# ============================================================================ #
# Multiply the number of trees form the following slopes:
# - Right 1, down 1.
# - Right 3, down 1. (This is the slope you already checked.)
# - Right 5, down 1.
# - Right 7, down 1.
# - Right 1, down 2.
slopes <- list(c(1, 1), c(1, 3), c(1, 5), c(1, 7), c(2, 1))
prod(sapply(slopes, function(x, map) EncounteredTrees(x, map), map = input))
