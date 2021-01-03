# ============================================================================ #
# Topic : Advent of Code 2020
# Day   : 11
# Theme : Seating System
# ============================================================================ #
input <- read.csv('~/Advent of Code/2020/Assignments/Day11Input.txt', header = F)[[1]]

# ============================================================================ #
# Part One
# ============================================================================ #
# The following rules are applied to every seat simultaneously:
# - If a seat is empty (L) and there are no occupied seats adjacent to it, 
#   the seat becomes occupied
# - If a seat is occupied (#) and four or more seats adjacent to it are also 
#   occupied, the seat becomes empty
# - Otherwise, the seat's state does not change
# Simulate your seating area by applying the seating rules repeatedly until 
# no seats change state
# Calculate the number of occuppied seats
seat_layout <- matrix(t(rbind(sapply(input, function(x) strsplit(x, '')[[1]]))), nrow = length(input))
seat_layout <- rbind('.', cbind('.', seat_layout, '.'), '.')
seat_layout_origin <- matrix('.', ncol = ncol(seat_layout), nrow = nrow(seat_layout))
while(any(seat_layout != seat_layout_origin)){
  seat_layout_origin <- seat_layout
  for(i in 2:(nrow(seat_layout) - 1)){
    for(j in 2:(ncol(seat_layout) - 1)){
      adjacent <- as.vector(seat_layout_origin[(i - 1):(i + 1), (j - 1):(j + 1)])[-5]
      seat_layout[i, j] <- ifelse(seat_layout[i, j] == 'L' & all(adjacent != '#'), '#', 
                                  ifelse(seat_layout[i, j] == '#' & sum(adjacent == '#') >= 4, 'L', seat_layout[i, j]))
    }
  }
}
sum(seat_layout == '#')

# ============================================================================ #
# Part Two
# ============================================================================ #
# Now, instead of considering just the eight immediately adjacent seats, 
# consider the first seat in each of those eight directions
# It now takes five or more visible occupied seats for an occupied seat to become empty
seat_layout <- matrix(t(rbind(sapply(input, function(x) strsplit(x, '')[[1]]))), nrow = length(input))
seat_layout <- rbind('.', cbind('.', seat_layout, '.'), '.')
seat_layout_origin <- matrix('.', ncol = ncol(seat_layout), nrow = nrow(seat_layout))
while(any(seat_layout != seat_layout_origin)){
  seat_layout_origin <- seat_layout
  for(i in 2:(nrow(seat_layout) - 1)){
    for(j in 2:(ncol(seat_layout) - 1)){
      adjacent <- rep(NA, 8)
      seats <- seat_layout_origin[(i - 1):1, j]
      adjacent[1] <- seats[which(seats != '.')[1]]
      seats <- seat_layout_origin[(i + 1):nrow(seat_layout_origin), j]
      adjacent[2] <- seats[which(seats != '.')[1]]
      seats <- seat_layout_origin[i, (j - 1):1]
      adjacent[3] <- seats[which(seats != '.')[1]]
      seats <- seat_layout_origin[i, (j + 1):ncol(seat_layout_origin)]
      adjacent[4] <- seats[which(seats != '.')[1]]
      seats <- diag(as.matrix(seat_layout_origin[(i - 1):1, (j - 1):1]))
      adjacent[5] <- seats[which(seats != '.')[1]]
      seats <- diag(as.matrix(seat_layout_origin[(i + 1):nrow(seat_layout_origin), (j + 1):ncol(seat_layout_origin)]))
      adjacent[6] <- seats[which(seats != '.')[1]]
      seats <- diag(as.matrix(seat_layout_origin[(i + 1):nrow(seat_layout_origin), (j - 1):1]))
      adjacent[7] <- seats[which(seats != '.')[1]]
      seats <- diag(as.matrix(seat_layout_origin[(i - 1):1, (j + 1):ncol(seat_layout_origin)]))
      adjacent[8] <- seats[which(seats != '.')[1]]
      adjacent[is.na(adjacent)] <- '.'
      seat_layout[i, j] <- ifelse(seat_layout[i, j] == 'L' & all(adjacent != '#'), '#', 
                                  ifelse(seat_layout[i, j] == '#' & sum(adjacent == '#') >= 5, 'L', seat_layout[i, j]))
    }
  }
}
sum(seat_layout == '#')
