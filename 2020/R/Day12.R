# ============================================================================ #
# Topic : Advent of Code 2020
# Day   : 12
# Theme : Rain Risk
# ============================================================================ #
library(data.table)
input <- read.csv('~/Advent of Code/2020/Assignments/Day12Input.txt', header = F)[[1]]

# ============================================================================ #
# Part One
# ============================================================================ #
# - Action N means to move north by the given value
# - Action S means to move south by the given value
# - Action E means to move east by the given value
# - Action W means to move west by the given value
# - Action L means to turn left the given number of degrees
# - Action R means to turn right the given number of degrees
# - Action F means to move forward by the given value in the direction the ship 
#   is currently facing
# The ship starts by facing east
# Calculate the Manhattan distance between that location and the ship's starting position
instructions <- data.table(action = sub('[^[:alpha:]]+', '', input), 
                           value = as.numeric(sub('[^[:digit:]]+', '', input)))

location <- c(0, 0)
facing <- c(0, 1)
for(i in 1:nrow(instructions)){
  action <- instructions$action[i]
  value <- instructions$value[i]
  if(action == 'N'){
    direction <- c(1, 0)
  }else if(action == 'S'){
    direction <- c(-1, 0)
  }else if(action == 'E'){
    direction <- c(0, 1)
  }else if(action == 'W'){
    direction <- c(0, -1)
  }else if(action == 'L'){
    for(i in 1:(value / 90)){
      facing <- facing[2:1]
      facing[2] <- -facing[2]
      value <- 0
      direction <- facing
    }
  }else if(action == 'R'){
    for(i in 1:(value / 90)){
      facing <- facing[2:1]
      facing[1] <- -facing[1]
      value <- 0
      direction <- facing
    }
  }else if(action == 'F'){
    direction <- facing
  }
  location <- location + value * direction
}
sum(abs(location))

# ============================================================================ #
# Part Two
# ============================================================================ #
# - Action N means to move the waypoint north by the given value
# - Action S means to move the waypoint south by the given value
# - Action E means to move the waypoint east by the given value
# - Action W means to move the waypoint west by the given value
# - Action L means to rotate the waypoint around the ship left 
#   (counter-clockwise) the given number of degrees
# - Action R means to rotate the waypoint around the ship right (clockwise) 
#   the given number of degrees
# - Action F means to move forward to the waypoint a number of times equal to 
#   the given value
# The waypoint starts 10 units east and 1 unit north relative to the ship
location <- c(0, 0)
waypoint <- c(1, 10)
for(i in 1:nrow(instructions)){
  action <- instructions$action[i]
  value <- instructions$value[i]
  if(action %in% c('N', 'S', 'E', 'W')){
    if(action == 'N'){
      direction <- c(1, 0)
    }else if(action == 'S'){
      direction <- c(-1, 0)
    }else if(action == 'E'){
      direction <- c(0, 1)
    }else if(action == 'W'){
      direction <- c(0, -1)
    }
    waypoint <- waypoint + direction * value
  }else if(action == 'F'){
    location <- location + waypoint * value
  }else if(action == 'L'){
    for(i in 1:(value / 90)){
      waypoint[1] <- -waypoint[1]
      waypoint <- waypoint[2:1]
    }
  }else if(action == 'R'){
    for(i in 1:(value / 90)){
      waypoint[2] <- -waypoint[2]
      waypoint <- waypoint[2:1]
    }
  }
}
sum(abs(location))