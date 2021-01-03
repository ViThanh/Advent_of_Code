# ============================================================================ #
# Topic : Advent of Code 2020
# Day   : 13
# Theme : Shuttle Search
# ============================================================================ #
library(primes)
input <- read.csv('~/Advent of Code/2020/Assignments/Day13Input.txt', header = F, sep = '\n')[[1]]

# ============================================================================ #
# Part One
# ============================================================================ #
# Bus schedules are defined based on a timestamp that measures the number of 
# minutes since some fixed reference point in the past
# Calculate the ID of the earliest bus you can take to the airport multiplied 
# by the number of minutes you'll need to wait for that bus
timestamp <- as.numeric(input[1])
busID <- strsplit(input[2], ',')[[1]]
busID <- as.numeric(busID[busID != 'x'])
waiting_time <- busID - ifelse(timestamp %% busID != 0, timestamp %% busID, busID)
busID[waiting_time == min(waiting_time)] * min(waiting_time)

# ============================================================================ #
# Part Two
# ============================================================================ #
# Calculate the earliest timestamp such that all of the listed bus IDs depart at 
# offsets matching their positions in the list
busID <- strsplit(input[2], ',')[[1]]
offset <- (1:length(busID))[busID != 'x'] - 1
busID <- as.numeric(busID[busID != 'x'])
offset <- offset %% busID
is_prime(busID)
timestamp <- 100000000000000
waiting_time <- busID - ifelse(timestamp %% busID != 0, timestamp %% busID, busID)
increase <- 1
for(i in 1:length(busID)){
  while(waiting_time[i] != offset[i]){
    timestamp <- timestamp + increase
    waiting_time <- busID - ifelse(timestamp %% busID != 0, timestamp %% busID, busID)
  }
  increase <- increase * busID[i]
}
paste(timestamp)

