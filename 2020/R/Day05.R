# ============================================================================ #
# Topic : Advent of Code 2020
# Day   : 5
# Theme : Binary Boarding
# ============================================================================ #
library(data.table)
input <- read.csv('~/Advent of Code/2020/Assignments/DayO5Input.txt', header = F)[[1]]

# ============================================================================ #
# Part One
# ============================================================================ #
# Calculate the  highest seat ID on a boarding pass
# The first 7 characters will either be F or B; these specify exactly one of 
# the 128 rows on the plane
# The last three characters will be either L or R; these specify exactly one of 
# the 8 columns of seats on the plane
# Every seat has a unique seat ID: multiply the row by 8, then add the column
boarding_passes <- data.table(boarding_pass = input)
boarding_passes[, row_number := sapply(strsplit(boarding_pass, ''), function(x) sum(2 ^ (6:0)[x[1:7] == 'B']))]
boarding_passes[, column := sapply(strsplit(boarding_pass, ''), function(x) sum(2 ^ (2:0)[x[8:10] == 'R']))]
boarding_passes[, seat_ID := row_number * 8 + column]
max(boarding_passes$seat_ID)

# ============================================================================ #
# Part Two
# ============================================================================ #
# FInd the missing seat ID, for which there are seat IDs one bigger and one smaller
setdiff(min(boarding_passes$seat_ID):max(boarding_passes$seat_ID), boarding_passes$seat_ID)
