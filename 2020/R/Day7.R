# ============================================================================ #
# Topic : Advent of Code 2020
# Day   : 7
# Theme : Handy Haversacks
# ============================================================================ #
library(data.table)
input <- read.csv('~/Advent of Code/2020/Assignments/Day7Input.txt', header = F)[[1]]

# ============================================================================ #
# Part One
# ============================================================================ #
# Calculate how many bag colors can contain at least one shiny gold bag
rules <- data.table(rule = input)
rules[, big := sub(' bags.*', '', rule)]
rules[, small := sub(' bag.*', '', sub('(.*[0-9] )|(.*no other)', '', rule))]

# ============================================================================ #
# --- Part Two ---
# ============================================================================ #
