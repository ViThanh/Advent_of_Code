# ============================================================================ #
# Topic : Advent of Code 2020
# Day   : 1
# Theme : Report Repair
# ============================================================================ #
input <- read.csv('~/Advent of Code/2020/Assignments/Day01Input.txt', header = F)[[1]]

# ============================================================================ #
# Part One
# ============================================================================ #
# Find a product of the two elements in the input, that adds up to 2020
n <- 2
combn(input, n, prod)[which(combn(input, n, sum) == 2020)]


# ============================================================================ #
# Part Two
# ============================================================================ #
# Find a product of the three elements in the input, that adds up to 2020
n <- 3
combn(input, n, prod)[which(combn(input, n, sum) == 2020)]