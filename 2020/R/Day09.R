# ============================================================================ #
# Topic : Advent of Code 2020
# Day   : 9
# Theme : Encoding Error
# ============================================================================ #
input <- read.csv('~/Advent of Code/2020/Assignments/Day09Input.txt', header = F)[[1]]

# ============================================================================ #
# Part One
# ============================================================================ #
# Determine the first number after the first 25, that is not a sum of two of 
# the 25 previous numbers
valid <- T
pos <- 25
while(valid & pos < length(input)){
  pos <- pos + 1
  valid <- input[pos] %in% combn(input[(pos - 26):(pos - 1)], 2, sum)
}
(invalid_number <- input[pos])

# ============================================================================ #
# --- Part Two ---
# ============================================================================ #
# To find the encryption weakness, add together the smallest and largest number 
# in the contiguous range in the input, that add up to the invalid number
weakness <- F
counter <- 0
while(!weakness & counter < length(input)){
  counter <- counter + 1
  weakness <- invalid_number %in% cumsum(input[counter:length(input)])
}
cont_range <- input[counter:(counter + which(cumsum(input[counter:length(input)]) == invalid_number) - 1)]
sum(range(cont_range))