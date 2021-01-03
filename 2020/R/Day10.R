# ============================================================================ #
# Topic : Advent of Code 2020
# Day   : 10
# Theme : Adapter Array
# ============================================================================ #
library(data.table)
input <- read.csv('~/Advent of Code/2020/Assignments/Day10Input.txt', header = F)[[1]]

# ============================================================================ #
# Part One
# ============================================================================ #
# Find a chain that uses all of your adapters to connect the charging outlet to 
# your device's built-in adapter and count the joltage differences between 
# the charging outlet, the adapters, and your device
# Calculate the number of 1-jolt differences multiplied by the number of 
# 3-jolt differences
adapters <- data.table(adapter = c(0, sort(input), max(input) + 3))
adapters[, jolt_diff := c(diff(adapter), NA)]
sum(adapters$jolt_diff == 1, na.rm = T) * sum(adapters$jolt_diff == 3, na.rm = T)

# ============================================================================ #
# Part Two
# ============================================================================ #
# Calculate the total number of distinct ways you can arrange the adapters 
# to connect the charging outlet to your device
adapters[adapter %in% c(0, max(adapter)) | jolt_diff == 3 | shift(jolt_diff, 1) == 3, keep := T]
arrange <- adapters[is.na(keep), ]
arrange[, group := c(0, cumsum(diff(adapter) > 3))]
arrange <- arrange[, .(count = .N), by = group]
arrange[, combinations := colSums(sapply(count, function(x) choose(x, 0:2)))]
print(paste(prod(arrange$combinations)))