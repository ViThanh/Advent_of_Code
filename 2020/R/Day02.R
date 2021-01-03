# ============================================================================ #
# Topic : Advent of Code 2020
# Day   : 2
# Theme : Password Philosophy
# ============================================================================ #
library(data.table)
library(stringr)
input <- read.csv('~/Advent of Code/2020/Assignments/Day02Input.txt', header = F)[[1]]

# ============================================================================ #
# Part One
# ============================================================================ #
# Calculate the number of valid passwords in the input
# The password policy indicates the lowest and highest number of times a given 
# letter must appear for the password to be valid
policy_passwords <- strsplit(input, ' ')
policy <- t(sapply(policy_passwords,
                   function(x) c(unlist(strsplit(x[[1]], '-')), 
                                 sub(':', '', x[[2]]))))
policy_passwords <- as.data.table(policy)[, password := sapply(policy_passwords, function(x) x[[3]])]
policy_passwords[, `:=` (V1 = as.numeric(V1), V2 = as.numeric(V2))]
policy_passwords[, allowed := between(str_count(password, V3), V1, V2)]
sum(policy_passwords$allowed)

# ============================================================================ #
# Part Two
# ============================================================================ #
# Calculate the number of valid passwords in the input
# Each policy describes two positions in the password, where 1 means the first 
# character, 2 means the second character, and so on
# Exactly one of these positions must contain the given letter
policy_passwords[, allowed := xor(substr(password, V1, V1) == V3,
                                  substr(password, V2, V2) == V3)]
sum(policy_passwords$allowed)