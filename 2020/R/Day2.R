# ============================================================================ #
# Topic : Advent of Code
# Day   : 1
# Theme : Report Repair
# ============================================================================ #
library(data.table)
library(stringr)
input <- read.csv('~/Advent of Code/2020/Assignments/Day2Input.txt', header = F)[[1]]

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
# --- Part Two ---
# ============================================================================ #
# Calculate the number of valid passwords in the input
# Each policy describes two positions in the password, where 1 means the first 
# character, 2 means the second character, and so on
# Exactly one of these positions must contain the given letter
for(i in 1:nrow(policy_passwords)){
  letters <- strsplit(policy_passwords$password[i], '')[[1]]
  pattern <- policy_passwords$V3[i]
  policy_passwords$allowed[i] <- xor(letters[policy_passwords$V1[i]] == pattern,
                                     letters[policy_passwords$V2[i]] == pattern)
}
sum(policy_passwords$allowed)