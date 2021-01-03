# ============================================================================ #
# Topic : Advent of Code 2020
# Day   : 6
# Theme : Custom Customs
# ============================================================================ #
library(data.table)
input <- read.csv('~/Advent of Code/2020/Assignments/Day06Input.txt', 
                  header = F, blank.lines.skip = F)[[1]]

# ============================================================================ #
# Part One
# ============================================================================ #
# For each group, count the number of questions to which anyone answered "yes"
# Calculate the sum of these counts
form <- data.table(yes = input)[, group := cumsum(yes == '')]
form <- form[yes != '', .(yes = paste0(yes, collapse = '')), by = group][, -'group']
form[, count := sapply(yes, function(x) length(unique(strsplit(x, '')[[1]])))]
sum(form$count)

# ============================================================================ #
# Part Two
# ============================================================================ #
# For each group, count the number of questions to which everyone answered "yes"
# Calculate the sum of these counts
form <- data.table(yes = input)[, group := cumsum(yes == '')]
form <- form[yes != '', .(yes = paste0(yes, collapse = ''), size = .N), by = group][, -'group']
form[, count := apply(form, 1, function(x) length(which(table(strsplit(x['yes'], '')[[1]]) == x['size'])))]
sum(form$count)
