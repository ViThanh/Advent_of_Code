# ============================================================================ #
# Topic : Advent of Code 2020
# Day   : 7
# Theme : Handy Haversacks
# ============================================================================ #
library(data.table)
library(dplyr)
input <- read.csv('~/Advent of Code/2020/Assignments/Day7Input.txt', header = F, sep = ".")[[1]]

# ============================================================================ #
# Part One
# ============================================================================ #
# Calculate how many bag colors can contain at least one shiny gold bag
rules <- data.table(rule = input)
rules[, short_rule := gsub(' bags contain', ',', rule)]
rules[, short_rule := gsub('( bags)|( bag)|(, no other bags)', '', short_rule)]
rules[, outer := sapply(short_rule, function(x) strsplit(x, ', ')[[1]][1])]

inner <- unlist(sapply(rules$short_rule, function(x) strsplit(x, ', ')[[1]][-1]))
rules <- left_join(rules, 
                   data.table(short_rule = sub('[0-9]$', '', names(inner)), 
                              number = as.numeric(regmatches(inner, gregexpr("[[:digit:]]", inner))), 
                              inner = sub('[0-9] ', '', inner)), by = 'short_rule')[, -'short_rule']

rules_all <- as.data.table(rules)
i <- 0
while(any(!is.na(rules_all$inner))){
  i <- i + 1
  rules_all <- left_join(rules_all, rules[, c('outer', 'inner')], by = c('inner' = 'outer'))
  setnames(rules_all, 'inner', paste('inner', i, sep = '_'))
  setnames(rules_all, 'inner.y', 'inner')
}
rules_all <- rules_all[, -'inner']
rules_all <- as.data.table(rules_all)[, shiny_gold := apply(rules_all[, -c('rule', 'outer')], 1, function(x) any(x == 'shiny gold', na.rm = T))]
uniqueN(rules_all[shiny_gold == T, outer])

# ============================================================================ #
# --- Part Two ---
# ============================================================================ #
#  Calculate how many individual bags are required inside a shiny gold bag
rules_all <- as.data.table(rules)[is.na(inner), inner_bags := 0]
while(any(is.na(rules_all$inner_bags))){
  inner_bags <- rules_all[, .(inner_bags = sum(inner_bags)), by = outer]
  rules_all <- data.table(left_join(rules_all, inner_bags[!is.na(inner_bags), ], by = c('inner' = 'outer')))
  setnames(rules_all, 'inner_bags.x', 'inner_bags')
  rules_all[is.na(inner_bags), inner_bags := number * (inner_bags.y + 1)]
  rules_all <- rules_all[, -'inner_bags.y']
  rules_all[!is.na(inner_bags), c('number', 'inner') := NA]
}
inner_bags <- rules_all[, .(inner_bags = sum(inner_bags)), by = outer]
inner_bags[outer == 'shiny gold', inner_bags]