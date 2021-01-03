# ============================================================================ #
# Topic : Advent of Code 2020
# Day   : 4
# Theme : Passport Processing
# ============================================================================ #
library(data.table)
input <- read.csv('~/Advent of Code/2020/Assignments/Day04Input.txt',
                  header = F, blank.lines.skip = F)

# ============================================================================ #
# Part One
# ============================================================================ #
# Count the number of valid passports - those that have all required fields
# - byr (Birth Year)
# - iyr (Issue Year)
# - eyr (Expiration Year)
# - hgt (Height)
# - hcl (Hair Color)
# - ecl (Eye Color)
# - pid (Passport ID)
# - cid (Country ID)
# Treat cid as optional
fields <- c('byr', 'iyr', 'eyr', 'hgt', 'hcl', 'ecl', 'pid', 'cid')
passports <- as.data.table(input)[, next_ID := cumsum(nchar(V1) == 0)]
passports <- passports[V1 != '', paste(V1, collapse = ' '), by = next_ID][, 'V1']
passports[, V1 := apply(passports, 1, function(x) strsplit(x, ' ')[[1]])]
passports[, valid := sapply(V1, function(x, y) {
  sd <- setdiff(y, substr(x, 1, 3))
  if(length(sd) == 0){
    return(T)
  }else if(length(sd) == 1){
    if(sd == 'cid'){
      return(T)
    }else{
      return(F)
    }
  }else{
    return(F)
  }
}, y = fields)]
sum(passports$valid)

# ============================================================================ #
# Part Two
# ============================================================================ #
# Each field has strict rules about what values are valid for automatic validation:
# - byr (Birth Year) - four digits; at least 1920 and at most 2002
# - iyr (Issue Year) - four digits; at least 2010 and at most 2020
# - eyr (Expiration Year) - four digits; at least 2020 and at most 2030
# - hgt (Height) - a number followed by either cm or in:
#   - If cm, the number must be at least 150 and at most 193
#   - If in, the number must be at least 59 and at most 76
# - hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f
# - ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth
# - pid (Passport ID) - a nine-digit number, including leading zeroes
# - cid (Country ID) - ignored, missing or not
ValidPassport <- function(passports){
  fields <- substr(passports, 1, 3)
  values <- substr(passports, 5, nchar(passports))
  valid <- T
  valid <- valid & between(as.numeric(values[fields == 'byr']), 1920, 2002)
  valid <- valid & between(as.numeric(values[fields == 'iyr']), 2010, 2020)
  valid <- valid & between(as.numeric(values[fields == 'eyr']), 2020, 2030)
  hgt <- as.numeric(sub('in', '', sub('cm', '', values[fields == 'hgt'])))
  unit <- substr(values[fields == 'hgt'], nchar(values[fields == 'hgt']) - 1, nchar(values[fields == 'hgt']))
  unit <- ifelse(unit == 'cm', 'cm', ifelse(unit == 'in', 'in', NA))
  valid <- valid & ifelse(unit == 'cm', between(hgt, 150, 193), between(hgt, 59, 76))
  valid <- valid & substr(values[fields == 'hcl'], 1, 1) == '#' & 
    ifelse('hcl' %in% fields, attributes(gregexpr("[0-9A-Za-z]+", values[fields == 'hcl'])[[1]])$match.length == 6, F)
  valid <- valid & values[fields == 'ecl'] %in% c('amb', 'blu', 'brn', 'gry', 'grn', 'hzl', 'oth')
  valid <- valid & nchar(values[fields == 'pid']) == 9 & !grepl('\\D', values[fields == 'pid'])
  if(length(valid) == 0){
    valid <- F
  }else if(is.na(valid)){
    valid <- F
  }
  return(valid)
}
passports[, valid := sapply(V1, function(x) ValidPassport(x))]
sum(passports$valid)
