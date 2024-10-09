# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Author: Vi Thanh Pham
# Date: 2023-12-01
# Script Name: Advent of Code - Day 01
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Load input --------------------------------------------------------------
path <- strsplit(rstudioapi::getSourceEditorContext()$path, "/")[[1]]
day <- gsub("(Day)|(.R)", "", path[length(path)])
path <- paste0(sub("Code/", "", paste0(paste(head(path, -1), collapse = "/"), "/")),
               "Data/input", day, ".txt")
input <- unlist(read.table(path))

# Part 1 ------------------------------------------------------------------
calibration <- strsplit(input, "")
calibration <- sapply(calibration, \(x) x[regexpr("[0-9]", x) == 1])
calibration <- sapply(calibration, \(x) as.numeric(paste0(x[1], rev(x)[1])))
sum(calibration)

# Part 2 ------------------------------------------------------------------
calibration <- strsplit(input, "")
digits <- 1:9
digits_char <- c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine")
for(i in seq_along(input)){
  for(j in seq_along(digits)){
    position <- gregexpr(digits_char[j], input[i])[[1]]
    if(any(position != -1)) calibration[[i]][position] <- digits[j]
  }
}
calibration <- sapply(calibration, \(x) x[regexpr("[0-9]", x) == 1])
calibration <- sapply(calibration, \(x) as.numeric(paste0(x[1], rev(x)[1])))
sum(calibration)
