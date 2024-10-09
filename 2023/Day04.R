# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Author: Vi Thanh Pham
# Date: 2023-12-01
# Script Name: Advent of Code - Day 04
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Load input --------------------------------------------------------------
path <- strsplit(rstudioapi::getSourceEditorContext()$path, "/")[[1]]
day <- gsub("(Day)|(.R)", "", path[length(path)])
path <- paste0(sub("Code/", "", paste0(paste(head(path, -1), collapse = "/"), "/")),
               "Data/input", day, ".txt")
input <- unlist(read.table(path, sep = "\n"))

# Part 1 ------------------------------------------------------------------
scratch_cards <- gsub("  ", " ", input)
scratch_cards <- strsplit(substr(scratch_cards, regexpr(":", scratch_cards) + 2, nchar(scratch_cards)), " | ", fixed = T)
scratch_cards <- lapply(scratch_cards, \(x) strsplit(x, " "))
winning <- sapply(scratch_cards, \(x) sum(x[[2]] %in% x[[1]]))
sum(2 ^ (winning[winning > 0] - 1))

# Part 2 ------------------------------------------------------------------
total_scratch_cards <- rep(1, length(winning))
for(i in seq_along(total_scratch_cards)){
  points <- winning[i]
  total_scratch_cards[i + seq_len(points)] <- total_scratch_cards[i + seq_len(points)] + total_scratch_cards[i]
}
sum(total_scratch_cards)