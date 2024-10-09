# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Author: Vi Thanh Pham
# Date: 2023-12-01
# Script Name: Advent of Code - Day 13
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
options(scipen = 999)
# Load input --------------------------------------------------------------
path <- strsplit(rstudioapi::getSourceEditorContext()$path, "/")[[1]]
day <- gsub("(Day)|(.R)", "", path[length(path)])
path <- paste0(sub("Code/", "", paste0(paste(head(path, -1), collapse = "/"), "/")),
               "Data/input", day, ".txt")
input <- unlist(read.table(path, sep = "\n", comment.char = "", blank.lines.skip = F))

# Part 1 ------------------------------------------------------------------
separator <- c(0, which(input == ""), length(input) + 1)
patterns <- lapply(seq_len(length(separator) - 1), \(x) do.call(rbind, strsplit(input[(separator[x] + 1):(separator[x + 1] - 1)], "")))
notes <- 0
for(i in seq_along(patterns)){
  pattern <- patterns[[i]]
  mirror_col <- which(apply(pattern[, -1] == pattern[, -ncol(pattern)], 2, sum) == nrow(pattern))
  mirror_row <- which(apply(pattern[-1,] == pattern[-nrow(pattern),], 1, sum) == ncol(pattern))
  if(length(mirror_col) != 0){
    mirror_col <- mirror_col[sapply(mirror_col, \(x){
      l <- min(x, ncol(pattern) - x)
      all(pattern[, seq_len(l) + x - l, drop = F] == pattern[, rev(seq_len(l)) + x, drop = F])
    })]
    if(length(mirror_col) != 0) notes <- notes + mirror_col
  }
  if(length(mirror_row) != 0){
    mirror_row <- mirror_row[sapply(mirror_row, \(x){
      l <- min(x, nrow(pattern) - x)
      all(pattern[seq_len(l) + x - l,, drop = F] == pattern[rev(seq_len(l)) + x,, drop = F])
    })]
    if(length(mirror_row) != 0) notes <- notes + 100 * mirror_row
  }
}
notes

# Part 2 ------------------------------------------------------------------
notes <- 0
for(i in seq_along(patterns)){
  pattern <- patterns[[i]]
  mirror_col <- which(apply(pattern[, -1] == pattern[, -ncol(pattern)], 2, sum) %in% (nrow(pattern) - 1:0))
  mirror_row <- which(apply(pattern[-1,] == pattern[-nrow(pattern),], 1, sum) %in% (ncol(pattern) - 1:0))
  if(length(mirror_col) != 0){
    mirror_col <- mirror_col[sapply(mirror_col, \(x){
      l <- min(x, ncol(pattern) - x)
      sum(pattern[, seq_len(l) + x - l, drop = F] != pattern[, rev(seq_len(l)) + x, drop = F]) == 1
    })]
    if(length(mirror_col) != 0) notes <- notes + mirror_col
  }
  if(length(mirror_row) != 0){
    mirror_row <- mirror_row[sapply(mirror_row, \(x){
      l <- min(x, nrow(pattern) - x)
      sum(pattern[seq_len(l) + x - l,, drop = F] != pattern[rev(seq_len(l)) + x,, drop = F]) == 1
    })]
    if(length(mirror_row) != 0) notes <- notes + 100 * mirror_row
  }
}
notes