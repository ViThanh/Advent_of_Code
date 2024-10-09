# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Author: Vi Thanh Pham
# Date: 2023-12-01
# Script Name: Advent of Code - Day 12
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
options(scipen = 999)
library(memoise)
# Load input --------------------------------------------------------------
path <- strsplit(rstudioapi::getSourceEditorContext()$path, "/")[[1]]
day <- gsub("(Day)|(.R)", "", path[length(path)])
path <- paste0(sub("Code/", "", paste0(paste(head(path, -1), collapse = "/"), "/")),
               "Data/input", day, ".txt")
input <- unlist(read.table(path, sep = "\n", comment.char = ""))

# Part 1 ------------------------------------------------------------------
records <- strsplit(input, " ")
springs <- lapply(records, \(x) strsplit(x[1], "", fixed = T)[[1]])
conditions <- lapply(records, \(x) as.numeric(strsplit(x[2], ",")[[1]]))
arrangements <- numeric(length(springs))
for(i in seq_along(springs)){
  spring <- springs[[i]]
  condition <- conditions[[i]]
  damaged <- sum(condition)
  expansion <- as.matrix(expand.grid(lapply(spring, \(x) if(x == "?") c(".", "#") else x), stringsAsFactors = F))
  expansion <- expansion[apply(expansion, 1, \(x) sum(x == "#") == damaged),, drop = F]
  arrangements[i] <- sum(apply(expansion, 1, \(x){
    a <- diff(c(0, which(c(diff(which(x == "#")) > 1, T))))
    if(length(a) == length(condition)) all(a == condition) else F
  }))
}
sum(arrangements)

# Part 2 ------------------------------------------------------------------
damaged <- \(spring, condition){
  l <- condition[1]
  if(any(spring[seq_len(l)] == ".")){
    0
  }else{
    if(length(spring) == l){
      1
    }else if(spring[l + 1] %in% c("?", ".")){
      if(length(spring) > (l + 1)){
        valid(spring[(l + 2):length(spring)], condition[-1])
      }else{
        1
      }
    }else{
      0
    }
  }
}
damaged <- memoise(damaged)
valid <- \(spring, condition){
  if(length(condition) == 0){
    if(length(spring) > 0){
      if(any(spring == "#")) 0 else 1
    }else{
      1
    }
  }else if(length(spring) < (sum(condition) + length(condition) - 1)){
    0
  }else{
    if(spring[1] == "#"){
      damaged(spring, condition)
    }else if(spring[1] == "."){
      valid(spring[-1], condition)
    }else{
      damaged(spring, condition) + valid(spring[-1], condition)
    }
  }
}
valid <- memoise(valid)
arrangements <- numeric(length(springs))
for(i in seq_along(springs)){
  spring <- rep(c("?", springs[[i]]), 5)[-1]
  condition <- rep(conditions[[i]], 5)
  arrangements[i] <- valid(spring, condition)
}
sum(arrangements)