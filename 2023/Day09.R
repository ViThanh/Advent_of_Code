# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Author: Vi Thanh Pham
# Date: 2023-12-01
# Script Name: Advent of Code - Day 09
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Load input --------------------------------------------------------------
path <- strsplit(rstudioapi::getSourceEditorContext()$path, "/")[[1]]
day <- gsub("(Day)|(.R)", "", path[length(path)])
path <- paste0(sub("Code/", "", paste0(paste(head(path, -1), collapse = "/"), "/")),
               "Data/input", day, ".txt")
input <- unlist(read.table(path, sep = "\n"))

# Part 1 ------------------------------------------------------------------
history <- do.call(rbind, strsplit(input, " "))
class(history) <- "numeric"
extrapolate <- numeric(nrow(history))
for(i in seq_len(nrow(history))){
  h <- history[i,]
  extrapolate[i] <- rev(h)[1]
  while(any(diff(h) != 0)){
    h <- diff(h)
    extrapolate[i] <- extrapolate[i] + rev(h)[1]
  }
}
sum(extrapolate)

# Part 2 ------------------------------------------------------------------
extrapolate <- numeric(nrow(history))
for(i in seq_len(nrow(history))){
  h <- history[i,]
  extrapolate[i] <- h[1]
  m <- 1
  while(any(diff(h) != 0)){
    h <- diff(h)
    m <- m * (-1)
    extrapolate[i] <- extrapolate[i] + h[1] * m
  }
}
sum(extrapolate)