# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Author: Vi Thanh Pham
# Date: 2023-12-01
# Script Name: Advent of Code - Day 06
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Load input --------------------------------------------------------------
path <- strsplit(rstudioapi::getSourceEditorContext()$path, "/")[[1]]
day <- gsub("(Day)|(.R)", "", path[length(path)])
path <- paste0(sub("Code/", "", paste0(paste(head(path, -1), collapse = "/"), "/")),
               "Data/input", day, ".txt")
input <- unlist(read.table(path, sep = "\n"))

# Part 1 ------------------------------------------------------------------
records <- strsplit(gsub("(Time:)|(Distance:)", "", input), " ")
times <- as.numeric(records[[1]][records[[1]] != ""])
distances <- as.numeric(records[[2]][records[[2]] != ""])
wins <- numeric(length(times))
for(i in seq_along(wins)){
  t <- times[i]
  d <- distances[i]
  time <- seq_len(t)
  wins[i] <- sum((t - time) * time > d)
}
prod(wins)

# Part 2 ------------------------------------------------------------------
time <- as.numeric(paste(times, collapse = ""))
distance <- as.numeric(paste(distances, collapse = ""))
t <- seq_len(time)
sum((time - t) * t > distance)