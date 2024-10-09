# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Author: Vi Thanh Pham
# Date: 2023-12-01
# Script Name: Advent of Code - Day 02
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Load input --------------------------------------------------------------
path <- strsplit(rstudioapi::getSourceEditorContext()$path, "/")[[1]]
day <- gsub("(Day)|(.R)", "", path[length(path)])
path <- paste0(sub("Code/", "", paste0(paste(head(path, -1), collapse = "/"), "/")),
               "Data/input", day, ".txt")
input <- unlist(read.table(path, sep = "\n"))

# Part 1 ------------------------------------------------------------------
colors <- c("red", "green", "blue")
games <- substr(input, regexpr(":", input) + 2, nchar(input))
games <- strsplit(games, "; ")
games <- lapply(games, \(x) strsplit(x, ", "))
games <- lapply(games, \(x) lapply(x, \(y) strsplit(paste(y, collapse = " "), " ")[[1]]))
games <- lapply(games, \(x) lapply(x, \(y){
  l <- length(y)
  balls <- as.numeric(y[seq_len(l / 2) * 2 - 1])
  names(balls) <- y[seq_len(l / 2) * 2]
  balls <- data.frame(t(balls))
  miss_col <- setdiff(colors, names(balls))
  if(length(miss_col) != 0){
    miss_balls <- numeric(length(miss_col))
    names(miss_balls) <- miss_col
    balls <- cbind(balls, data.frame(t(miss_balls)))
  }
  balls
}))
games <- lapply(games, \(x) do.call(rbind, x))
sum(which(sapply(games, \(x) all(x[, "red"] <= 12) & 
                   all(x[, "green"] <= 13) & 
                   all(x[, "blue"] <= 14))))

# Part 2 ------------------------------------------------------------------
sum(sapply(games, \(x) prod(apply(x, 2, max))))