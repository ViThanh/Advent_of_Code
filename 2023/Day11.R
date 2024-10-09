# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Author: Vi Thanh Pham
# Date: 2023-12-01
# Script Name: Advent of Code - Day 11
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Load input --------------------------------------------------------------
path <- strsplit(rstudioapi::getSourceEditorContext()$path, "/")[[1]]
day <- gsub("(Day)|(.R)", "", path[length(path)])
path <- paste0(sub("Code/", "", paste0(paste(head(path, -1), collapse = "/"), "/")),
               "Data/input", day, ".txt")
input <- unlist(read.table(path, sep = "\n", comment.char = ""))
options(scipen = 999)

# Part 1 ------------------------------------------------------------------
image <- do.call(rbind, strsplit(input, ""))
rows <- which(apply(image, 1, \(x) all(x == ".")))
cols <- which(apply(image, 2, \(x) all(x == ".")))
for(i in seq_along(rows)){
  image <- rbind(image, ".")
}
for(i in seq_along(cols)){
  image <- cbind(image, ".")
}
image <- image[order(c(seq_len(nrow(image) - length(rows)), rows)),]
image <- image[, order(c(seq_len(ncol(image) - length(cols)), cols))]
galaxies <- which(image == "#", arr.ind = T)
lengths <- 0
for(i in seq_len(nrow(galaxies) - 1)){
  for(j in (i + 1):nrow(galaxies)){
    lengths <- lengths + sum(abs(galaxies[i,] - galaxies[j,]))
  }
}
lengths

# Part 2 ------------------------------------------------------------------
image <- do.call(rbind, strsplit(input, ""))
galaxies <- which(image == "#", arr.ind = T)
rows <- which(apply(image, 1, \(x) all(x == ".")))
cols <- which(apply(image, 2, \(x) all(x == ".")))
lengths <- 0
multiplier <- 10 ^ 6 - 1
for(i in seq_len(nrow(galaxies) - 1)){
  for(j in (i + 1):nrow(galaxies)){
    lengths <- lengths + sum(abs(galaxies[i,] - galaxies[j,])) + 
      sum(data.table::between(rows, min(galaxies[i, 1], galaxies[j, 1]), max(galaxies[i, 1], galaxies[j, 1]))) * multiplier +
      sum(data.table::between(cols, min(galaxies[i, 2], galaxies[j, 2]), max(galaxies[i, 2], galaxies[j, 2]))) * multiplier
  }
}
lengths
