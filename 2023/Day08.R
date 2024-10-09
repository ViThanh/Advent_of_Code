# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Author: Vi Thanh Pham
# Date: 2023-12-01
# Script Name: Advent of Code - Day 08
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Load input --------------------------------------------------------------
path <- strsplit(rstudioapi::getSourceEditorContext()$path, "/")[[1]]
day <- gsub("(Day)|(.R)", "", path[length(path)])
path <- paste0(sub("Code/", "", paste0(paste(head(path, -1), collapse = "/"), "/")),
               "Data/input", day, ".txt")
input <- unlist(read.table(path, sep = "\n"))

# Part 1 ------------------------------------------------------------------
instructions <- strsplit(input[1], "")[[1]]
nodes <- do.call(rbind, strsplit(gsub(")", "", gsub("(", "", input[-1], fixed = T), fixed = T), "( = )|(, )"))
i <- 0
l <- length(instructions)
node <- "AAA"
while(node != "ZZZ"){
  i <- i + 1
  ind <- i %% l
  instr <- instructions[if(ind != 0) ind else l]
  node <- nodes[which(nodes[,1] == node), if(instr == "L") 2 else 3]
}
i

# Part 2 ------------------------------------------------------------------
instructions <- strsplit(input[1], "")[[1]]
nodes <- do.call(rbind, strsplit(gsub(")", "", gsub("(", "", input[-1], fixed = T), fixed = T), "( = )|(, )"))
i <- 0
l <- length(instructions)
node <- nodes[which(substr(nodes[, 1], 3, 3) == "A"), 1]
ends <- numeric(length(node))
for(n in seq_along(node)){
  i <- 0
  while(substr(node[n], 3, 3) != "Z"){
    i <- i + 1
    ind <- i %% l
    instr <- instructions[if(ind != 0) ind else l]
    node[n] <- nodes[which(nodes[,1] == node[n]), if(instr == "L") 2 else 3]
  }
  ends[n] <- i
}
options(scipen = 999)
numbers::mLCM(ends)