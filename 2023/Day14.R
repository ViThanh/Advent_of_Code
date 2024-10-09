# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Author: Vi Thanh Pham
# Date: 2023-12-01
# Script Name: Advent of Code - Day 14
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
platform <- do.call(rbind, strsplit(input, ""))
n <- nrow(platform)
moving <- (platform[-1,] == "O") & (platform[-n,] == ".")
while(any(moving)){
  platform[-1, ][moving] <- "."
  platform[-n,][moving] <- "O"
  moving <- (platform[-1,] == "O") & (platform[-n,] == ".")
}
platform <- ifelse(platform == "O", 1, 0)
platform <- platform[rev(seq_len(nrow(platform))),]
sum(platform * array(seq_len(nrow(platform)), dim = dim(platform)))

# Part 2 ------------------------------------------------------------------
platform <- do.call(rbind, strsplit(input, ""))
n <- nrow(platform)
platforms <- list()
repeated <- F
i <- 0
cycles <- 1000000000
while((i < cycles) & !repeated){
  i <- i + 1
  for(j in 1:4){
    if(j == 1){
      moving <- (platform[-1,] == "O") & (platform[-n,] == ".")
      while(any(moving)){
        platform[-1, ][moving] <- "."
        platform[-n,][moving] <- "O"
        moving <- (platform[-1,] == "O") & (platform[-n,] == ".")
      }
    }else if(j == 2){
      moving <- (platform[, -1] == "O") & (platform[, -n] == ".")
      while(any(moving)){
        platform[, -1][moving] <- "."
        platform[, -n][moving] <- "O"
        moving <- (platform[, -1] == "O") & (platform[,-n] == ".")
      }
    }else if(j == 3){
      moving <- (platform[-n,] == "O") & (platform[-1,] == ".")
      while(any(moving)){
        platform[-n, ][moving] <- "."
        platform[-1,][moving] <- "O"
        moving <- (platform[-n,] == "O") & (platform[-1,] == ".")
      }
    }else if(j == 4){
      moving <- (platform[, -n] == "O") & (platform[, -1] == ".")
      while(any(moving)){
        platform[, -n][moving] <- "."
        platform[, -1][moving] <- "O"
        moving <- (platform[, -n] == "O") & (platform[,-1] == ".")
      }
    }
  }
  repeated <- any(sapply(platforms, \(x) all(x == platform)))
  platforms[[i]] <- platform
}
if(i != cycles){
  repeated <- which(sapply(platforms, \(x) all(x == platform)))[1]
  cycle <- i - repeated
  element <- (cycles - (repeated - 1)) %% cycle
  platform <- platforms[[repeated - 1 + element]]
}
platform <- ifelse(platform == "O", 1, 0)
platform <- platform[rev(seq_len(nrow(platform))),]
sum(platform * array(seq_len(nrow(platform)), dim = dim(platform)))