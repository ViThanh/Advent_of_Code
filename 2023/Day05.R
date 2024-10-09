# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Author: Vi Thanh Pham
# Date: 2023-12-01
# Script Name: Advent of Code - Day 05
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Load input --------------------------------------------------------------
path <- strsplit(rstudioapi::getSourceEditorContext()$path, "/")[[1]]
day <- gsub("(Day)|(.R)", "", path[length(path)])
path <- paste0(sub("Code/", "", paste0(paste(head(path, -1), collapse = "/"), "/")),
               "Data/input", day, ".txt")
input <- unlist(read.table(path, sep = "\n"))

# Part 1 ------------------------------------------------------------------
seeds <- as.numeric(strsplit(substr(input[1], 8, nchar(input[1])), " ")[[1]])
seeds <- data.frame(seed = seeds)
categories <- c("soil", "fertilizer", "water", "light", "temperature", "humidity", "location")
seeds[, categories] <- NA
for(i in setdiff(colnames(seeds), "seed")){
  category <- colnames(seeds)[which((colnames(seeds) == i)) - 1]
  range <- grep(i, input)
  if(i != "location"){
    full_map <- input[(range[1] + 1):(range[2] - 1)]
  }else{
    full_map <- input[(range[1] + 1):length(input)]
  }
  for(j in full_map){
    map <- as.numeric(strsplit(j, " ")[[1]])
    ind <- as.vector((seeds[, category] >= map[2]) & (seeds[, category] < (map[2] + map[3])))
    seeds[ind, i] <- seeds[ind, category] - map[2] + map[1]
  }
  seeds[is.na(seeds[, i]), i] <- seeds[is.na(seeds[, i]), category]
}
min(seeds$location)

# Part 2 ------------------------------------------------------------------
seeds <- as.numeric(strsplit(substr(input[1], 8, nchar(input[1])), " ")[[1]])
n <- length(seeds) / 2
seeds <- data.frame(seed = seeds[seq_len(n) * 2 - 1], end = seeds[seq_len(n) * 2 - 1] + seeds[seq_len(n) * 2] - 1)
seeds[, categories] <- NA
seeds <- cbind(seeds[, setdiff(colnames(seeds), "end")], end = seeds[, "end"])
for(i in categories){
  category <- colnames(seeds)[which((colnames(seeds) == i)) - 1]
  range <- grep(i, input)
  if(i != "location"){
    full_map <- input[(range[1] + 1):(range[2] - 1)]
  }else{
    full_map <- input[(range[1] + 1):length(input)]
  }
  for(j in full_map){
    map <- as.numeric(strsplit(j, " ")[[1]])
    almanac <- data.frame(matrix(nrow = 0, ncol = length(categories) + 2))
    colnames(almanac) <- c("seed", categories, "end")
    for(k in seq_len(nrow(seeds))){
      seed <- seeds[k,]
      start <- seed[, category]
      end <- seed$end
      source_start <- map[2]
      source_end <- map[2] + map[3] - 1
      if(!(((source_start > end) | (source_end < start)) | ((source_start <= start) & (source_end >= end)))){
        if((source_start <= start) & (source_end < end)){
          split <- rbind(seed, seed)
          split[1, "end"] <- source_end
          split[2, category] <- source_end + 1
          almanac <- rbind(almanac, split)
        }else if((source_start > start) & (source_end >= end)){
          split <- rbind(seed, seed)
          split[1, "end"] <- source_start - 1
          split[2, category] <- source_start
          almanac <- rbind(almanac, split)
        }else if((source_start > start) & (source_end < end)){
          split <- rbind(seed, seed, seed)
          split[1, "end"] <- source_start - 1
          split[2, category] <- source_start
          split[2, "end"] <- source_end
          split[3, category] <- source_end + 1
          almanac <- rbind(almanac, split)
        }
      }else{
        almanac <- rbind(almanac, seed)
      }
    }
    seeds <- almanac
  }
  for(j in full_map){
    map <- as.numeric(strsplit(j, " ")[[1]])
    ind <- as.vector((seeds[, category] >= map[2]) & (seeds[, category] < (map[2] + map[3])))
    seeds[ind, i] <- seeds[ind, category] - map[2] + map[1]
    seeds[ind, "end"] <- seeds[ind, "end"] - map[2] + map[1]
  }
  seeds[is.na(seeds[, i]), i] <- seeds[is.na(seeds[, i]), category]
}
min(seeds$location)