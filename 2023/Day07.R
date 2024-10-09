# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Author: Vi Thanh Pham
# Date: 2023-12-01
# Script Name: Advent of Code - Day 07
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Load input --------------------------------------------------------------
path <- strsplit(rstudioapi::getSourceEditorContext()$path, "/")[[1]]
day <- gsub("(Day)|(.R)", "", path[length(path)])
path <- paste0(sub("Code/", "", paste0(paste(head(path, -1), collapse = "/"), "/")),
               "Data/input", day, ".txt")
input <- unlist(read.table(path, sep = "\n"))

# Part 1 ------------------------------------------------------------------
bids <- strsplit(input, " ")
hand <- do.call(rbind, lapply(bids, \(x) strsplit(x[[1]], "")[[1]]))
bid <- sapply(bids, \(x) as.numeric(x[[2]]))
hand <- ifelse(hand == "T", 10, 
               ifelse(hand == "J", 11, 
                      ifelse(hand == "Q", 12, 
                             ifelse(hand == "K", 13, 
                                    ifelse(hand == "A", 14, hand)))))
strength <- apply(hand, 1, \(x){
  counts <- table(x)
  if(5 %in% counts){
    7
  }else if(4 %in% counts){
    6
  }else if(all(2:3 %in% counts)){
    5
  }else if(all(c(1, 3) %in% counts)){
    4
  }else if(all(1:2 %in% counts) & (length(counts) == 3)){
    3
  }else if(length(counts) == 4){
    2
  }else if(length(counts) == 5){
    1
  }
})
hands <- cbind(hand, strength, bid)
storage.mode(hands) <- "numeric"
hands <- data.frame(hands)
hands <- hands[order(hands[, "strength"], hands[, 1], hands[, 2], hands[, 3], hands[, 4], hands[, 5]),]
hands <- cbind(hands, rank = seq_len(nrow(hands)))
sum(hands$bid * hands$rank)

# Part 2 ------------------------------------------------------------------
hand <- do.call(rbind, lapply(bids, \(x) strsplit(x[[1]], "")[[1]]))
bid <- sapply(bids, \(x) as.numeric(x[[2]]))
hand <- ifelse(hand == "T", 10, 
               ifelse(hand == "J", 1, 
                      ifelse(hand == "Q", 12, 
                             ifelse(hand == "K", 13, 
                                    ifelse(hand == "A", 14, hand)))))
strength <- apply(hand, 1, \(x){
  counts <- table(x)
  cards <- names(counts)
  if("1" %in% cards){
    count_j <- counts[cards == "1"]
    counts <- counts[-which(cards == "1")]
    if(length(counts) == 0){
      counts <- 5
    }else{
      counts <- sort(counts, decreasing = T)
      counts[1] <- counts[1] + count_j
    }
  }
  if(5 %in% counts){
    7
  }else if(4 %in% counts){
    6
  }else if(all(2:3 %in% counts)){
    5
  }else if(all(c(1, 3) %in% counts)){
    4
  }else if(all(1:2 %in% counts) & (length(counts) == 3)){
    3
  }else if(length(counts) == 4){
    2
  }else if(length(counts) == 5){
    1
  }
})
hands <- cbind(hand, strength, bid)
storage.mode(hands) <- "numeric"
hands <- data.frame(hands)
hands <- hands[order(hands[, "strength"], hands[, 1], hands[, 2], hands[, 3], hands[, 4], hands[, 5]),]
hands <- cbind(hands, rank = seq_len(nrow(hands)))
sum(hands$bid * hands$rank)