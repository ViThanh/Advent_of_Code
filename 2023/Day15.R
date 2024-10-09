# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Author: Vi Thanh Pham
# Script Name: Advent of Code - Day 15
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
hash <- strsplit(input, ",")[[1]]
h <- numeric(length(hash))
Hash <- \(x){
  H <- strsplit(x, "")[[1]]
  h <- 0
  for(i in H){
    h <- h + as.numeric(charToRaw(i))
    h <- h * 17
    h <- h %% 256
  }
  return(h)
}
for(i in seq_along(hash)){
  h[i] <- Hash(hash[i])
}
sum(h)

# Part 2 ------------------------------------------------------------------
hash <- strsplit(input, ",")[[1]]
boxes <- lapply(seq_len(256), \(x) matrix(ncol = 2, nrow = 0))
for(i in hash){
  command <- regexpr("(=)|(-)", i)
  label <- substr(i, 1, command - 1)
  operation <- substr(i, command, command)
  lense <- ifelse(operation == "=", substr(i, command + 1, nchar(i)), "")
  h <- Hash(label) + 1
  if(operation == "="){
    if(label %in% boxes[[h]][, 1]){
      boxes[[h]][boxes[[h]][, 1] == label, 2] <- lense
    }else{
      boxes[[h]] <- rbind(boxes[[h]], c(label, lense))
    }
  }else{
    if(label %in% boxes[[h]][, 1]) boxes[[h]] <- boxes[[h]][-which(boxes[[h]][, 1] == label),, drop = F]
  }
}
focusing_power <- sapply(boxes, \(x) sum(as.numeric(x[, 2]) * seq_along(x[, 2])))
sum(focusing_power * seq_along(focusing_power))