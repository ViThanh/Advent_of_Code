# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Author: Vi Thanh Pham
# Script Name: Advent of Code
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
contraption <- do.call(rbind, strsplit(input, ""))
energized <- array(".", dim = dim(contraption))
dot <- \(direction, i, j){
  if(direction == ">"){
    j <- j + 1
  }else if(direction == "v"){
    i <- i + 1
  }else if(direction == "<"){
    j <- j - 1
  }else if(direction == "^"){
    i <- i - 1
  }
  return(list(direction = direction, i = i, j = j))
}
i <- 1
j <- 1
direction <- ">"
tile <- contraption[i, j]
while((i > 0) & (i <= nrow(contraption)) & (j > 0) & (j <= ncol(contraption))){
  if(tile == "."){
    move <- dot(direction, i, j)
    direction <- move$direction
    i <- move$i
    j <- move$j
  }else if(tile == "\\"){
    if(direction == ">"){
      i <- i + 1
      direction <- "v"
    }else if(direction == "v"){
      j <- j + 1
      direction <- ">"
    }else if(direction == "<"){
      i <- i - 1
      direction <- "^"
    }else if(direction == "^"){
      j <- j - 1
      direction <- "<"
    }
  }else if(tile == "/"){
    if(direction == ">"){
      i <- i - 1
      direction <- "^"
    }else if(direction == "v"){
      j <- j - 1
      direction <- "<"
    }else if(direction == "<"){
      i <- i + 1
      direction <- "v"
    }else if(direction == "^"){
      j <- j + 1
      direction <- ">"
    }
  }else if(tile == "|")
}


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