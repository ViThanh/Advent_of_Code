# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Author: Vi Thanh Pham
# Date: 2023-12-01
# Script Name: Advent of Code - Day 10
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Load input --------------------------------------------------------------
path <- strsplit(rstudioapi::getSourceEditorContext()$path, "/")[[1]]
day <- gsub("(Day)|(.R)", "", path[length(path)])
path <- paste0(sub("Code/", "", paste0(paste(head(path, -1), collapse = "/"), "/")),
               "Data/input", day, ".txt")
input <- unlist(read.table(path, sep = "\n"))

# Part 1 ------------------------------------------------------------------
tiles <- do.call(rbind, strsplit(input, ""))
coords <- matrix(c(1, 0, 0, 1, -1, 0, 0, -1), ncol = 2, byrow = T)
pipes <- matrix(c("|", "L", "J", "-", "7", "J", "|", "7", "F", "-", "L", "F"), ncol = 3, byrow = T)
pos <- which(tiles == "S", arr.ind = T)
step <- 0
loop <- array(dim = dim(tiles))
loop[pos] <- step
while((nrow(pos) > 1) | all((tiles[pos] == "S"))){
  step <- step + 1
  pipe <- tiles[pos]
  P <- matrix(ncol = 2, nrow = 0)
  for(i in seq_along(pipe)){
    for(j in 1:4){
      coord <- pos[i,, drop = F] + coords[j,, drop = F]
      if(all(coord != 0)){
        if(is.na(loop[coord])){
          if(tiles[coord] %in% pipes[j,]){
            if(pipe[i] %in% c("S", pipes[which(apply(t(coords) == -coords[j,], 2, sum) == 2),])){
              loop[coord] <- step
              P <- rbind(P, coord)
            }
          }
        }
      }
    }
  }
  pos <- P
}
step

# Part 2 ------------------------------------------------------------------
pos <- which(tiles == "S", arr.ind = T)
neighbourhood <- loop[-1:1 + pos[,1], -1:1 + pos[,2]]
connect <- which(neighbourhood == 1)
if(all(connect == c(4, 6))){
  pipe <- "|"
}else if(all(connect == c(2, 8))){
  pipe <- "-"
}else if(all(connect == c(4, 8))){
  pipe <- "L"
}else if(all(connect == c(6, 8))){
  pipe <- "F"
}else if(all(connect == c(2, 6))){
  pipe <- "7"
}else if(all(connect == c(2, 4))){
  pipe <- "J"
}
tiles[pos] <- pipe
loop_pipes <- !is.na(loop)
for(i in seq_len(nrow(loop))){
  for(j in seq_len(ncol(loop))){
    if(is.na(loop[i, j])){
      if((i %in% c(1, nrow(loop))) | (j %in% c(1, ncol(loop)))){
        loop[i, j] <- "O"
      }else if(any(loop[-1:1 + i, -1:1 + j][which(!is.na(loop[-1:1 + i, -1:1 + j]))] == "O")){
        loop[i, j] <- "O"
      }else if(any(loop[-1:1 + i, -1:1 + j][which(!is.na(loop[-1:1 + i, -1:1 + j]))] == "I")){
        loop[i, j] <- "I"
      }else{
        pipes <- tiles[1:(i - 1), j][loop_pipes[1:(i - 1), j]]
        if(length(pipes) == 0){
          loop[i, j] <- "O"
        }else{
          pipes <- pipes[pipes != "|"]
          cross <- sum(pipes == "-")
          pipes <- pipes[pipes != "-"]
          pipes <- t(matrix(pipes, ncol = 2, byrow = T))
          cross <- cross + sum(apply(pipes == c("7", "L"), 2, sum) == 2) + sum(apply(pipes == c("F", "J"), 2, sum) == 2)
          if((cross %% 2) == 0){
            loop[i, j] <- "O"
          }else{
            loop[i, j] <- "I"
          }
        }
      }
    }
  }
}
sum(loop == "I")