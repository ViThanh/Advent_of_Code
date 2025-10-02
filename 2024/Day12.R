# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Topic : Advent of Code 2024
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Load data ---------------------------------------------------------------
source("Get_data.R")
options(scipen = 999)
input <- aoc_get_response(12, year = 2024, session_cookie = keyring::key_get("RStudio Keyring Secrets", "Advent of Code Session Cookie")) |>
  httr::content(encoding = 'UTF-8') |>
  readr::read_lines()

# Part 1 ------------------------------------------------------------------
garden <- do.call(rbind, strsplit(input, ""))
garden_padded <- array(".", dim = dim(garden) + 2)
garden_padded[seq_len(dim(garden)[1]) + 1, seq_len(dim(garden)[2]) + 1] <- garden
plants <- unique(c(garden))
fence <- numeric(length(plants))
for(i in seq_along(plants)){
  p <- plants[i]
  plots <- which(garden_padded == p, arr.ind = TRUE)
  while(nrow(plots) > 0){
    plant <- same_plot <- plots[1,, drop = FALSE]
    plots <- plots[-1,, drop = FALSE]
    neighbor <- TRUE
    while(any(neighbor)){
      neighbor <- apply(matrix(apply(same_plot, 1, \(x) apply(abs(t(plots) - x), 2, sum) == 1), nrow = nrow(plots)), 1, any)
      same_plot <- plots[neighbor,, drop = FALSE]
      plant <- rbind(plant, same_plot)
      plots <- plots[!neighbor,, drop = FALSE]
    }
    hedge <- sum(apply(plant, 1, \(loc){
      area <- garden_padded[loc[1] + (-1:1), loc[2] + (-1:1)]
      diag(area) <- diag(area[,3:1]) <- p
      sum(area != p)
    }))
    fence[i] <- fence[i] + nrow(plant) * hedge
  }
}
cat("The total price of fencing all regions on the map is", sum(fence), "\n")

# Part 2 ------------------------------------------------------------------
fence <- numeric(length(plants))
for(i in seq_along(plants)){
  p <- plants[i]
  plots <- which(garden_padded == p, arr.ind = TRUE)
  while(nrow(plots) > 0){
    plant <- same_plot <- plots[1,, drop = FALSE]
    plots <- plots[-1,, drop = FALSE]
    neighbor <- TRUE
    while(any(neighbor)){
      neighbor <- apply(matrix(apply(same_plot, 1, \(x) apply(abs(t(plots) - x), 2, sum) == 1), nrow = nrow(plots)), 1, any)
      same_plot <- plots[neighbor,, drop = FALSE]
      plant <- rbind(plant, same_plot)
      plots <- plots[!neighbor,, drop = FALSE]
    }
    sides <- sum(apply(plant, 1, \(loc){
      area <- area_aux <- garden_padded[loc[1] + (-1:1), loc[2] + (-1:1)]
      diag(area_aux) <- diag(area_aux[,3:1]) <- "."
      side <- ifelse(sum(area_aux == p) == 1, 2, ifelse(sum(area_aux == p) == 0, 4, 0))
      area_aux <- area
      for(j in 1:4){
        if((sum(area_aux[1:2, 1:2] == p) == 4) & all(area_aux[2, 3] != p) & all(area_aux[3, 2] != p)){
          side <- side + 1
        }
        area_aux <- t(apply(area_aux, 2, rev))
      }
      for(j in 1:4){
        if((sum(area_aux[1:2, 1:2] == p) == 3) & all(area_aux[2, 3] != p) & all(area_aux[3, 2] != p) & all(area_aux[1, 1] != p)){
          side <- side + 1
        }
        area_aux <- t(apply(area_aux, 2, rev))
      }
      for(j in 1:4){
        if(sum(area_aux[1:2, 1:2] == p) == 3){
          area_aux_aux <- area_aux[1:2, 1:2]
          diag(area_aux_aux) <- "."
          side <- side + as.numeric((sum(area_aux_aux == p) == 2))
        }
        area_aux <- t(apply(area_aux, 2, rev))
      }
      side
    }))
    fence[i] <- fence[i] + nrow(plant) * sides
  }
}
cat("The total price of fencing all regions on the map is", sum(fence), "\n")