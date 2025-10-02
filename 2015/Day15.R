# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Topic : Advent of Code 2015
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Load data ---------------------------------------------------------------
source("Get_data.R")
options(scipen = 999)
input <- aoc_get_response(15, year = 2015, session_cookie = keyring::key_get("RStudio Keyring Secrets", "Advent of Code Session Cookie")) |>
  httr::content(encoding = 'UTF-8') |>
  readr::read_lines()

# Part 1 ------------------------------------------------------------------
ingredients <- do.call(rbind, lapply(strsplit(input, "(: )|( )|(, )"), \(x) x[(seq_len(length(x) %/% 2 + 1) - 1) * 2 + 1]))
ingredients <- data.table::data.table(ingredients[, 1], array(as.numeric(as.matrix(ingredients[, -1])), dim = c(dim(ingredients)[1], dim(ingredients)[2] - 1)))
colnames(ingredients) <- c("ingredient", strsplit(input, "(: )|( )|(, )")[[1]][seq_len(ncol(ingredients) - 1) * 2])
spoons <- data.table::data.table(expand.grid(rep(list(0:100), nrow(ingredients))))
colnames(spoons) <- ingredients$ingredient
spoons <- spoons[apply(spoons, 1, sum) == 100,]




spoons <- 100
max_score <- 0

for(i in c(0, seq_len(spoons))){
  for(j in c(0, seq_len(spoons - i))){
    for(k in c(0, seq_len(spoons - i - j))){
      for(l in c(0, seq_len(spoons - i - j - k))){
        print(paste(i, j, k, l))
        score <- as.matrix(ingredients[, .(capacity, durability, flavor, texture)]) %*% matrix(c(i, j, k, l), ncol = 1)
        score <- ifelse(score < 0, 0, score)
        max_score <- max(c(max_score, prod(score)))
      }
    }
  }
}
cat("The total score of the highest-scoring cookie is", max_score, "\n")

# Part 2 ------------------------------------------------------------------
distance <- sapply(reindeers, \(x) cumsum(rep(rep(c(x$speed, 0), c(x$time, x$rest)), travel_time %/% (x$time + x$rest) + 1)[seq_len(travel_time)]))
points <- apply(apply(distance, 1, \(x) x == max(x)), 1, sum)
cat("The winning reindeer has", max(points), "points", "\n")
