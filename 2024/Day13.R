# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Topic : Advent of Code 2024
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Load data ---------------------------------------------------------------
source("Get_data.R")
options(scipen = 999)
input <- aoc_get_response(13, year = 2024, session_cookie = keyring::key_get("RStudio Keyring Secrets", "Advent of Code Session Cookie")) |>
  httr::content(encoding = 'UTF-8') |>
  readr::read_lines()

# Part 1 ------------------------------------------------------------------
A <- do.call(rbind, lapply(strsplit(input[seq_len((length(input) + 1) / 4) * 4 - 3], "(Button A: X+)|(, Y+)"), \(x) as.numeric(x[-1])))
B <- do.call(rbind, lapply(strsplit(input[seq_len((length(input) + 1) / 4) * 4 - 2], "(Button B: X+)|(, Y+)"), \(x) as.numeric(x[-1])))
prize <- do.call(rbind, lapply(strsplit(input[seq_len((length(input) + 1) / 4) * 4 - 1], "(Prize: X=)|(, Y=)"), \(x) as.numeric(x[-1])))
winning <- array(Inf, dim = nrow(prize))
for(i in seq_len(nrow(A))){
  p <- prize[i,]
  a <- A[i,]
  b <- B[i,]
  for(j in 1:100){
    win <- all((p - j * a) %% b == 0)
    if(win){
      k <- unique((p - j * a) %/% b)
      if(length(k) == 1)
        winning[i] <- min(c(winning[i], j * 3 + k))
    }
  }
}
cat("The fewest tokens needed to win all possible prizes is", sum(winning[is.finite(winning)]), "\n")

# Part 2 ------------------------------------------------------------------
prize <- do.call(rbind, lapply(strsplit(input[seq_len((length(input) + 1) / 4) * 4 - 1], "(Prize: X=)|(, Y=)"), \(x) as.numeric(x[-1]))) + 10000000000000
winning <- array(Inf, dim = nrow(prize))
for(i in seq_len(nrow(A))){
  p <- prize[i,]
  a <- A[i,]
  b <- B[i,]
  j <- (p[1] * b[2] - p[2] * b[1]) / (a[1] * b[2] - b[1] * a[2])
  if((j %% 1) == 0){
    k <- unique((p - j * a) %/% b)
    if(length(k) == 1) winning[i] <- min(c(winning[i], j * 3 + k))
  }
}
cat("The fewest tokens needed to win all possible prizes is", sum(winning[is.finite(winning)]), "\n")