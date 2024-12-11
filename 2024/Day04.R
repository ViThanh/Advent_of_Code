# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Topic : Advent of Code 2024
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Load data ---------------------------------------------------------------
source("Get_data.R")
input <- aoc_get_response(4, year = 2024, session_cookie = keyring::key_get("RStudio Keyring Secrets", "Advent of Code Session Cookie")) |>
  httr::content(encoding = 'UTF-8') |>
  readr::read_lines()

# Part 1 ------------------------------------------------------------------
cross_word <- do.call(rbind, strsplit(input, ""))
cross_word_padding <- array(0, dim = dim(cross_word) + 6)
cross_word_padding[3 + seq_len(dim(cross_word)[1]), 3 + seq_len(dim(cross_word)[2])] <- cross_word
Xs <- which(cross_word_padding == "X", arr.ind = TRUE)
xmas_count <- 0
for(i in seq_len(nrow(Xs))){
  idx <- Xs[i,]
  mini_cross <- cross_word_padding[idx[1] + (-3:3), idx[2] + (-3:3)]
  xmas <- list(diag(mini_cross)[1:4], diag(mini_cross)[4:7],
               diag(mini_cross[,7:1])[1:4], diag(mini_cross[,7:1])[4:7],
               mini_cross[4, 1:4], mini_cross[4, 4:7],
               mini_cross[1:4, 4], mini_cross[4:7, 4])
  xmas_count <- xmas_count + sum(sapply(xmas, \(x) (paste(x, collapse = "") == "XMAS") | (paste(rev(x), collapse = "") == "XMAS")))
}
cat("XMAS appears", xmas_count, "times", "\n")

# Part 2 ------------------------------------------------------------------
cross_word_padding <- array(0, dim = dim(cross_word) + 2)
cross_word_padding[1 + seq_len(dim(cross_word)[1]), 1 + seq_len(dim(cross_word)[2])] <- cross_word
As <- which(cross_word_padding == "A", arr.ind = TRUE)
xmas_count <- 0
for(i in seq_len(nrow(As))){
  idx <- As[i,]
  mini_cross <- cross_word_padding[idx[1] + (-1:1), idx[2] + (-1:1)]
  xmas <- list(diag(mini_cross), diag(mini_cross[3:1,]))
  xmas_count <- xmas_count + (sum(sapply(xmas, \(x) (paste(x, collapse = "") == "MAS") | (paste(rev(x), collapse = "") == "MAS"))) == 2)
}
cat("X-MAS appears", xmas_count, "times", "\n")