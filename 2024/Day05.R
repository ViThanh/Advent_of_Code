# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Topic : Advent of Code 2024
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Load data ---------------------------------------------------------------
source("Get_data.R")
input <- aoc_get_response(5, year = 2024, session_cookie = keyring::key_get("RStudio Keyring Secrets", "Advent of Code Session Cookie")) |>
  httr::content(encoding = 'UTF-8') |>
  readr::read_lines()

# Part 1 ------------------------------------------------------------------
split <- which(input == "")
rules <- do.call(rbind, lapply(strsplit(input[1:(split - 1)], "|", fixed = TRUE), \(x) as.numeric(x)))
updates <- lapply(strsplit(input[(split + 1):length(input)], ","), \(x) as.numeric(x))
correct_updates <- logical(length(updates))
for(i in seq_along(updates)){
  update <- updates[[i]]
  correct <- TRUE
  for(j in seq_along(update)){
    x <- update[j]
    rule <- rules[(rules[, 1] == x) | (rules[, 2] == x),]
    previous <- update[seq_len(j - 1)]
    following <- update[seq_len(length(update) - j) + j]
    correct <- correct & (length(intersect(rule[, 2], previous)) == 0) & (length(intersect(rule[, 1], following)) == 0)
  }
  correct_updates[i] <- correct
}
middle <- sapply(updates[correct_updates], \(x) x[(length(x) + 1) / 2])
cat("The sum of middle page numbers of correctly-ordered updates is", sum(middle), "\n")

# Part 2 ------------------------------------------------------------------
incorrect_updates <- updates[!correct_updates]
for(i in seq_along(incorrect_updates)){
  update <- incorrect_updates[[i]]
  for(j in seq_along(update)){
    stable <- FALSE
    while(!stable){
      x <- update[j]
      rule <- rules[(rules[, 1] == x) | (rules[, 2] == x),]
      following <- update[seq_len(length(update) - j) + j]
      incorrect <- intersect(rule[, 1], following)
      new_order <- c(update[seq_len(j - 1)], incorrect, setdiff(update, c(update[seq_len(j - 1)], incorrect)))
      stable <- all(update == new_order)
      update <- new_order
    }
  }
  incorrect_updates[[i]] <- update
}
middle <- sapply(incorrect_updates, \(x) x[(length(x) + 1) / 2])
cat("The sum of middle page numbers of correctly-ordered updates is", sum(middle), "\n")