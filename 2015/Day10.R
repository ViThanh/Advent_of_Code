# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Topic : Advent of Code 2015
# Day   : 10
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Load data ---------------------------------------------------------------
source("Get_data.R")
input <- aoc_get_response(10, year = 2015, session_cookie = keyring::key_get("RStudio Keyring Secrets", "Advent of Code Session Cookie")) |>
  httr::content(encoding = 'UTF-8') |>
  readr::read_lines()

# Part 1 ------------------------------------------------------------------
sequence_input <- input
for(i in seq_len(40)){
  sequence_input <- as.numeric(strsplit(sequence_input, "")[[1]])
  sequence_input <- paste(c(rbind(ifelse(c(0, diff(sequence_input)) == 0, "", "_"), sequence_input)), collapse = "")
  sequence_input <- strsplit(sequence_input, "_")[[1]]
  sequence_input <- paste(c(rbind(nchar(sequence_input), substr(sequence_input, 1, 1))), collapse = "")
}
cat("The length of the result is", nchar(sequence_input), "\n")

# Part 2 ------------------------------------------------------------------
for(i in seq_len(10)){
  sequence_input <- as.numeric(strsplit(sequence_input, "")[[1]])
  sequence_input <- paste(c(rbind(ifelse(c(0, diff(sequence_input)) == 0, "", "_"), sequence_input)), collapse = "")
  sequence_input <- strsplit(sequence_input, "_")[[1]]
  sequence_input <- paste(c(rbind(nchar(sequence_input), substr(sequence_input, 1, 1))), collapse = "")
}
cat("The length of the result is", nchar(sequence_input), "\n")