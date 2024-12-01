# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Topic : Advent of Code 2015
# Day   : 12
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Load data ---------------------------------------------------------------
source("Get_data.R")
input <- aoc_get_response(12, year = 2015, session_cookie = keyring::key_get("RStudio Keyring Secrets", "Advent of Code Session Cookie")) |>
  httr::content(encoding = 'UTF-8') |>
  readr::read_lines()

# Part 1 ------------------------------------------------------------------
storage <- jsonlite::fromJSON(input)
unlisted <- unlist(storage)
numbers <- as.numeric(grep("[0-9]", unlisted, value = TRUE))
cat("The sum of all numbers in the document is", sum(numbers), "\n")

# Part 2 ------------------------------------------------------------------
red <- names(unlisted)[unlisted == "red"]
numbers <- as.numeric(grep("[0-9]", unlisted[!(names(unlisted) %in% red)], value = TRUE))
cat("The sum of all numbers in the document is", sum(numbers), "\n")