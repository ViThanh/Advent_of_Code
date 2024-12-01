# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Topic : Advent of Code 2015
# Day   : 8
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Load data ---------------------------------------------------------------
source("Get_data.R")
input <- aoc_get_response(8, year = 2015, session_cookie = keyring::key_get("RStudio Keyring Secrets", "Advent of Code Session Cookie")) |>
  httr::content(encoding = 'UTF-8') |>
  readr::read_lines()

# Part 1 ------------------------------------------------------------------
santas_list <- gsub("__", "-", 
                    gsub("(_x[abcdef0-9]{2})", ">", 
                         gsub("\\", "_", fixed = TRUE, 
                              gsub('\"', "=", fixed = TRUE,
                                   gsub('\\\"', "<", fixed = TRUE, input)))))
in_memory <- nchar(santas_list) - 2
literals <- in_memory + 2 + 
  stringr::str_count(santas_list, pattern = "-") + 
  stringr::str_count(santas_list, pattern = "<") +
  stringr::str_count(santas_list, pattern = ">") * 3
cat("The size of the digital copy Santa's list is", sum(literals) - sum(in_memory), "\n")

# Part 2 ------------------------------------------------------------------
new_literals <- nchar(santas_list) + 2 +
  stringr::str_count(santas_list, pattern = "=") + 
  stringr::str_count(santas_list, pattern = "-") * 3 + 
  stringr::str_count(santas_list, pattern = "<") * 3 + 
  stringr::str_count(santas_list, pattern = ">") * 4 + 
  stringr::str_count(santas_list, pattern = "_")
cat("The size of the digital copy Santa's list is", sum(new_literals) - sum(literals), "\n")