# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Topic : Advent of Code 2022
# Day   : 01
# Theme : Calorie Counting
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Load data ---------------------------------------------------------------
if(!("day" %in% ls())){
  day <- ""
  while(!(day %in% 1:25)){
    day <- readline("Day: ")
  }
}
path <- rstudioapi::getSourceEditorContext()$path
source(paste0(substr(path, 1, regexpr("Day", path) - 1), "Get_data.R"))
input <- aoc_get_response(day, session_cookie = keyring::key_get("RStudio Keyring Secrets", "Advent of Code Session Cookie")) |>
  httr::content(encoding = 'UTF-8') |>
  readr::read_lines() |>
  as.numeric()

# Part One ----------------------------------------------------------------
calories <- input
elfs <- which(is.na(calories))
calories[is.na(calories)] <- 0
calories <- cumsum(calories)
calories <- c(calories[elfs - 1], calories[length(calories)])
calories <- diff(calories)
cat("The Elf carrying the most calories is carrying", max(calories), "calories.\n")

# Part Two ----------------------------------------------------------------
