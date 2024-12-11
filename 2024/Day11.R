# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Topic : Advent of Code 2024
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Load data ---------------------------------------------------------------
source("Get_data.R")
options(scipen = 999)
input <- aoc_get_response(11, year = 2024, session_cookie = keyring::key_get("RStudio Keyring Secrets", "Advent of Code Session Cookie")) |>
  httr::content(encoding = 'UTF-8') |>
  readr::read_lines()

# Part 1 ------------------------------------------------------------------
Blink <- \(stone, blinks){
  if(blinks == 1){
    if(stone == 0){
      return(1)
    }else if((nchar(stone) %% 2) == 0){
      return(as.numeric(c(substr(stone, 1, nchar(stone) / 2), substr(stone, nchar(stone) / 2 + 1, nchar(stone)))))
    }else{
      return(stone * 2024)
    }
  }else{
    unlist(lapply(Blink(stone, 1), \(stone) Blink(stone, blinks - 1)))
  }
}
Blink <- memoise::memoise(Blink)
stones <- as.numeric(strsplit(input, " ")[[1]])
blinks <- 25
cat("There will be", length(unlist(lapply(stones, \(x) Blink(x, blinks)))), "stones after blinking", blinks, "times", "\n")

# Part 2 ------------------------------------------------------------------
Stones <- \(stone, blinks){
  if(blinks == 1){
    length(Blink(stone, 1))
  }else{
    sum(sapply(Blink(stone, 1), \(x) Stones(x, blinks - 1)))
  }
}
Stones <- memoise::memoise(Stones)
blinks <- 75
cat("There will be", sum(sapply(stones, \(x) Stones(x, blinks))), "stones after blinking", blinks, "times", "\n")