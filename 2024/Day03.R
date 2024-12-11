# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Topic : Advent of Code 2024
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Load data ---------------------------------------------------------------
source("Get_data.R")
input <- aoc_get_response(3, year = 2024, session_cookie = keyring::key_get("RStudio Keyring Secrets", "Advent of Code Session Cookie")) |>
  httr::content(encoding = 'UTF-8')

# Part 1 ------------------------------------------------------------------
memory <- sapply(strsplit(input, "mul(", fixed = TRUE)[[1]], \(x){
  instructions <- strsplit(x, ",")[[1]]
  first <- instructions[1]
  second <- instructions[2]
  if(grepl("^[0-9]{1,3}$", first)){
    first <- as.numeric(first)
  }
  if(grepl(")", second, fixed = TRUE)){
    second <- substr(second, 1, regexpr(")", second, fixed = TRUE) - 1)
    if(grepl("^[0-9]{1,3}$", second)){
      second <- as.numeric(second)
    }
  }
  if(all(is.numeric(c(first, second)))){
    first * second
  }else{
    0
  }
})
cat("The memory size is", sum(memory), "\n")

# Part 2 ------------------------------------------------------------------
memory <- strsplit(input, "mul(", fixed = TRUE)[[1]]
dos <- unlist(gregexpr("do\\(\\)", memory))
donts <- unlist(gregexpr("don't\\(\\)", memory))
enabled <- TRUE
memory_size <- 0
for(i in seq_along(memory)){
  if(enabled){
    instructions <- strsplit(memory[i], ",")[[1]]
    first <- instructions[1]
    second <- instructions[2]
    if(grepl("^[0-9]{1,3}$", first)){
      first <- as.numeric(first)
    }
    if(grepl(")", second, fixed = TRUE)){
      second <- substr(second, 1, regexpr(")", second, fixed = TRUE) - 1)
      if(grepl("^[0-9]{1,3}$", second)){
        second <- as.numeric(second)
      }
    }
    if(all(is.numeric(c(first, second)))){
      memory_size <- memory_size + first * second
    }
    if(donts[i] > 0) enabled <- FALSE
  }else{
    if(dos[i] > 0) enabled <- TRUE
  }
}
cat("The memory size is", sum(memory_size), "\n")