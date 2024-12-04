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
fixed_input <- input
while(grepl("red", fixed_input)){
  idx <- regexpr("red", fixed_input)
  input_split <- strsplit(fixed_input, "")[[1]]
  input_split_pre <- rev(input_split[1:idx])
  input_split_post <- input_split[idx:length(input_split)]
  arr <- which(cumsum(input_split_pre == "[") > cumsum(input_split_pre == "]"))[1]
  obj <- which(cumsum(input_split_pre == "{") > cumsum(input_split_pre == "}"))[1]
  if(is.na(obj)) obj <- Inf
  if(arr < obj){
    input_split <- input_split[-(idx + 1:2)]
    input_split[idx] <- 0
  }else{
    obj_end <- which(cumsum(input_split_post == "}") > cumsum(input_split_post == "{"))[1]
    input_split <- c(rev(input_split_pre[-(1:obj)]), 0, input_split_post[-(1:obj_end)])
  }
  fixed_input <- paste(input_split, collapse = "")
}
storage <- jsonlite::fromJSON(fixed_input)
unlisted <- unlist(storage)
numbers <- as.numeric(grep("[0-9]", unlisted, value = TRUE))
cat("The sum of all numbers in the document is", sum(numbers), "\n")