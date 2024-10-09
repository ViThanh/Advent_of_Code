# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Topic : Advent of Code 2015
# Day   : 3
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Load data ---------------------------------------------------------------
source("Get_data.R")
input <- aoc_get_response(3, year = 2015, session_cookie = keyring::key_get("RStudio Keyring Secrets", "Advent of Code Session Cookie")) |>
  httr::content(encoding = 'UTF-8')

# Part 1 ------------------------------------------------------------------
directions <- strsplit(input, "")[[1]]
ConvertDirections <- \(directions){
  t(sapply(directions, \(x){
    if(x == "^"){
      c(0, 1)
    }else if(x == ">"){
      c(1, 0)
    }else if(x == "v"){
      c(0, -1)
    }else{
      c(-1, 0)
    }
  }))
}
directions <- ConvertDirections(directions)
directions <- rbind(c(0, 0), directions)
houses <- apply(directions, 2, cumsum)
houses <- apply(houses, 1, paste, collapse = ",")
houses <- unique(houses)
cat(length(houses), "houses received at least one present")

# Part 2 ------------------------------------------------------------------
directions <- strsplit(input, "")[[1]]
directions_split <- lapply(1:2, \(x) directions[(seq_along(directions) %% 2) == (x %% 2)])
directions_split <- lapply(directions_split, \(x) ConvertDirections(x))
houses <- c(sapply(directions_split, \(x){
  directions <- rbind(c(0, 0), x)
  houses <- apply(directions, 2, cumsum)
  houses <- apply(houses, 1, paste, collapse = ",")
}))
houses <- unique(houses)
cat(length(houses), "houses received at least one present")