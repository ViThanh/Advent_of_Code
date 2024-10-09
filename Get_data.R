# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Topic   : Advent of Code
# Purpose : Get data
# Comment : This code is a modified code taken from https://colin-fraser.net/post/a-quick-tutorial-on-importing-data-from-advent-of-code-into-r/
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
.aoc_get_response <- function(day, year,
                              session_cookie = rstudioapi::askForSecret("Advent of Code Session Cookie")){
  aoc_url <- glue::glue("adventofcode.com/{year}/day/{day}/input")
  cookie <- httr::set_cookies(session = session_cookie)
  response <- httr::GET(aoc_url, cookie)
  return(response)
}
aoc_get_response <- memoise::memoise(.aoc_get_response)
