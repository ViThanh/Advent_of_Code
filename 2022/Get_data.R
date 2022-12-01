# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Topic   : Advent of Code 2022
# Purpose : Get data
# Comment : This code is a modified code taken from https://colin-fraser.net/post/a-quick-tutorial-on-importing-data-from-advent-of-code-into-r/
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
aoc_build_url <- function(day, year = 2022){
  formatted_url <- glue::glue("adventofcode.com/{year}/day/{day}/input")
  return(formatted_url)
}
aoc_build_url(1)
.aoc_get_response <- function(day, 
                              session_cookie = rstudioapi::askForSecret("Advent of Code Session Cookie"), 
                              year = 2022){
  aoc_url <- aoc_build_url(day, year)
  cookie <- httr::set_cookies(session = session_cookie)
  response <- httr::GET(aoc_url, cookie)
  return(response)
}
path <- rstudioapi::getSourceEditorContext()$path
path <- substr(path, 1, regexpr("Day", path) - 1)
aoc_get_response <- memoise::memoise(.aoc_get_response, cache = memoise::cache_filesystem(paste0(path, ".aoc")))
