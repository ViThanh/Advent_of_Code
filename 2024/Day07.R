# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Topic : Advent of Code 2024
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Load data ---------------------------------------------------------------
source("Get_data.R")
input <- aoc_get_response(7, year = 2024, session_cookie = keyring::key_get("RStudio Keyring Secrets", "Advent of Code Session Cookie")) |>
  httr::content(encoding = 'UTF-8') |>
  readr::read_lines()

# Part 1 ------------------------------------------------------------------
bridge <- lapply(strsplit(input, "(: )|( )"), \(x) as.numeric(x))
test_results <- sapply(bridge, \(x) x[1])
calibration <- lapply(bridge, \(x) x[-1])
possible_equations <- sapply(seq_along(test_results), \(x){
  operators <- calibration[[x]]
  operations <- expand.grid(rep(list(c("+", "*")), length(operators) - 1))
  equations <- t(apply(operations, 1, \(y) rbind(operators, c("", rep(")", ncol(operations))), c(y, ""))))
  equations <- cbind(paste(rep("(", ncol(operations)), collapse = ""), equations)
  equations <- apply(equations, 1, \(y) eval(parse(text = paste(y, collapse = ""))))
  if(test_results[x] %in% equations) test_results[x] else 0
})
cat("The total calibration result is", sum(possible_equations), "\n")

# Part 2 ------------------------------------------------------------------
new_test <- which(possible_equations == 0)
new_possible_equation <- sapply(new_test, \(x){
  operators <- calibration[[x]]
  operations <- expand.grid(rep(list(c("+", "*", ",")), length(operators) - 1), stringsAsFactors = FALSE)
  operations <- operations[apply(operations, 1, \(y) any(y == ",")),]
  equations <- lapply(seq_len(nrow(operations)), \(y){
    eval(parse(text = paste0(paste(ifelse(rev(operations[y,]) == ",", "as.numeric(paste0(", "("), collapse = ""),
                             paste(rbind(operators, c("", ifelse(operations[y,] == ",", "))", ")")), c(operations[y,], "")), collapse = ""))))
  })
  if(test_results[x] %in% equations) test_results[x] else 0
})
options(scipen = 999)
cat("The total calibration result is", sum(possible_equations) + sum(new_possible_equation), "\n")