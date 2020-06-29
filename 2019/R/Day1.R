# ============================================================================ #
# Topic : Advent of Code
# --- Day 1: The Tyranny of the Rocket Equation ---
# ============================================================================ #
setwd("~/Advent of Code/2019/1")
input <- read.csv("input.txt", header = F)[[1]]
calculateFuelNeeded <- function(input) {max(floor(input / 3) - 2, 0)}
sum(calculateFuelNeeded(input))

# ============================================================================ #
# --- Part Two ---
# ============================================================================ #
fuel <- as.vector(numeric(length(input)))
for (i in 1:length(input)) {
  f <- input[i]
  while (f > 0) {
    f <- calculateFuelNeeded(f)
    fuel[i] <- fuel[i] + f
  }
}
sum(fuel)

