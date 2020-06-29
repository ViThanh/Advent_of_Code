# ============================================================================ #
# Topic : Advent of Code
# --- Day 2: 1202 Program Alarm ---
# ============================================================================ #
setwd("~/Advent of Code/2019/2")
input <- as.vector(read.csv("input.txt", header = F), mode = "numeric")
Intcode <- function(input, initialize) {
  input[2] <- initialize[1]
  input[3] <- initialize[2]
  opcode <- input[1]
  i = 1
  while (opcode != 99) {
    pos1 <- input[i + 1] + 1
    pos2 <- input[i + 2] + 1
    pos3 <- input[i + 3] + 1
    if (opcode == 1) {
      input[pos3] <- input[pos1] + input[pos2]
    } else if (opcode == 2) {
      input[pos3] <- input[pos1] * input[pos2]
    } else if (opcode != 1 & opcode !=2) {stop("Something went wrong")}
    i <- i + 4
    opcode <- input[i]
  }
  return(input[1])
}
Intcode(input, c(12, 2))

# ============================================================================ #
# --- Part Two ---
# ============================================================================ #
for(i in 0:99) {
  for (j in 0:99)
    if (Intcode(input, c(i, j)) == 19690720) print(100 * i + j)
}