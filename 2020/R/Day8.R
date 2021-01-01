# ============================================================================ #
# Topic : Advent of Code 2020
# Day   : 8
# Theme : Handheld Halting
# ============================================================================ #
library(data.table)
input <- read.csv('~/Advent of Code/2020/Assignments/Day8Input.txt', header = F)[[1]]

# ============================================================================ #
# Part One
# ============================================================================ #
# Each instruction consists of an operation (acc, jmp, or nop) and an argument
# - acc increases or decreases the accumulator by the argument
# - jmp jumps to a new instruction relative to itself
# - nop stands for No OPeration
# Return the value of the accumulator immediately before any instruction is 
# executed a second time
instructions <- sapply(input, function(x) strsplit(x, ' '))
instructions <- data.table(operation = sapply(instructions, function(x) x[1]), 
                           argument = sapply(instructions, function(x) as.numeric(x[2])),
                           order = NA)
Accumulator <- function(instructions){
  accumulator <- 0
  pos <- 1
  counter <- 0
  while(is.na(instructions$order[pos]) & pos <= nrow(instructions)){
    counter <- counter + 1
    instructions$order[pos] <- counter
    operation <- instructions$operation[pos]
    argument <- instructions$argument[pos]
    if(operation == 'acc' | operation == 'nop'){
      pos <- pos + 1
      if(operation == 'acc'){
        accumulator <- accumulator + argument
      }
    }else if(operation == 'jmp'){
      pos <- pos + argument
    }
  }
  return(list(accumulator = accumulator, position = pos))
}
Accumulator(instructions)$accumulator

# ============================================================================ #
# --- Part Two ---
# ============================================================================ #
# Fix the program so that it terminates normally by changing exactly one jmp 
# (to nop) or nop (to jmp)
# Return the value of the accumulator after the program terminates?
SwitchOperations <- function(instructions){
  for(i in 1:nrow(instructions)){
    instr <- instructions
    if(instr$operation[i] %in% c('jmp', 'nop')){
      instr[, order := NA]
      instr$operation[i] <- ifelse(instr$operation[i] == 'jmp', 'nop', 'jmp')
      program <- Accumulator(instr)
      if(program$position > nrow(instr)){
        return(program$accumulator)
      }
    }
  }
}
SwitchOperations(instructions)
