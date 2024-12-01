# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Topic : Advent of Code 2015
# Day   : 7
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Load data ---------------------------------------------------------------
source("Get_data.R")
input <- aoc_get_response(7, year = 2015, session_cookie = keyring::key_get("RStudio Keyring Secrets", "Advent of Code Session Cookie")) |>
  httr::content(encoding = 'UTF-8') |>
  readr::read_lines()

# Part 1 ------------------------------------------------------------------
ToBin <- \(w){
  bin <- as.numeric(strsplit(R.utils::intToBin(as.numeric(w)), "")[[1]])
  return(c(rep(0, 16 - length(bin)), bin))
}
ToDec <- \(w) sum(2 ^ (which(rev(w) == 1) - 1))
CheckWire <- \(w, wires) (!grepl("[A-Za-z]", w)) | (w %in% names(wires))
ExtractWire <- \(w, wires){
  if(w %in% names(wires)){
    return(wires[[w]])
  }else{
    return(ToBin(w))
  }
}
Circuit <- \(instructions){
  gates <- c("NOT ", "OR ", "AND ", "RSHIFT ", "LSHIFT ")
  gates_pasted <- paste0("(", paste(gates, collapse = ")|("), ")")
  wires <- list()
  while(length(instructions) > 0){
    for(i in seq_along(instructions)){
      connection <- strsplit(instructions[i], " -> ")[[1]]
      instr <- connection[1]
      wire <- strsplit(sub(gates_pasted, "", instr), " ")[[1]]
      wire_result <- connection[2]
      if(all(CheckWire(wire, wires))){
        wire <- lapply(wire, \(w) ExtractWire(w, wires))
        if(!grepl(gates_pasted, instr)){
          wires[[wire_result]] <- wire[[1]]
        }else if(grepl("NOT", instr)){
          wires[[wire_result]] <- as.numeric(!wire[[1]])
        }else if(grepl("OR", instr)){
          wires[[wire_result]] <- as.numeric(wire[[1]] | wire[[2]])
        }else if(grepl("AND", instr)){
          wires[[wire_result]] <- as.numeric(wire[[1]] & wire[[2]])
        }else if(grepl("RSHIFT", instr)){
          wire[[2]] <- ToDec(wire[[2]])
          wires[[wire_result]] <- c(rep(0, wire[[2]]), wire[[1]][seq_len(16 - wire[[2]])])
        }else if(grepl("LSHIFT", instr)){
          wire[[2]] <- ToDec(wire[[2]])
          wires[[wire_result]] <- c(wire[[1]][-seq_len(wire[[2]])], rep(0, wire[[2]]))
        }
        instructions[i] <- NA
      }
    }
    instructions <- instructions[!is.na(instructions)]
  }
  return(wires)
}
wires <- Circuit(input)
cat("Ultimately,", ToDec(wires[["a"]]), "is provided to wire a\n")

# Part 2 ------------------------------------------------------------------
instructions <- input
instructions[grep("(-> b)$", instructions)] <- paste(ToDec(wires[["a"]]), "-> b")
wires <- Circuit(instructions)
cat("Ultimately,", ToDec(wires[["a"]]), "is provided to wire a\n")