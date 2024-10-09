# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Author: Vi Thanh Pham
# Date: 2023-12-01
# Script Name: Advent of Code - Day 03
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Load input --------------------------------------------------------------
path <- strsplit(rstudioapi::getSourceEditorContext()$path, "/")[[1]]
day <- gsub("(Day)|(.R)", "", path[length(path)])
path <- paste0(sub("Code/", "", paste0(paste(head(path, -1), collapse = "/"), "/")),
               "Data/input", day, ".txt")
input <- unlist(read.table(path, comment.char = ""))

# Part 1 ------------------------------------------------------------------
engine <- do.call(rbind, strsplit(input, ""))
symbols <- T
for(i in c(".", 0:9)){
  symbols <- symbols - (engine == i)
}
numbers <- T
numbers <- numbers - (engine == ".")
numbers <- numbers - symbols
symbols <- which(symbols == 1, arr.ind = T)
symbols <- data.table::data.table(symbols)
numbers <- which(numbers == 1, arr.ind = T)
numbers <- numbers[order(numbers[, 1]),]
numbers <- data.table::data.table(numbers)
numbers[, ID := cumsum(c(0, diff(col)) != 1), by = row]
numbers[, min := min(col), by = .(row, ID)]
numbers[, max := max(col), by = .(row, ID)]
numbers <- unique(numbers, by = c("row", "min", "max"))[, .(row, min, max)]
numbers[, number := numeric(0)]
numbers[, symbol := NA]
for(i in seq_len(nrow(numbers))){
  numbers$number[i] <- with(numbers, as.numeric(paste(engine[row[i], unique(min[i]:max[i])], collapse = "")))
  numbers$symbol[i] <- nrow(symbols[(row %in% (-1:1 + numbers$row[i])) & (col %in% ((numbers$min[i] - 1):(numbers$max[i] + 1))),]) > 0
}
sum(numbers[symbol == T, number])

# Part 2 ------------------------------------------------------------------
symbols[, gear_ratio := NA]
for(i in seq_len(nrow(symbols))){
  possible_numbers <- numbers[(row %in% (-1:1 + symbols$row[i])),]
  adjacent_numbers <- possible_numbers[(possible_numbers$min - 1 <= symbols$col[i]) & (possible_numbers$max + 1 >= symbols$col[i]),]$number
  symbols$gear_ratio[i] <- ifelse(length(adjacent_numbers) == 2, prod(adjacent_numbers), 0)
}
sum(symbols$gear_ratio)