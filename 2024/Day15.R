# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Topic : Advent of Code 2024
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Load data ---------------------------------------------------------------
source("Get_data.R")
options(scipen = 999)
input <- aoc_get_response(15, year = 2024, session_cookie = keyring::key_get("RStudio Keyring Secrets", "Advent of Code Session Cookie")) |>
  httr::content(encoding = 'UTF-8') |>
  readr::read_lines()
input <- c("##########",
           "#..O..O.O#",
           "#......O.#",
           "#.OO..O.O#",
           "#..O@..O.#",
           "#O#..O...#",
           "#O..O..O.#",
           "#.OO.O.OO#",
           "#....O...#",
           "##########",
  "",
           "<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^",
             "vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v",
           "><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<",
             "<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^",
             "^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><",
             "^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^",
             ">^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^",
             "<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>",
             "^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>",
             "v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^")
# Part 1 ------------------------------------------------------------------
Move <- \(x){
  empty <- which(x == ".")
  if(length(empty) > 0){
    empty <- empty[1]
    c(x[empty], x[-empty])
  }else{
    x
  }
}
separator <- which(input == "")
box <- do.call(rbind, strsplit(input[seq_len(separator - 1)], ""))
directions <- strsplit(paste(input[seq_len(length(input) - separator) + separator], collapse = ""), "")[[1]]
for(i in directions){
  robot <- which(box == "@", arr.ind = TRUE)
  if(i == "^"){
    idx <- as.matrix(data.frame(row = robot[1]:1, col = robot[2]))
  }else if(i == ">"){
    idx <- as.matrix(data.frame(row = robot[1], col = robot[2]:ncol(box)))
  }else if(i == "v"){
    idx <- as.matrix(data.frame(row = robot[1]:nrow(box), col = robot[2]))
  }else{
    idx <- as.matrix(data.frame(row = robot[1], col = robot[2]:1))
  }
  idx <- idx[seq_len(which(box[idx] == "#")[1] - 1),]
  box[idx] <- Move(box[idx])
}
box <- box[-c(1, nrow(box)),-c(1, ncol(box))]
GPS <- which(box == "O", arr.ind = TRUE)
coordinates <- matrix(c(100, 1), ncol = 1)
cat("The sum of all boxes' GPS coordinates is", sum(GPS %*% coordinates), "\n")

# Part 2 ------------------------------------------------------------------
Move <- \(x, move){
  empty <- which(x == ".")
  if(move %in% c("<", ">")){
    if(length(empty) > 0){
      empty <- empty[1]
      c(x[empty], x[-empty])
    }else{
      x
    }
  }
}
input <- gsub("@", "@.", gsub(".", "..", gsub("O", "[]", gsub("#", "##", input), fixed = TRUE), fixed = TRUE), fixed = TRUE)
separator <- which(input == "")
box <- do.call(rbind, strsplit(input[seq_len(separator - 1)], ""))
directions <- strsplit(paste(input[seq_len(length(input) - separator) + separator], collapse = ""), "")[[1]]
for(i in directions){
  robot <- which(box == "@", arr.ind = TRUE)
  if(i == "^"){
    idx <- as.matrix(data.frame(row = robot[1]:1, col = robot[2]))
  }else if(i == ">"){
    idx <- as.matrix(data.frame(row = robot[1], col = robot[2]:ncol(box)))
  }else if(i == "v"){
    idx <- as.matrix(data.frame(row = robot[1]:nrow(box), col = robot[2]))
  }else{
    idx <- as.matrix(data.frame(row = robot[1], col = robot[2]:1))
  }
  idx <- idx[seq_len(which(box[idx] == "#")[1] - 1),]
  box[idx] <- Move(box[idx], i)
}
box <- box[-c(1, nrow(box)),-c(1, ncol(box))]
GPS <- which(box == "O", arr.ind = TRUE)
coordinates <- matrix(c(100, 1), ncol = 1)
cat("The sum of all boxes' GPS coordinates is", sum(GPS %*% coordinates), "\n")