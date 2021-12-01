library(adventofcode21)
data_01 <- read.table("./inst/input01.txt")

p1 <- f01a(data_01)
p2 <- f01b_slider(data_01)

stopifnot(p1 == aoc_solutions$day01a)
stopifnot(p2 == aoc_solutions$day01b)

