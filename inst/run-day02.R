library(adventofcode21)

x <- readr::read_table("inst/input02.txt",
                       col_names  = c("direction", "magnitude"),
                       col_types = "cd")

p1 <- f02a(x)
p2 <- f02b(x)

stopifnot(p1 == aoc_solutions$day02a)
stopifnot(p2 == aoc_solutions$day02b)
