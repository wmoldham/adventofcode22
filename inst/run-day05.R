library(adventofcode22)
path <- "./inst/input05.txt"
x <- read_lines(path, n_max = 9)
y <- read_lines(path, skip = 10)

p1 <- f05(x, y, rev = TRUE)
p2 <- f05(x, y, rev = FALSE)

stopifnot(p1 == aoc_solutions$day05a)
stopifnot(p2 == aoc_solutions$day05b)
