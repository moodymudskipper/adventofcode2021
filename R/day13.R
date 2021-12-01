#' day 13
#'
#' @export
day13 <- function() {
  # dependencies
  `%>%` <- magrittr::`%>%`

  # data
  file <- system.file("extdata/day13.txt", package = "adventofcode2020")
  input <- readLines(file)

  # part 1

  start <- as.numeric(input[1])
  ids   <- as.numeric(setdiff(strsplit(input[2],",")[[1]], "x"))
  next_dep <- ids - start %% ids
  i <- which.min(next_dep)
  part1 <- ids[i] * next_dep[i]

  # part 2

  part2 <- NULL

  list(part1 = part1, part2 = part2)
}
