#' day 1
#'
#' @export
day1 <- function() {
  # data
  file <- system.file("extdata/day1.txt", package = "adventofcode2021")
  input <- scan(file, what = numeric(), sep = "\n", quiet = TRUE)

  # part 1
  part1 <- sum(diff(input) > 0)

  # part 2
  part2 <- sum(diff(diff(c(0, cumsum(input)), 3)) > 0)

  list(part1 = part1, part2 = part2)
}
