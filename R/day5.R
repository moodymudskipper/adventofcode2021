#' day 5
#'
#' @export
#' @importFrom utils tail
day5 <- function() {
  # data
  file <- system.file("extdata/day5.txt", package = "adventofcode2020")
  input <- readLines(file)

  # part 1
  all_ids <- strtoi(chartr("FBLR", "0101", input), base = 2)
  part1 <- max(all_ids)

  # part 2
  part2 <- tail(setdiff(seq(max(all_ids)), all_ids), 1)
  list(part1 = part1, part2 = part2)
}



