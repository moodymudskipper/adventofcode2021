#' day 18
#'
#' @export
day18 <- function() {

  file <- system.file("extdata/day18.txt", package = "adventofcode2020")
  input <- readLines(file)

  # part 1
  `-` <- `*`
  input <- gsub("\\*", "\\-", input)
  part1 <- sum(eval(as.call(c(c, parse(text = input)))))

  # part 2
  `*` <- `+`
  input <- gsub("\\+", "\\*", input)
  part2 <- sum(eval(as.call(c(c, parse(text = input)))))
  format(part2, scientific = FALSE)

  list(part1, part2)
}
