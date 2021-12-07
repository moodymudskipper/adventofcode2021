#' day 7
#'
#' @export
day7 <- function() {
  # data
  file <- system.file("extdata/day7.txt", package = "adventofcode2021")
  input <- scan(file, what = numeric(), sep = ",", quiet = TRUE)
  counts <- transform(as.data.frame(table(input), stringsAsFactors = FALSE),
                      input = as.numeric(input))

  # part1
  grid <- merge(seq(max(input)), counts, by = NULL) |>
    transform(dist = abs((x - input)))
  part1 <- min(with(grid, tapply(dist * Freq, x, sum)))

  # part2
  grid$dist2 <- grid$dist * (grid$dist+1) / 2
  part2 <- min(with(grid, tapply(dist2 * Freq, x, sum)))

  list(part1 = part1, part2 = part2)
}
