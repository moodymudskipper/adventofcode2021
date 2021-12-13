#' day 13
#'
#' @export
day13 <- function() {
  # data
  file <- system.file("extdata/day13.txt", package = "adventofcode2021")
  coords1 <- coords2 <- na.omit(unglue::unglue_data(readLines(file), "{x},{y}", convert = TRUE))
  x_folds <- na.omit(unglue::unglue_vec(readLines(file), "fold along x={val}", convert = TRUE))
  y_folds <- na.omit(unglue::unglue_vec(readLines(file), "fold along y={val}", convert = TRUE))

  # part 1
  coords1$x <- abs(x_folds[1] - coords1$x) - 1
  part1 <- nrow(unique(coords1))

  # part 2
  for (x in x_folds) {
    coords2$x <- abs(x - coords2$x) - 1
  }
  for (y in y_folds) {
    coords2$y <- abs(y - coords2$y) - 1
  }
  coords2 <- unique(coords2)
  plot(transform(coords2, x = -x))

  list(part1 = part1, part2 = NULL)
}
