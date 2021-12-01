#' day 3
#'
#' @export
day3 <- function() {
  # data
  file <- system.file("extdata/day3.txt", package = "adventofcode2020")
  input <- readLines(file)

  # part 1

  # make input a logical matrix of trees
  trees_mat <- as.matrix(do.call(rbind, strsplit(input, ""))) == "#"
  # make sure it's at least 3 times wider then long
  mult <- ceiling(3 * nrow(trees_mat) / ncol(trees_mat))
  trees_mat <- do.call(cbind, replicate(mult, trees_mat, simplify = FALSE))
  # transpose so we reaching bottom means reaching end of matrix
  trees_mat <- t(trees_mat)
  part1 <- sum(trees_mat[seq(1, length(trees_mat), 3 + nrow(trees_mat))])

  # part 2

  # same as part 1
  trees_mat <- as.matrix(do.call(rbind, strsplit(input, ""))) == "#"

  # wrap part 1 in function (we keep trees_map global)
  count_trees <- function(right, down) {
    mult <- ceiling(right * nrow(trees_mat) / ncol(trees_mat))
    trees_mat <- do.call(cbind, replicate(mult, trees_mat, simplify = FALSE))
    # transpose so we reaching bottom means reaching end of matrix
    trees_mat <- t(trees_mat)
    sum(trees_mat[seq(1, length(trees_mat), right + nrow(trees_mat)*down)])
  }

  part2 <- prod(mapply(count_trees, c(1,3,5,7,1), c(1,1,1,1,2)))

  list(part1 = part1, part2 = part2)
}
