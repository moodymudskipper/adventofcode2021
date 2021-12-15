#' day 15
#'
#' @export
day15 <- function() {
  # data
  library(tidyverse)
  file <- system.file("extdata/day15ex.txt", package = "adventofcode2021")
  input_mat <- do.call(rbind, lapply(strsplit(readLines(file), ""), as.numeric))

  solve <- function(input_mat) {
    # build walls of Inf simulating cells that cannot have their score improved
    input_mat <- cbind(Inf, rbind(Inf, input_mat, Inf), Inf)
    n_rows <- nrow(input_mat)
    # start point
    points <- c(n_rows + 2)

    input_vec <- scores <- c(input_mat)
    #  at first we only know the lowest score to 1st cell is 0
    scores[] <- Inf
    scores[points] <- 0

    # keep going if scores improve
    while (length(points)) {
      old_scores <- scores
      # check if we can improve a score in any of the 4 directions
      for (delta in c(+1, -1, +n_rows, -n_rows)) {
        new_scores <- scores
        new_scores[points + delta] <- scores[points] + input_vec[points + delta]
        scores <- pmin(scores, new_scores) # keep best scores only
      }
      points <- which(scores != old_scores)
    }
    scores[length(scores)-n_rows-1]
  }

  # part1
  part1 <- solve(input_mat)

  # part2
  input_mat2 <- do.call(rbind, replicate(5, input_mat, simplify = F))
  input_mat2 <- do.call(cbind, replicate(5, input_mat2, simplify = F))
  additions <- (col(input_mat2) -1) %/% ncol(input_mat) + (row(input_mat2) -1) %/% nrow(input_mat)
  input_mat2 <- (input_mat2 + additions - 1) %% 9 + 1
  part2 <- solve(input_mat2)

  list(part1 = part1, part2 = NULL)
}
