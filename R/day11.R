#' day 11
#'
#' @export
day11 <- function() {
  # data
  file <- system.file("extdata/day11.txt", package = "adventofcode2021")
  input_mat <- do.call(rbind, strsplit(readLines(file), ""))
  # build walls of NAs so we don't have to bother with edge/corner cases <- lol
  input_mat <- cbind(NA, rbind(NA, input_mat, NA), NA)
  n_rows <- nrow(input_mat)
  input_vec <- as.numeric(input_mat)
  flashes <- i <- 0
  repeat {
    i <- i + 1
    input_vec <- input_vec + 1
    pos <- which(input_vec > 9)
    while(new_flashes <- length(pos)) {
      flashes <- flashes + new_flashes
      input_vec[pos] <- -Inf # protect flashed cells
      input_vec <- Reduce(
        \(vec, j) { vec[pos + j] <- vec[pos + j] + 1 ; vec},
        c((-n_rows-1):(-n_rows+1), -1, +1, (+n_rows-1):(+n_rows+1)),
        input_vec)
      pos <- which(input_vec > 9)
    }
    input_vec[input_vec == -Inf] <- 0
    if (i == 100) {
      part1 <- flashes
    }
    if(sum(input_vec, na.rm = TRUE) == 0) {
      part2 <- i
      break
    }
  }
  list(part1 = part1, part2 = part2)
}
