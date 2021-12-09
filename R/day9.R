#' day 9
#'
#' @export
day9 <- function() {
  # data
  file <- system.file("extdata/day9.txt", package = "adventofcode2021")
  input_mat <- do.call(rbind,strsplit(readLines(file), ""))
  # pad with 9s
  input_mat <- cbind("9", rbind("9", input_mat, "9"), "9")
  n_rows <- nrow(input_mat)
  input_vec <- as.numeric(input_mat)

  # implement own base lag/lead padding with 9s
  lag  <- function(x, n = 1) c(rep(9, n), head(x, -n))
  lead <- function(x, n = 1) c(tail(x, -n), rep(9, n))

  ## part1
  lows_lgl <-
    input_vec < lag(input_vec) &
    input_vec < lead(input_vec) &
    input_vec < lag(input_vec, n_rows) &
    input_vec < lead(input_vec, n_rows)
  part1 <- sum(input_vec[lows_lgl] + 1)

  ## part2
  # every location next to a basin cell is from this basin, unless it's a 9
  # first set low points to different negative values so we can keep track
  input_vec[lows_lgl] <- -seq(sum(lows_lgl))
  nines <- input_vec == 9
  while(any(input_vec %in% 1:8)) {
    input_vec <- pmin(
      input_vec,
      lag(input_vec),
      lead(input_vec),
      lag(input_vec, n_rows),
      lead(input_vec, n_rows)
    )
    input_vec[nines] <- 9 # repair the walls
  }
  # visual check : matrix(input_vec, n_rows)
  part2 <- prod(tail(sort(tabulate(-input_vec)),3))

  list(part1 = part1, part2 = part2)
}
