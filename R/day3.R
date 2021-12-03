#' day 3
#'
#' @export
day3 <- function() {
  # data
  file  <- system.file("extdata/day3.txt", package = "adventofcode2021")
  input <- scan(file, what = character(), sep = "", quiet = TRUE)
  mat   <- do.call(rbind, strsplit(input, "")) == 1

  # part 1
  gamma <- colSums(mat) >= nrow(mat)/2
  epsilon <- !gamma
  powers <- 2^(rev(seq_along(gamma)-1))
  part1 <-  sum(powers * gamma) * sum(powers * epsilon)

  # part 2
  oxygen <- co2 <- mat
  for (i in seq(ncol(mat))) {
    if (is.matrix(oxygen))
      oxygen <- oxygen[oxygen[,i] == (sum(oxygen[,i]) >= nrow(oxygen)/2),]
    if (is.matrix(co2))
      co2 <- co2[co2[,i] == (sum(co2[,i]) < nrow(co2)/2),]
  }

  part2 <-  sum(powers * oxygen) * sum(powers * co2)

  list(part1 = part1, part2 = part2)
}
