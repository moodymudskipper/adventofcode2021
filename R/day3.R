#' day 3
#'
#' @export
day3 <- function() {
  # data
  file  <- system.file("extdata/day3.txt", package = "adventofcode2021")
  input <- scan(file, what = character(), sep = "", quiet = TRUE)
  mat   <- do.call(rbind, strsplit(input, "")) == 1

  # part 1
  gamma <- colMeans(mat) >= .5
  epsilon <- !gamma
  powers <- 2^(rev(seq_along(gamma)-1))
  part1 <-  sum(powers * gamma) * sum(powers * epsilon)

  # part 2
  oxygen <- co2 <- mat
  for (i in seq(ncol(mat))) {
    if (is.matrix(oxygen))
      oxygen <- oxygen[oxygen[,i] == (mean(oxygen[,i]) >= .5),]
    if (is.matrix(co2))
      co2 <- co2[co2[,i] == (mean(co2[,i]) < .5),]
  }
  part2 <-  sum(powers * oxygen) * sum(powers * co2)

  list(part1 = part1, part2 = part2)
}
