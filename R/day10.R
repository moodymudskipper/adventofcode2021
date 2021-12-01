#' day 10
#'
#' @export
day10 <- function() {

  # data
  file <- system.file("extdata/day10.txt", package = "adventofcode2020")
  input <- scan(file, what = numeric(), sep = "\n", quiet = TRUE)

  # part 1

  diffs <- c(diff(c(0,sort(input))), 3)
  tab   <- tabulate(diffs)
  part1 <- prod(tab[c(1,3)])

  # jumps are 3 at most, and tab shows we have no jump of 2, thus we can
  # find rles of 1s and find all possible combinations of 1,2 and 3 that we
  # can build from those.
  # we find that the max running length of 1s is 4, small enough to do manually :
  # max(rle(diff(sort(input)))$lengths) # 4
  # 4 can be expressed in 7 ways :
  # 4 = 1 + 1 + 1 + 1
  # 4 = 1 + 1 + 2
  # 4 = 1 + 2 + 1
  # 4 = 2 + 1 + 1
  # 4 = 2 + 2
  # 4 = 1 + 3
  # 4 = 3 + 1
  # 3 can be expressed in 4 ways
  # 3 = 1 + 1 + 1
  # 3 = 1 + 2
  # 3 = 2 + 1
  # 3 = 3
  # 2 can be expressed in 2 ways
  # 2 = 1 + 1
  # 2 = 2

  # part 2
  multiplier <- c(1,2,4,7)
  rle_1s     <- with(rle(diffs), lengths[values == 1])
  part2 <- prod(multiplier[rle_1s])
  part2 <- format(part2, scientific = FALSE) # to see all digits

  list(part1 = part1, part2 = part2)
}
