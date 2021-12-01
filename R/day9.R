#' day 9
#'
#' @export
day9 <- function() {
  # for notes
  . <- Var1 <- Var2 <- NULL

  # dependencies
  `%>%` <- magrittr::`%>%`

  # data
  file <- system.file("extdata/day9.txt", package = "adventofcode2020")
  input <- scan(file, what = numeric(), sep = "\n", quiet = TRUE)

  # part 1
  i <- 26
  while(input[i] %in% (
    input[(i-25):(i-1)] %>% expand.grid(.,.) %>% subset(Var1 != Var2) %>% rowSums())
  ) i <- i +1
  part1 <- input[i]

  # part 2
  j <- 1
  while(!part1 %in% (tmp <- cumsum(input[j:(i-1)]))) j <- j+1
  range <- input[j:(which(part1 == tmp) + j - 1)]
  part2 <- max(range) + min(range)

  list(part1 = part1, part2 = part2)
}
