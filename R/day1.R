#' day 1
#'
#' @export
day1 <- function() {
  # for notes
  Var1 <- Var2 <- Var3 <- . <- NULL
  # dependencies
  `%>%` <- magrittr::`%>%`

  # data
  file <- system.file("extdata/day1.txt", package = "adventofcode2020")
  input <- scan(file, what = numeric(), sep = "\n", quiet = TRUE)

  # part 1
  part1 <-
    input %>%
    expand.grid(.,.) %>%
    subset(Var1 < Var2 & Var1+Var2 == 2020) %>%
    prod()

  # part 2
  part2 <-
    input %>%
    expand.grid(.,.,.) %>%
    subset(Var1 < Var2 & Var2 < Var3 & Var1+Var2+Var3 == 2020) %>%
    prod()

  list(part1 = part1, part2 = part2)
}
