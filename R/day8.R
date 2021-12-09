#' day 8
#'
#' @export
day8 <- function() {
  # data
  file <- system.file("extdata/day8ex.txt", package = "adventofcode2021")
  input <- readLines(file)

  pattern <- "{x0} {x1} {x2} {x3} {x4} {x5} {x6} {x7} {x8} {x9} | {y0} {y1} {y2} {y3}"
  input_df <- unglue::unglue_data(input, pattern)

  # part1
  part1 <- sum(nchar(unlist(input_df[11:14])) %in% c(2,3,4,7))

  # part2
  library(tidyverse)
  # establish number profiles
  numbers <- tibble(
    x0 = "abcefg", x1 = "cf", x2 = "acdeg", x3 = "acdfg", x4 = "bcdf",
    x5 = "abdfg", x6 = "abdefg", x7 = "acf", x8 = "abcdefg", x9 = "abcdfg")

  to_long <-
    . %>%
    mutate(line = 1:n()) %>%
    pivot_longer(-line, names_to = "pos", values_to = "segment") %>%
    separate_rows(segment, sep="") %>%
    filter(segment != "") %>%
    mutate(input = startsWith(pos, "x")) %>%
    with_groups(c(line, segment), mutate, n_segment = sum(input)) %>%
    with_groups(c(line, pos), summarise, key = list(sort(n_segment)))

  numbers_long <- to_long(numbers) %>% transmute(key, num = 0:9)
  input_long <-  to_long(input_df)

  part2 <- left_join(input_long, numbers_long, by = "key") %>%
    filter(startsWith(pos, "y")) %>%
    with_groups(line, summarise, num = sum(num * c(1000, 100, 10, 1))) %>%
    pull(num) %>%
    sum()

  list(part1 = part1, part2 = part2)
}
