#' day 14
#'
#' @export
day14 <- function() {
  # data
  library(tidyverse)
  file <- system.file("extdata/day14.txt", package = "adventofcode2021")
  input <- readLines(file)
  pairs <- tibble(
    from = substring(input[1], 1:(nchar(input[1]) - 1), 2:nchar(input[1])),
    n = 1)
  first_letter <- substr(input[1], 1, 1)
  mapper <-
    unglue::unglue_data(input[-(1:2)], "{from} -> {to}") %>%
    mutate(to = pmap(list(from, to), ~ c(
      paste0(substr(.x, 1, 1), .y),
      paste0(.y, substr(.x, 2, 2))))) %>%
    unnest(to)

  solve <- function(steps) {
    for (i in 1:steps) {
      pairs <-
        left_join(pairs, mapper, by = "from") %>%
        count(from = to, wt = n)
    }
    counts <-
      pairs %>%
      separate(from, c("from1", "from2"), sep = 1) %>%
      count(letter = from2, wt = n) %>%
      mutate(n = ifelse(letter == first_letter, n+1, n))
    format(max(counts$n) - min(counts$n), scientific = FALSE)
  }

  part1 <- solve(10)
  part2 <- solve(40)

  list(part1 = part1, part2 = NULL)
}
