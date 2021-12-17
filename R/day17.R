# length of litteral can be found with trick below
# length of operator 0 is constant
# length of operator 1 is recursive

# should have a while loop that starts new packets if there is remainder

#' day 17
#'
#' @export
day17 <- function() {
  # data
  library(tidyverse)
  file <- system.file("extdata/day17.txt", package = "adventofcode2021")
  input <- unglue::unglue_data(
    readLines(file),
    "target area: x={x_min}..{x_max}, y={y_min}..{y_max}",
    convert = TRUE)

  # part1
  # we want to in one step as deep as allowed
  # it also takes vy steps to go to the top, since we take back one unit per step
  # and 1+..+n is n*(n-1)/2
  n <- vy_max <- -input$y_min - 1
  part1 <- n * vy_max - n*(n-1)/2

  # part2 tidy

  library(tidyverse)
  max_steps <- 2*vy_max+2

  # all combinations that would work vertically
  y2 <-
    crossing(n = 1:max_steps, vy = input$y_min:vy_max) %>%
    mutate(y = n * vy - n^2/2 + n/2) %>%
    filter(y >= input$y_min & y <= input$y_max)

  # all combinations that would work horizontally
  x2 <-
    crossing(n = 1:max_steps, vx = 1:input$x_max) %>%
    with_groups("vx", mutate, x = cummax(n * vx - n^2/2 + n/2)) %>%
    filter(x >= input$x_min & x <= input$x_max)

  # combine and count
  part2 <-
    inner_join(x2, y2, by = "n") %>%
    distinct(vx, vy) %>%
    nrow()

  #---------------------------------------------------------------------------
  # part2 base
  max_steps <- 2*vy_max+2
  n_seq <- 1: max_steps

  # all combinations that would york vertically
  y <- do.call(rbind, lapply(input$y_min:vy_max, function(vy) {
    data.frame(vy, n = n_seq, y = n_seq * vy - n_seq^2/2 + n_seq/2) |>
      subset(y >= input$y_min & y <= input$y_max)
  }))

  # all combinations that would work horizontally
  x <- do.call(rbind, lapply(1:input$x_max, function(vx) {
    data.frame(vx, n = n_seq, x = cummax(n_seq * vx - n_seq^2/2 + n_seq/2)) |>
      subset(x >= input$x_min & x <= input$x_max)
  }))

  # combine and count
  part2 <-
    merge(x, y, by = "n")[c("vx", "vy")] |>
    unique() |>
    nrow()

  list(part1 = part1, part2 = NULL) # 246225449979
}
