#' day 7
#'
#' @export
day7 <- function() {
  # for notes
  key <- value <- color <- parent_color <- i <- NULL

  # dependencies
  `%>%` <- magrittr::`%>%`
  unglue_data <- unglue::unglue_data
  filter      <- dplyr::filter
  mutate_at   <- dplyr::mutate_at
  select      <- dplyr::select
  gather      <- tidyr::gather
  separate    <- tidyr::separate
  spread      <- tidyr::spread

  # data
  file <- system.file("extdata/day7.txt", package = "adventofcode2020")
  input <- readLines(file)

  # part 1
  patterns <- c(
    "{parent_color} bags contain no other bags.",
    "{parent_color} bags contain {n_1} {color_1} bag{}, {n_2} {color_2} bag{}, {n_3} {color_3} bag{}, {n_4} {color_4} bag{}.",
    "{parent_color} bags contain {n_1} {color_1} bag{}, {n_2} {color_2} bag{}, {n_3} {color_3} bag{}.",
    "{parent_color} bags contain {n_1} {color_1} bag{}, {n_2} {color_2} bag{}.",
    "{parent_color} bags contain {n_1} {color_1} bag{}."
  )

  clean_data <-
    input %>%
    unglue_data(patterns) %>%
    gather(key,value,-1) %>%
    separate(key, c("col", "i")) %>%
    spread(col, value) %>%
    select(-i) %>%
    filter(!is.na(color)) %>%
    mutate_at("n", as.numeric)

  find_parents <- function(col) {
    direct_parents <- filter(clean_data, color %in% col)$parent_color
    if(length(direct_parents))
      unique(c(direct_parents, find_parents(direct_parents)))
    else
      character(0)
  }

  part1 <- length(find_parents("shiny gold"))

  # part 2
  count_children <- function(col) {
    children <- filter(clean_data, parent_color == col)
    if(nrow(children))
      sum(children$n) + sum(children$n * sapply(children$color, count_children))
    else
      0
  }

  part2 <- count_children("shiny gold")

  list(part1 = part1, part2 = part2)
}
