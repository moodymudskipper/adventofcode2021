#' day 13
#'
#' @export
day17 <- function() {
  input <- c("####...#",
             "......#.",
             "#..#.##.",
             ".#...#.#",
             "..###.#.",
             "##.###..",
             ".#...###",
             ".##....#")

  data <- do.call(rbind, strsplit(input, "")) == "#"

  # a wrapper with better arg order and defaults
  replicate2 <- function(x, n) replicate(n, x, simplify = FALSE)

  # convert list to dots to avoid horrible do.call calls
  as_dots <- function(x) {
    f <- function(...) environment()$...
    do.call(f, as.list(x))
  }

  solve <- function(data0, n_dim, n_cycles) {
    # create empty array, we make it cubic to simplify later computations
    side0 <- nrow(data0)
    side  <- 2*n_cycles + side0 + 2
    mid <- 2:(side-1) # indices to build central square/cube
    data <- array(dim = rep(side, n_dim)) & FALSE

    # fill in initial data
    init_ind <- c(
      replicate2(n_cycles+1+seq(side0), 2),   # skip margins
      rep(n_cycles+1+(side0+1)/2, n_dim - 2)) # keep central layer
    `...` <- as_dots(init_ind)
    data[...] <- data0

    # build offset cubes
    `...` <- as_dots(replicate2(1:3, n_dim))
    meta_indices <- expand.grid(...)

    for(i in seq(n_cycles)) {
      `...` <- as_dots(meta_indices)
      offset_cubes <- Map(..., f = function(...){
        `...` <- as_dots(list(mid-1, mid, mid+1)[c(...)])
        data[...]})

      # compute active neighbours and update data
      `...` <- as_dots(replicate2(mid, n_dim))
      data_mid <- data[...]
      act_nb <- Reduce("+", offset_cubes) - data_mid

      `...` <- as_dots(replicate2(mid, n_dim))
      data[...] <- (data_mid & act_nb %in% 2:3) | (!data_mid & act_nb == 3)
    }
    sum(data)
  }

  list(part1 = solve(data, 3, 6), part2 =solve(data, 4, 6))
}



# might be fast enough using data.table's inequi joins
#
#
# day17_b <- function() {
#
#   library(tidyverse)
#
#   input <- c("####...#",
#              "......#.",
#              "#..#.##.",
#              ".#...#.#",
#              "..###.#.",
#              "##.###..",
#              ".#...###",
#              ".##....#")
#
#   input <- c(".#.",
#              "..#",
#              "###")
#
#   data <- do.call(rbind, strsplit(input, "")) == "#"
#   ind <- which(data)
#   active_data <- data.frame(
#     x = (ind-1) %% nrow(data) + 1,
#     y = (ind-1) %/% nrow(data) + 1,
#     z = 0,
#     w= 0,
#     active = TRUE)
#
#   for (i in 1:6) {
#     grid <- do.call(expand.grid, lapply(active_data[-5], function(x) seq(min(x)-1, max(x)+1)))
#     merged_data <- grid %>%
#       setNames(c("x", "y", "z", "w")) %>%
#       left_join(active_data, by = c("x", "y", "z", "w")) %>%
#       mutate(active = !is.na(active)) %>%
#       rowid_to_column("id") %>%
#       gather(coord,value, x, y, z, w) %>%
#       left_join(., filter(., active), by = "coord") %>%
#       filter(id.x != id.y) %>%
#       with_groups(c("id.x", "id.y"), mutate, elligible = all(abs(value.x - value.y) <= 1)) %>%
#       filter(elligible) %>%
#       with_groups("id.x",mutate, elligible = n_distinct(id.y))
#
#     treeprof({grid ->.
#       setNames(., c("x", "y", "z", "w")) ->.
#       left_join(., active_data, by = c("x", "y", "z", "w")) ->.
#       mutate(., active = !is.na(active)) ->.
#       rowid_to_column(., "id") ->.
#       gather(., coord,value, x, y, z, w) ->.
#       left_join(., filter(., active), by = "coord") ->.
#       filter(., id.x != id.y) ->.
#       with_groups(., c("id.x", "id.y"), mutate, elligible = all(abs(value.x - value.y) <= 1)) ->.
#       filter(., elligible) ->.
#       with_groups(., "id.x",mutate, elligible = n_distinct(id.y))
#
#     active_data_1 <- filter(merged_data, active.x, elligible %in% c(2,3))
#     })
#
#     active_data_1 <-
#       if(!nrow(active_data_1)) {
#         data.frame(x=double(), y = double(), z = double(), w = double())
#       } else {
#         distinct(active_data_1, id.x,active.x, coord, value.x ) %>%
#           spread(coord, value.x) %>%
#           select(x, y, z, w)
#       }
#
#     active_data_2 <-
#       merged_data %>%
#       filter(!active.x, elligible == 3) %>%
#       distinct(id.x,active.x, coord, value.x ) %>%
#       spread(coord, value.x) %>%
#       select(x, y, z, w)
#
#     active_data <- rbind(active_data_1, active_data_2) %>%
#       mutate(active = TRUE)
#   }
#
# }
#
