#' day 5
#'
#' @export
day5 <- function() {

  # data
  file <- system.file("extdata/day5.txt", package = "adventofcode2021")
  input <- unglue::unglue_data(readLines(file), "{x1},{y1}->{x2},{y2}", convert = TRUE) + 1
  grid <- with(input, matrix(0, nrow = max(x1, x2), ncol = max(y1, y2)))

  # part1
  input1 <- subset(input, x1 == x2 | y1 == y2)
  update_grid <- function(grid, coords) with(coords, {
    grid[x1:x2, y1:y2] <- grid[x1:x2, y1:y2] + 1
    grid})
  grid1 <- Reduce(update_grid, x = split(input1, 1:nrow(input1)), init = grid)
  part1 <- sum(grid1 >= 2)

  # part 2
  update_grid <- function(grid, coords) with(coords, {
    if(x1 == x2 | y1 == y2)
      grid[x1:x2, y1:y2] <- grid[x1:x2, y1:y2] + 1
    else
      diag(grid[x1:x2, y1:y2]) <- diag(grid[x1:x2, y1:y2]) + 1
    grid})
  grid2 <- Reduce(update_grid, x = split(input, 1:nrow(input)), init = grid)
  part2 <- sum(grid2 >= 2)

  list(part1 = part1, part2 = part2)
}
