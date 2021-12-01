#' day 12
#'
#' @export
day12 <- function() {
  # for notes
  angle_incr <- NULL

  # dependencies
  `%>%`       <- magrittr::`%>%`
  unglue_data <- unglue::unglue_data
  case_when   <- dplyr::case_when
  mutate      <- dplyr::mutate

  # data
  file <- system.file("extdata/day12.txt", package = "adventofcode2020")
  input <- readLines(file)
  data <- unglue_data(input, "{dir=.}{amount}", convert = TRUE)

  # part1

  data1 <- data %>%
    mutate(
      amount = as.numeric(amount),
      angle_incr = case_when(
        dir == "L" ~ amount,
        dir == "R" ~ -amount,
        TRUE ~ 0
      ),
      angle_total = cumsum(angle_incr),
      y_incr = case_when(
        dir == "N" ~ amount,
        dir == "S" ~ -amount,
        dir == "F" ~ amount * sin(angle_total * pi/180)),
      x_incr = case_when(
        dir == "E" ~ amount,
        dir == "W" ~ -amount,
        dir == "F" ~ amount * cos(angle_total * pi/180))
    )
  part1 <- abs(sum(data1$y_incr, na.rm = TRUE)) + abs(sum(data1$x_incr, na.rm = TRUE))

  # part2
  waypoint <- c(10,1)
  boat <- c(0,0)

  for(i in seq(nrow(data))) {
    dir <- data[i, "dir"]
    amount <- data[i, "amount"]
    if(dir == "L")
      waypoint <- switch(
        as.character(amount),
        "90"  = c(-waypoint[2],  waypoint[1]),
        "180" = c(-waypoint[1], -waypoint[2]),
        "270" = c(waypoint[2],  -waypoint[1]))
    else if(dir == "R")
      waypoint <- switch(
        as.character(amount),
        "90"  = c(waypoint[2],  -waypoint[1]),
        "180" = c(-waypoint[1], -waypoint[2]),
        "270" = c(-waypoint[2],  waypoint[1]))
    else if(dir == "N")
      waypoint[2] <- waypoint[2] + amount
    else if(dir == "S")
      waypoint[2] <- waypoint[2] - amount
    else if(dir == "E")
      waypoint[1] <- waypoint[1] + amount
    else if(dir == "W")
      waypoint[1] <- waypoint[1] - amount
    else if(dir == "F")
      boat <- boat + amount * waypoint
  }
  part2 <- sum(abs(boat))

  list(part1 = part1, part2 = part2)
}
