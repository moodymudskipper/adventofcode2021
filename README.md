
<!-- README.md is generated from README.Rmd. Please edit that file -->

# adventofcode2021

My solutions for adventofcode2021. You can install it with

``` r
remotes::install_github("moodymudskipper/adventofcode2021")
```

The package contains functions `day1`, `day2` etc, the README displays
the body of these functions.

## day 1

    function() {
      # data
      file <- system.file("extdata/day1.txt", package = "adventofcode2021")
      input <- scan(file, what = numeric(), sep = "\n", quiet = TRUE)

      # part 1
      part1 <- sum(diff(input) > 0)

      # part 2
      part2 <- sum(diff(diff(c(0, cumsum(input)), 3)) > 0)
      # should be :
      part2 <- sum(diff(input, 3) > 0)

      list(part1 = part1, part2 = part2)
    }

## day 2

    function() {
      # data
      file <- system.file("extdata/day2.txt", package = "adventofcode2021")
      input <- scan(file, what = character(), sep = "\n", quiet = TRUE)

      # part 1
      # transform code into complex arithmetic and eval
      code <- paste(input, collapse = "")
      code <- gsub("forward ", "+", code)
      code <- gsub("down (\\d+)", "+\\1i", code)
      code <- gsub("up (\\d+)", "-\\1i", code)
      pos <- eval(str2lang(code))
      part1 <- Re(pos) * Im(pos)

      # part1 again
      # same with vectors rather than complex values
      code <- paste(input, collapse = "")
      code <- gsub("forward (\\d+)", "+c(\\1,0)", code)
      code <- gsub("down (\\d+)", "+c(0,\\1)", code)
      code <- gsub("up (\\d+)", "-c(0,\\1)", code)
      pos <- eval(str2lang(code))
      part1 <- prod(pos)

      # part 2
      # convert instructions to functions and reduce
      code <- gsub("forward (\\d+)", "\\\\(x) x + c(\\1, x[3] * \\1, 0)", input)
      code <- gsub("down (\\d+)", "\\\\(x) x + c(0, 0, \\1)", code)
      code <- gsub("up (\\d+)", "\\\\(x) x - c(0, 0, \\1)", code)
      pos <- Reduce(function(x, f) f(x), lapply(code, \(f) eval(str2lang(f))), c(0,0,0))
      part2 <- prod(pos[1:2])

      list(part1 = part1, part2 = part2)
    }

## day 3

    function() {
      # data
      file  <- system.file("extdata/day3.txt", package = "adventofcode2021")
      input <- scan(file, what = character(), sep = "", quiet = TRUE)
      mat   <- do.call(rbind, strsplit(input, "")) == 1

      # part 1
      gamma <- colMeans(mat) >= .5
      epsilon <- !gamma
      powers <- 2^(rev(seq_along(gamma)-1))
      part1 <-  sum(powers * gamma) * sum(powers * epsilon)

      # part 2
      oxygen <- co2 <- mat
      for (i in seq(ncol(mat))) {
        if (is.matrix(oxygen))
          oxygen <- oxygen[oxygen[,i] == (mean(oxygen[,i]) >= .5),]
        if (is.matrix(co2))
          co2 <- co2[co2[,i] == (mean(co2[,i]) < .5),]
      }
      part2 <-  sum(powers * oxygen) * sum(powers * co2)

      list(part1 = part1, part2 = part2)
    }

## day 4

    function() {
      # data
      file <- system.file("extdata/day4.txt", package = "adventofcode2021")
      draws <- scan(file, what = numeric(), sep = ",", quiet = TRUE, nlines = 1)
      boards_raw <- read.delim(file, skip = 2, header = F, sep ="")
      n <- ncol(boards_raw)
      n_boards <- nrow(boards_raw) / n
      boards <- data.frame(
        num = unlist(boards_raw),
        board = rep(1:n_boards, each = n),
        col = rep(rep(1:n, each = n * n_boards)),
        row = 1:n)

      # the row/line indice is stored in `row` because "reshape"
      boards_tidy <- reshape(
        boards, idvar = c("num", "board"), direction = "long",
        varying = list(c("row", "col")), times = c("row", "col"), timevar = "dir")

      boards_tidy <- within(boards_tidy, {
        match <- match(num, draws)
        highest_match_by_line <- ave(match, board, dir, row, FUN = max)
        winning_match_by_board <- ave(highest_match_by_line, board, FUN = min)
      })

      # part 1
      ending_match <- min(boards_tidy$winning_match_by_board)
      winning_board <- subset(
        boards_tidy,
        board == board[winning_match_by_board == ending_match] & match > ending_match)
      part1 <- sum(winning_board$num) * draws[ending_match] / 2

      # part 2, same but min -> max
      ending_match <- max(boards_tidy$winning_match_by_board)
      winning_board <- subset(
        boards_tidy,
        board == board[winning_match_by_board == ending_match] & match > ending_match)
      part2 <- sum(winning_board$num) * draws[ending_match] / 2

      list(part1 = part1, part2 = part2)
    }

## day 5

    function() {

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
