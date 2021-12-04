
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
      boards1 <- boards2 <- data.frame(
        num = unlist(boards_raw),
        board = rep(1:n_boards, each = n),
        col = rep(rep(1:n, each = n * n_boards)),
        row = 1:n)
      counts1 <- counts2 <- data.frame(
        board = rep(1:n_boards, each = 2*n),
        direction = rep(c("row", "col"), each = n),
        ind = 1:5,
        count = 0
      )

      # part 1
      for(draw in draws) {
        ind <- which(boards1$num == draw)
        boards1$num[ind] <- NA
        for(i in ind) {
          counts1 <- transform(
            counts1, count = count + (
              (direction == "row" & ind == boards1$row[i] |
                direction == "col" & ind == boards1$col[i]) &
                board == boards1$board[i]))
        }
        bingo <- counts1$count == n
        if(any(bingo)) break
      }
      part1 <- draw * sum(boards1$num[boards1$board == counts1$board[bingo]], na.rm = TRUE)

      # part 2
      for(draw in draws) {
        ind <- which(boards2$num == draw)
        boards2$num[ind] <- NA
        for(i in ind) {
          counts2 <- transform(
            counts2, count = count + (
              (direction == "row" & ind == boards2$row[i] |
                 direction == "col" & ind == boards2$col[i]) &
                board == boards2$board[i]))
          counts2$bingo <- counts2$count == n
        }
        bingos <- aggregate(bingo ~ board, data = counts2, FUN = any)
        if(all(bingos$bingo)) break
        boards2 <- boards2[boards2$board %in% bingos$board[!bingos$bingo],]
      }
      part2 <- draw * sum(boards2$num, na.rm = TRUE)

      list(part1 = part1, part2 = part2)
    }
