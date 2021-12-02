
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
