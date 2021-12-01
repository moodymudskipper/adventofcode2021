
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
