#' day 10
#'
#' @export
day10 <- function() {
  # data
  file <- system.file("extdata/day10ex.txt", package = "adventofcode2021")
  input <- strsplit(readLines(file), "")

  ## part1
  # fun fact: these numbers are cumprod(c(3,19,21,21)), is there a reason ?
  mapper <- list("(" = -3, ")" = 3, "[" = -57, "]" = 57, "{" = -1197, "}" = 1197, "<" = -25137, ">" = 25137)
  check_line <- function(l) {
    l <- mapper[l]
    n <- length(l)
    i <- 2
    while(i <= n) {
      if(l[[i]] > 0) {
        if(l[[i]] != -l[[i-1]]) return(l[[i]]) # corrupted chunk!
        l[(i-1):i] <- NULL
        i <- i-1
        n <- n-2
      } else {
        i <- i + 1
      }
    }
    0
  }
  scores <- sapply(input, check_line)
  part1 <- sum(scores)

  ## part2
  input2 <- input[scores == 0]
  mapper2 <- list("(" = 1, ")" = -1, "[" = 2, "]" = -2, "{" = 3, "}" = -3, "<" = 4, ">" = -4)
  complete_line <- function(l) {
    l <- mapper2[l]
    n <- length(l)
    i <- 2
    while(i <= n) {
      if(l[[i]] < 0) {
        l[(i-1):i] <- NULL
        i <- i-1
        n <- n-2
      } else {
        i <- i + 1
      }
    }
    # reversed number in base 5
    sum(unlist(l) * 5^(seq_along(l)-1))
  }
  scores2 <- sapply(input2, complete_line)
  part2 <- median(scores2)

  list(part1 = part1, part2 = part2)
}
