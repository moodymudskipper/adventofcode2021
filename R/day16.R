# length of litteral can be found with trick below
# length of operator 0 is constant
# length of operator 1 is recursive

# should have a while loop that starts new packets if there is remainder

#' day 16
#'
#' @export
day16 <- function() {
  # data
  library(tidyverse)
  file <- system.file("extdata/day16.txt", package = "adventofcode2021")
  mapper <- setNames(
    sapply(0:15, \(x) substr(paste(rev(as.integer(intToBits(x))), collapse=""), 29, 32)),
    c(0:9, LETTERS[1:6]))
  bin <- paste(mapper[strsplit(readLines(file), "")[[1]]], collapse = "")

  solve <- function(bin, packet_pos = 1, max_pos = 1) {
    len <- nchar(bin)
    version <- strtoi(substr(bin, 1, 3), base = 2)
    type    <- strtoi(substr(bin, 4, 6), base = 2)
    if(type == 4) {
      split_remainder <- strsplit(bin, "")[[1]][-(1:6)]
      n_litteral <-
        suppressWarnings(which.min(split_remainder == 1 | c(F, T, T, T, T)) + 4)
      value <- split_remainder[1:n_litteral][c(F, T, T, T, T)]
      value <- sum((value==1)*2^(rev(seq_along(value))-1))
      remainder <- substr(bin, 6 + n_litteral + 1, len)
      len <- len - nchar(remainder)
      subpackets <- NULL
      sum_of_versions <- version
    } else {
      length_type <- substr(bin, 7, 7)
      if(length_type == 0) {
        subpackets_length <- strtoi(substr(bin, 7+1, 7+15), base = 2)
        subpackets_bin <- substr(bin, 7+15+1, 7+15+subpackets_length)
        # we have a limit on chars but not on number of subpackets
        subpackets <- solve(subpackets_bin, packet_pos = 1, max_pos = Inf)
        remainder <- substr(bin, 8+15+subpackets_length, len)
        len <- len - nchar(remainder)
      } else {
        subpackets_n <- strtoi(substr(bin, 7+1, 7+11), base = 2)
        # we have a limit on number of subpackets, not on chars
        subpackets_bin <- substr(bin, 7+11+1, len)
        subpackets <- solve(subpackets_bin, packet_pos = 1, max_pos = subpackets_n)
        covered_len <- 7 + 11 + sum(sapply(subpackets, \(x) x$len))
        remainder <- substr(bin, covered_len + 1, len)
        len <- covered_len
      }
      value <- switch(
        as.character(type),
        "0" = sum(sapply(subpackets, \(x) x$value)),
        "1" = prod(sapply(subpackets, \(x) x$value)),
        "2" = min(sapply(subpackets, \(x) x$value)),
        "3" = max(sapply(subpackets, \(x) x$value)),
        "5" = subpackets[[1]]$value > subpackets[[2]]$value,
        "6" = subpackets[[1]]$value < subpackets[[2]]$value,
        "7" = subpackets[[1]]$value == subpackets[[2]]$value
      )
      sum_of_versions <- version + sum(sapply(subpackets, \(x) x$sum_of_versions))
    }
    # 1st subpacket
    res <- list(list(
      value = value,
      len = len,
      subpackets = subpackets,
      sum_of_versions = sum_of_versions))

    # other subpackets
    if(packet_pos != max_pos && remainder != "" && !grepl("^0+$", remainder)) {
      res <- c(res, solve(remainder, packet_pos = packet_pos + 1, max_pos = max_pos))
    }
    res
  }

  packet1 <- solve(bin)[[1]]
  part1 <- packet1$sum_of_versions
  part2 <- packet1$value

  list(part1 = part1, part2 = NULL) # 246225449979
}
