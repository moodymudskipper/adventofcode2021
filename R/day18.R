#' day 18
#'
#' @export
day18 <- function() {
  # data
  library(tidyverse)
  file <- system.file("extdata/day18.txt", package = "adventofcode2021")

  add_pair <- function(x, y) {
    snail_num <- paste0("[", x, ",", y, "]")
    chars <- strsplit(snail_num, "")[[1]] # only 1 digit nums at this stage
    repeat {
      first_5th_bracket <- which(cumsum(chars == "[") - cumsum(chars == "]")== 5)[1]
      all_num_pos <- grep("\\d", chars)
      if(!is.na(first_5th_bracket)) {
        # explode
        before_bracket <- which(grep("\\d", chars) > first_5th_bracket)[1] - 1
        num_pos <- all_num_pos[before_bracket + (0:3)]
        if(before_bracket == 0) num_pos <- c(NA, num_pos)
        if(!is.na(num_pos[1]))
          chars[num_pos[1]] <- as.numeric(chars[num_pos[1]]) + as.numeric(chars[num_pos[2]])
        if(!is.na(num_pos[4]))
          chars[num_pos[4]] <- as.numeric(chars[num_pos[4]]) + as.numeric(chars[num_pos[3]])
        chars <- c(chars[1:(first_5th_bracket-1)], "0", chars[(first_5th_bracket+5):length(chars)])
        next
      }
      all_nums <- as.numeric(chars[all_num_pos])
      first_big_num <- all_num_pos[all_nums >= 10][1]
      if(!is.na(first_big_num)) {
        #split
        num <- all_nums[all_nums >= 10][1]
        chars <- c(chars[1:(first_big_num-1)], "[", floor(num/2), "," , ceiling(num/2), "]", chars[(first_big_num+1):length(chars)])
        next
      }
      break
    }
    paste(chars, collapse = "")
  }

  # part1
  input <- readLines(file)
  reduced <- Reduce(add_pair, input)
  reduced_call <- gsub("\\[", "magn(", reduced)
  reduced_call <- gsub("\\]", ")", reduced_call)
  magn <- function(x, y) 3 * x + 2 * y
  part1 <- eval(str2lang(reduced_call))

  # part 2
  grid <- merge(input, input, by = NULL) |> subset(x != y)
  part2 <- with(grid, {
    reduced <- mapply(add_pair, x, y)
    reduced_call <- gsub("\\[", "magn(", reduced)
    reduced_call <- gsub("\\]", ")", reduced_call)
    max(sapply(parse(text=reduced_call), eval))
  })

  list(part1 = part1, part2 = NULL)
}
