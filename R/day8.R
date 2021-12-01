#' day 8
#'
#' @export
day8 <- function() {
  # dependencies
  `%>%`       <- magrittr::`%>%`
  unglue_data <- unglue::unglue_data
  mutate      <- dplyr::mutate

  # data
  file <- system.file("extdata/day8.txt", package = "adventofcode2020")
  input <- readLines(file)

  # part 1
  clean_data <- clean_data1 <-
    unglue_data(input, "{call} {value}", convert = TRUE) %>%
    mutate(passes = 0)

  i <- 1
  passes <- 0
  glob <- 0
  repeat {
    passes <- clean_data1[i, "passes"] + 1
    clean_data1[i, "passes"] <- passes
    if(passes == 2) break
    call  <- clean_data1[i, "call"]
    value <- clean_data1[i, "value"]
    if(call == "acc") {
      glob <- glob + value
      i <- i + 1
    } else if(call == "jmp") {
      i <- i + value
    } else {
      i <- i + 1
    }
  }
  part1 <- glob

  # part 2

  # return NULL if inf loop, glob otherwise
  test_code <- function(clean_data) {
    i <- 1
    passes <- 0
    glob <- 0
    repeat {
      passes <- clean_data[i, "passes"] + 1
      clean_data[i, "passes"] <- passes
      if(passes == 2) return(NULL)
      call  <- clean_data[i, "call"]
      value <- clean_data[i, "value"]
      if(call == "acc") {
        glob <- glob + value
        i <- i + 1
      } else if(call == "jmp") {
        i <- i + value
      } else {
        i <- i + 1
      }
      if(i == nrow(clean_data)) return(glob)
    }
  }

  for (j in seq(nrow(clean_data))) {
    call  <- clean_data[j, "call"]
    clean_data_modif <- clean_data
    if(call == "nop") {
      clean_data_modif[j, "call"] <- "jmp"
      res <- test_code(clean_data_modif)
      if(!is.null(res)) break
    } else   if(call == "jmp") {
      clean_data_modif[j, "call"] <- "nop"
      res <- test_code(clean_data_modif)
      if(!is.null(res)) break
    }
  }
  part2 <- res

  list(part1 = part1, part2 = part2)
}
