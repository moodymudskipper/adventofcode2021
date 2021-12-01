#' day 15
#'
#' @export
day15 <- function() {
  # for notes
  last_pos_i <- NULL

  input <- "1,0,15,2,10,13"

  # part 1

  data    <- rev(as.numeric(strsplit(input, ",")[[1]]))
  start <- length(data)+1
  for (i in start:2020) {
    new  <- c(which(data[1] == data[-1]), 0)[1]
    data <- c(new, data)
  }
  part1 <- new

  # part 2

  # build the data
  data    <- as.integer(strsplit(input, ",")[[1]])

  # build a vector long enough to store the last positions of every value
  # the index is the number, the value will be the position (0 when initiating)
  # note : character indexing was too slow
  last_pos  <- integer(3e7)

  # we need to store positions of zeroes too, so we offset the data by 1
  data <- data + 1

  # and we fill in the data that we have, except the last number, because
  # we'll need to test its previous last position
  last_pos[head(data,-1)] <- seq_along(head(data,-1))
  last_num <- tail(data,1)

  start <- length(data)
  end <- 3e7 - 1
  for (i in start:end) {
    last_pos_of_last_num <- last_pos[last_num]
    # update the position of the last num
    last_pos[last_num] <- i
    last_num <- if(last_pos_i) i - last_pos_i + 1 else 1
    if(last_pos_of_last_num) {
      # compute the new num as the offset (don't forget to add 1)
      last_num <-  i - last_pos_of_last_num + 1
    } else {
      # update the position of the last num and update the last num to 1 (0+1)
      last_pos[last_num] <- i
      last_num <- 1
    }
  }
  part2 <- last_num - 1

  list(part1 = part1, part2 = part2)
}


# data  <- c(1,0,15,2,10,13) + 1
# last_pos  <- integer(3e7)
# last_pos[head(data,-1)] <- seq_along(head(data,-1))
# last_num <- tail(data,1)
# for (i in length(data):(3e7 - 1)) {
#   last_pos_i <- last_pos[last_num]
#   last_pos[last_num] <- i
#   last_num <- if(last_pos_i) i - last_pos_i + 1 else 1
# }
# last_num - 1
