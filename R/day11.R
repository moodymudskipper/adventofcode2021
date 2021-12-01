#' day _i
#'
#' @export
day11 <- function() {
  # for notes
  . <- NULL

  # dependencies
  `%>%` <- magrittr::`%>%`

  # data
  file <- system.file("extdata/day11.txt", package = "adventofcode2020")
  input <- readLines(file)

#   # example
#   input <- "L.LL.LL.LL
# LLLLLLL.LL
# L.L.L..L..
# LLLL.LL.LL
# L.LL.LL.LL
# L.LLLLL.LL
# ..L.L.....
# LLLLLLLLLL
# L.LLLLLL.L
# L.LLLLL.LL"
# input <- strsplit(input, "\n")[[1]]
  mat <- as.matrix(do.call(rbind, strsplit(input, "")))
  bkp_mat <-  mat
  nr <- nrow(mat)
  nc <- ncol(mat)

  # part 1

  repeat {
    bkp_mat1 <- mat
    taken_mat <- mat == "#"
    # build 8 padded matrices
    left_padded     <- cbind(FALSE, taken_mat[,-nc])
    right_padded    <- cbind(taken_mat[,-1], FALSE)
    top_padded      <- rbind(FALSE, taken_mat[-nr,])
    bottom_padded   <- rbind(taken_mat[-1,], FALSE)
    top_left_padded       <- rbind(FALSE, left_padded[-nr,])
    bottom_left_padded    <- rbind(left_padded[-1,], FALSE)
    top_right_padded      <- rbind(FALSE, right_padded[-nr,])
    bottom_right_padded   <- rbind(right_padded[-1,], FALSE)

    # sum `#` of padded matrices gives number of neighbouring `#`
    n_adj_taken <-
      left_padded + right_padded + top_padded + bottom_padded +
      top_left_padded + bottom_left_padded + top_right_padded + bottom_right_padded

    mat[] <- ifelse(mat == "L" & !n_adj_taken, "#",
                    ifelse(mat == "#" & n_adj_taken >= 4, "L", mat))
    if(identical(mat, bkp_mat1))
      break
  }
  part1 <- sum(mat == "#")

  # part 2
  mat <- bkp_mat
  # create an indicator for all diagonals in the matrix
  downward_diag_ind <- row(mat) - col(mat)
  upward_diag_ind   <- row(mat) + col(mat)
  mat_try_list <- list()
  repeat {
    bkp_mat2 <- mat
    mat_try_list[[length(mat_try_list) + 1]] <- mat

    # "#" must be contagious to rightmost "." for left_padded, and so on
    # we first build rles for all directions
    hor_rle <- apply(mat, 1, rle)
    ver_rle <- apply(mat, 2, rle)
    upward_diag_rle   <- lapply(split(mat, upward_diag_ind), rle)
    downward_diag_rle <- lapply(split(mat, downward_diag_ind), rle)

    # build 8 padded matrices
    left_padded     <-
      lapply(hor_rle, function(x) {
        l <- length(x$values)
      x$values[-1][x$values[-l] == "#" & x$values[-1] == "."] <- "#"
      inverse.rle(x)
    }) %>%
      do.call(rbind, .) %>%
      {cbind(".", .[,-nc])} %>%
      `==` ("#")

    right_padded     <-
      lapply(hor_rle, function(x) {
        l <- length(x$values)
        x$values[-l][x$values[-1] == "#" & x$values[-l] == "."] <- "#"
        inverse.rle(x)
      }) %>%
      do.call(rbind, .) %>%
      {cbind(.[,-1], ".")} %>%
      `==` ("#")

    top_padded     <-
      lapply(ver_rle, function(x) {
        l <- length(x$values)
        x$values[-1][x$values[-l] == "#" & x$values[-1] == "."] <- "#"
        inverse.rle(x)
      }) %>%
      do.call(cbind, .) %>%
      {rbind(".", .[-nc,])} %>%
      `==` ("#")

    bottom_padded     <-
      lapply(ver_rle, function(x) {
        l <- length(x$values)
        x$values[-l][x$values[-1] == "#" & x$values[-l] == "."] <- "#"
        inverse.rle(x)
      }) %>%
      do.call(cbind, .) %>%
      {rbind(.[-1,], ".")} %>%
      `==` ("#")

    top_left_padded        <- mat
    split(top_left_padded, downward_diag_ind) <-
      lapply(downward_diag_rle, function(x) {
        l <- length(x$values)
        x$values[-1][x$values[-l] == "#" & x$values[-1] == "."] <- "#"
        inverse.rle(x)
      })
    top_left_padded <-
      top_left_padded %>%
      {rbind(".", .[-nc,])} %>%
      {cbind(".", .[,-nc])} %>%
      `==` ("#")

    bottom_left_padded      <- mat
    split(bottom_left_padded, upward_diag_ind) <-
      lapply(upward_diag_rle, function(x) {
        l <- length(x$values)
        x$values[-1][x$values[-l] == "#" & x$values[-1] == "."] <- "#"
        inverse.rle(x)
      })
    bottom_left_padded <-
      bottom_left_padded %>%
      {rbind(.[-1,], ".")} %>%
      {cbind(".", .[,-nc])} %>%
      `==` ("#")

    top_right_padded        <- mat
    split(top_right_padded, upward_diag_ind) <-
      lapply(upward_diag_rle, function(x) {
        l <- length(x$values)
        x$values[-l][x$values[-1] == "#" & x$values[-l] == "."] <- "#"
        inverse.rle(x)
      })
    top_right_padded <-
      top_right_padded %>%
      {rbind(".", .[-nc,])} %>%
      {cbind(.[,-1], ".")} %>%
      `==` ("#")

    bottom_right_padded        <- mat
    split(bottom_right_padded, downward_diag_ind) <-
      lapply(downward_diag_rle, function(x) {
        l <- length(x$values)
        x$values[-l][x$values[-1] == "#" & x$values[-l] == "."] <- "#"
        inverse.rle(x)
      })
    bottom_right_padded <-
      bottom_right_padded %>%
      {rbind(.[-1,], ".")} %>%
      {cbind(.[,-1], ".")} %>%
      `==` ("#")

    n_adj_taken <- Reduce(`+`, list(
      left_padded, right_padded, top_padded, bottom_padded,
      top_left_padded, bottom_left_padded, top_right_padded, bottom_right_padded))
    #n_adj_taken <- F
    mat[] <- ifelse(mat == "L" & !n_adj_taken, "#",
                    ifelse(mat == "#" & n_adj_taken >= 5, "L", mat))

    if(identical(mat, bkp_mat2))
      break
  }
  part2 <- sum(mat == "#") # should not be 2219

  list(part1 = part1, part2 = part2)
}
