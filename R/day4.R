#' day 4
#'
#' @export
day4 <- function() {
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
  boards_tidy <- reshape(boards, idvar =c("num", "board"),direction = "long", varying = list(c("row", "col")), times = c("row", "col"), timevar = "dir")
  boards_tidy$match <- match(boards_tidy$num, draws)
  boards_tidy$highest_match_by_line  <- ave(boards_tidy$match, boards_tidy$board, boards_tidy$dir, boards_tidy$row, FUN = max)
  boards_tidy$winning_match_by_board <- ave(boards_tidy$highest_match_by_line, boards_tidy$board, FUN = min)

  # part 1
  ending_match <- min(boards_tidy$winning_match_by_board)
  winning_board <- subset(boards_tidy, board == board[winning_match_by_board == ending_match] & match > ending_match)
  part1 <- sum(winning_board$num) * draws[ending_match] / 2

  # part 2, same but min -> max
  ending_match <- max(boards_tidy$winning_match_by_board)
  winning_board <- subset(boards_tidy, board == board[winning_match_by_board == ending_match] & match > ending_match)
  part2 <- sum(winning_board$num) * draws[ending_match] / 2

  list(part1 = part1, part2 = part2)
}

# day4 <- function() {
#   # data
#   file <- system.file("extdata/day4ex.txt", package = "adventofcode2021")
#   draws <- scan(file, what = numeric(), sep = ",", quiet = TRUE, nlines = 1)
#   boards_raw <- read.delim(file, skip = 2, header = F, sep ="")
#   n <- ncol(boards_raw)
#   n_boards <- nrow(boards_raw) / n
#   boards1 <- boards2 <- boards3 <- data.frame(
#     num = unlist(boards_raw),
#     board = rep(1:n_boards, each = n),
#     col = rep(rep(1:n, each = n * n_boards)),
#     row = 1:n)
#   counts1 <- counts2 <- data.frame(
#     board = rep(1:n_boards, each = 2*n),
#     direction = rep(c("row", "col"), each = n),
#     ind = 1:5,
#     count = 0
#   )
#
#   # part 1
#   for(draw in draws) {
#     ind <- which(boards1$num == draw)
#     boards1$num[ind] <- NA
#     for(i in ind) {
#       counts1 <- transform(
#         counts1, count = count + (
#           (direction == "row" & ind == boards1$row[i] |
#              direction == "col" & ind == boards1$col[i]) &
#             board == boards1$board[i]))
#     }
#     bingo <- counts1$count == n
#     if(any(bingo)) break
#   }
#   part1 <- draw * sum(boards1$num[boards1$board == counts1$board[bingo]], na.rm = TRUE)
#
#   # part 2
#   for(draw in draws) {
#     ind <- which(boards2$num == draw)
#     boards2$num[ind] <- NA
#     for(i in ind) {
#       counts2 <- transform(
#         counts2, count = count + (
#           (direction == "row" & ind == boards2$row[i] |
#              direction == "col" & ind == boards2$col[i]) &
#             board == boards2$board[i]))
#       counts2$bingo <- counts2$count == n
#     }
#     bingos <- aggregate(bingo ~ board, data = counts2, FUN = any)
#     if(all(bingos$bingo)) break
#     boards2 <- boards2[boards2$board %in% bingos$board[!bingos$bingo],]
#   }
#   part2 <- draw * sum(boards2$num, na.rm = TRUE)
#
#   list(part1 = part1, part2 = part2)
# }
