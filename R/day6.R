#' day 6
#'
#' @export
day6 <- function() {
  # data
  file <- system.file("extdata/day6.txt", package = "adventofcode2021")
  input <- scan(file, what = numeric(), sep = ",", quiet = TRUE)
  input <- merge(data.frame(input = 0:8), as.data.frame(table(input)), all.x = TRUE)
  input$Freq[is.na(input$Freq)] <- 0
  input2 <- input

  # part1
  for(i in 1:80) {
    input$input <- (input$input - 1) %% 9
    input$Freq[input$input == 6] <-
      input$Freq[input$input == 6] + input$Freq[input$input == 8]
  }
  sum(input$Freq)

  # can be simplified into :
  # for(i in (1 + (6 + 1:80) %% 9)) {
  #   input$Freq[i] <- input$Freq[i] + input$Freq[1 + (i + 1) %% 9]
  # }
  # sum(input$Freq)

  # part2 (same thing)
  for(i in 1:256) {
    input2$input <- (input2$input - 1) %% 9
    input2$Freq[input2$input == 6] <-
      input2$Freq[input2$input == 6] + input2$Freq[input2$input == 8]
  }
  format(sum(input2$Freq), scientific =  FALSE)

  list(part1 = part1, part2 = part2)
}
