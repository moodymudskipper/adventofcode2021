# aoc1(17, year = "2021")
aoc1 <- function(day, path = "inst/extdata", year = 2021) {
  # clear screen
  cat("\014")

  # close tabs

  problem_url <- sprintf("https://adventofcode.com/%s/day/%s", year, day)
  data_url    <- paste0(problem_url, "/input")

  # browse url to data,  open appropriate script for given day to copy and paste
  data_file <- file.path(path, paste0(year, "-12-", formatC(day, width = 2, flag = 0), ".txt"))
  file.create(data_file, showWarnings = FALSE)
  usethis::edit_file(data_file)
  browseURL(data_url)
  # cmd+a cmd+c close paste in new window

  # load instructions in Viewer
  tmp <- tempfile("aoc", fileext = ".html")
  download.file(problem_url, tmp)
  html_article <- rvest::html_elements(xml2::read_html(tmp), "body>main>article")
  xml2::write_html(html_article, file = tmp)
  rstudioapi::viewer(tmp)

  # here we should have a system to match input to output
  # maybe by naming codes with letters and we match them with "ab df"... where pairs of letters link input and output
  # inputs can be automatically parsed, so each input is a list, input$raw, input$matrix, input$data.frame
  # tmp2 <- tempfile("aoc", fileext = ".html")
  # chunks_html <- rvest::html_elements(html_article, "pre")
  # xml2::write_html(chunks_html[[1]], file = tmp2)
  # rstudioapi::viewer(tmp2)

  chunks <- rvest::html_text(rvest::html_elements(html_article, "code"))
  lett <- c(letters, LETTERS)
  chunks <- setNames(chunks, lett[seq_along(chunks)])
  if(is.atomic(chunks) && !any(grepl("\\n", chunks))) print(chunks) else {
    for(i in seq_along(chunks)) {
      message(lett[i])
      cat(chunks[[i]], "\n\n")
      #lines <- strsplit(chunk, "\n")[[1]]
    }
  }
  chunks <- as.list(chunks)
  message("Match input to value (e.g. 'a 100 b 5' or 'a c b d':")
  inputs1 <- readline("matches: ") # d 0 e 0 g 3 h 3 j 3 l -1 m -1 o -3 p -3
  inputs1 <- strsplit(inputs1, " ")[[1]]
  inputs1 <- matrix(inputs1, byrow = TRUE, ncol =2)
  colnames(inputs1) <- c("input", "result1")
  inputs1 <- as.data.frame(inputs1)
  inputs1$input <- chunks[inputs1$input]


  input <- readLines(data_file)
  inputs1 <- rbind(inputs1, data.frame(input = I(list(input)), result1 = NA))
  input_is_single_line <- length(input) == 1
  if (input_is_single_line) {
    input_is_binary <- grepl("^[01]+$", input)
    input_is_almost_binary <- length(unique(strsplit(input, "")[[1]])) == 2
    input_is_number <- grepl("^\\d+$", input)
    input_is_hexa <- grepl("^[0-1A-F]+$", input)
    if(TRUE) {
      inputs1$char_vec <- strsplit(unlist(inputs1$input), "")
      inputs1$char_split <- strsplit(unlist(inputs1$input), "[^a-zA-Z]+")
      inputs1$num_split <- strsplit(unlist(inputs1$input), "[^1-9]+")
    }
  }
  assign("inputs1", inputs1, parent.frame())
  assign("chunks", chunks, parent.frame())
  assign("problem_url", problem_url, parent.frame())
  assign("data_file", data_file, parent.frame())
  assign("day", day, parent.frame())


  usethis::edit_file("R/general_script.R")
}

# after part1 is done, save page in default download folder
aoc2 <- function(day, path = "inst/extdata", year = 2021) {
  file <- sprintf("~/Downloads/%s.html", day)
  tmp2 <- tempfile("aoc", fileext = ".html")
  html_article <- rvest::html_elements(xml2::read_html(file), "body>main>article")[[2]]
  xml2::write_html(html_article, file = tmp2)
  rstudioapi::viewer(tmp2)
  chunks <- rvest::html_text(rvest::html_elements(html_article, "code"))
  lett <- c(letters, LETTERS)
  chunks <- setNames(chunks, lett[seq_along(chunks)])
  if(is.atomic(chunks)) print(chunks) else {
    for(i in seq_along(chunks)) {
      message(lett[i])
      cat(chunks[[i]], "\n\n")
      #lines <- strsplit(chunk, "\n")[[1]]
    }
  }
  chunks <- as.list(chunks)
  message("Match input to value (e.g. 'a 100 b 5' or 'a c b d':")
  inputs2 <- readline("matches: ") # d 0 e 0 g 3 h 3 j 3 l -1 m -1 o -3 p -3
  #browser()
  inputs2 <- strsplit(inputs2, " ")[[1]]
  inputs2 <- matrix(inputs2, byrow = TRUE, ncol =2)
  colnames(inputs2) <- c("input", "result2")
  inputs2 <- as.data.frame(inputs2)
  inputs2$input <- chunks[inputs2$input]

  input <- readLines(data_file)
  inputs2 <- rbind(inputs2, data.frame(input =  I(list(input)), result2 = NA))
  input_is_single_line <- length(input) == 1
  if (input_is_single_line) {
    input_is_binary <- grepl("^[01]+$", input)
    input_is_almost_binary <- length(unique(strsplit(input, "")[[1]])) == 2
    input_is_number <- grepl("^\\d+$", input)
    input_is_hexa <- grepl("^[0-1A-F]+$", input)
    if(TRUE) {
      inputs2$char_vec <- strsplit(unlist(inputs2$input), "")
      inputs2$char_split <- strsplit(unlist(inputs2$input), "[^a-zA-Z]+")
      inputs2$num_split <- strsplit(unlist(inputs2$input), "[^1-9]+")
    }
  }
  assign("inputs2", inputs2, parent.frame())

  usethis::edit_file("R/general_script.R")
}




solve <- function(inputs) {

  for(i in seq(nrow(input))) {
    message("input ", i, ":")

    sol <- solve_impl(inputs$input[[1]])



  }


}
