#' day 19
#'
#' @export
day19 <- function() {
  # for notes
  pat <- letter <- message <- n <- NULL

  `%>%` <- magrittr::`%>%`
  unglue_data <- unglue::unglue_data
  filter      <- dplyr::filter
  transmute   <- dplyr::transmute
  mutate_at   <- dplyr::mutate_at
  pull        <- dplyr::pull
  vars        <- dplyr::vars
  case_when   <- dplyr::case_when
  glue        <- glue::glue

  # data
  file <- system.file("extdata/day19.txt", package = "adventofcode2020")
  input <- readLines(file)

  # extract values using {unglue}

  patterns <- c(
    "{n}: {B1} {B2} | {B3} {B4}",
    "{n}: {C1} | {C2}",
    "{n}: {A1} {A2}",
    "{n}: {D=\\d+}",
    '{n}: {letter}',
    "{message=.+}")
  raw_data <- unglue::unglue_data(input, patterns, convert = TRUE)
  messages <- na.omit(raw_data$message)

  head(raw_data, 2)

  # part 1

  # build reactive pattern list
  p <- list()
  pattern_definitions <-
    raw_data %>%
    filter(!is.na(n)) %>%
    mutate_at(vars(-letter, -message), ~ . + 1) %>%
    transmute(pat = case_when(
      !is.na(A1) ~ glue("p[[{n}]] <- function() paste0(p[[{A1}]](), p[[{A2}]]())"),
      !is.na(B1) ~ glue("p[[{n}]] <- function() paste0('(', p[[{B1}]](), p[[{B2}]](),'|', p[[{B3}]](), p[[{B4}]](), ')')"),
      !is.na(C1) ~ glue("p[[{n}]] <- function() paste0('(', p[[{C1}]](), '|', p[[{C2}]](),')')"),
      !is.na(D)  ~ glue("p[[{n}]] <- function() paste0(p[[{D}]]())"),
      TRUE ~ glue("p[[{n}]] <- function() {letter}")
    )) %>%
    pull(pat)
  eval(parse(text=pattern_definitions))

  # match first pattern
  part1 <- sum(grepl(paste0("^", p[[1]](), "$"), messages))

  # part 2

  # "8: 42 | 42 8" means that pattern 42 happens n times
  p[[9]]  <- function() paste0('((',p[[43]](),')+)')

  # "11: 42 31 | 42 11 31" means that 42 is repeated 1 or n times then 31 the same amount
  p[[12]] <- function() paste0(
    '((',p[[43]](),p[[32]](),')|((',p[[43]](),'){2}(',p[[32]](),'){2})|((',
    p[[43]](),'){3}(',p[[32]](),'){3})|((',p[[43]](),'){4}(',p[[32]](),'){4}))')

  pattern <- paste0("^", p[[1]](), "$")
  part2 <- sum(grepl(pattern, messages))


  list(part1 = part1, part2 = part2)
}
