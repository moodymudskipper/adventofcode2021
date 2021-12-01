#' day 4
#'
#' @export
day4 <- function() {
  # for notes
  key <- value <- byr <- ecl <- eyr <- iyr <- pid <- hcl <- NULL

  # dependencies
  `%>%` <- magrittr::`%>%`
  map_dfr     <- purrr::map_dfr
  n_distinct  <- dplyr::n_distinct
  with_groups <- dplyr::with_groups
  filter      <- dplyr::filter
  case_when   <- dplyr::case_when
  between     <- dplyr::between
  spread      <- tidyr::spread
  unglue_data <- unglue::unglue_data

  # data
  file <- system.file("extdata/day4.txt", package = "adventofcode2020")
  input <- paste(readLines(file), collapse = "\n")

  # part1
  passports <- strsplit(input, "\n\n")[[1]]
  passports <- strsplit(passports, "\\s")
  passports <- map_dfr(passports, unglue_data, "{key}:{value}", .id = "id")
  valid_passports <-
    passports %>%
    with_groups("id", filter, all(c("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid") %in% key))

  part1 <- dplyr::n_distinct(valid_passports$id)

  # part2
  silent_as_numeric <- function(x) suppressWarnings(as.numeric(x))

  part2 <-
    valid_passports %>%
    filter(key != "cid") %>%
    spread(key, value) %>%
    filter(between(as.numeric(byr), 1920, 2002),
           between(as.numeric(iyr), 2010, 2020),
           between(as.numeric(eyr), 2020, 2030),
           #grepl("^(\\d{3}cm)|(\\d{2}in)$", hgt),
           case_when(
             substr(hgt, 3,4) == "in" ~
               between(silent_as_numeric(substr(hgt, 1,2)), 59, 76),
             substr(hgt, 4,5) == "cm" ~
               between(silent_as_numeric(substr(hgt, 1,3)), 150, 193),
             TRUE ~ FALSE),
           grepl("^#[0-9a-f]{6}$", hcl),
           ecl %in% c("amb", "blu", "brn", "gry", "grn", "hzl", "oth"),
           grepl("^[0-9]{9}$", pid)
    ) %>%
    nrow()


  list(part1 = part1, part2 = part2)
}
