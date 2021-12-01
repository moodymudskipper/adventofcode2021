#' day 16
#'
#' @export
day16 <- function() {
  # for notes
  field <- ticket <- value <- from1 <- to1 <- from2 <- to2 <- valid <-
    valid_ticket <- ticket_id <- field_id <- valid_value <-
    n_valid <- NULL

  # dependencies
  `%>%` <- magrittr::`%>%`
  unglue_data <- unglue::unglue_data
  filter      <- dplyr::filter
  select      <- dplyr::select
  with_groups <- dplyr::with_groups
  mutate      <- dplyr::mutate
  n           <- dplyr::n
  distinct    <- dplyr::distinct
  summarize   <- dplyr::summarize
  pull        <- dplyr::pull
  left_join   <- dplyr::left_join
  rowid_to_column <- tibble::rowid_to_column
  separate_rows   <- tidyr::separate_rows
  crossing        <- tidyr::crossing

  # data
  file <- system.file("extdata/day16.txt", package = "adventofcode2020")
  input <- readLines(file)

  raw_data <- unglue_data(input, c(
    "{field}: {from1}-{to1} or {from2}-{to2}",
    "{ticket=\\d.*}"
    ), convert = TRUE)

  field_data  <- raw_data %>%
    filter(!is.na(field)) %>%
    select(-ticket)

  ticket_data <- raw_data %>%
    select(value = ticket) %>%
    filter(!is.na(value)) %>%
    rowid_to_column("ticket_id") %>%
    separate_rows(value, convert = TRUE) %>%
    with_groups("ticket_id", mutate, field_id = seq(n()))

  combined_data <-
    ticket_data  %>%
    tidyr::crossing(field_data) %>%
    mutate(valid = (value >= from1 & value <= to1) | (value >= from2 & value <= to2))

  # part 1

  part1 <-
    combined_data %>%
    with_groups(c("ticket_id", "value"), summarize, valid = any(valid)) %>%
    filter(ticket_id != 1 & !valid) %>%
    dplyr::pull(value) %>%
    sum()

  # part 2

  valid_counts <-
    combined_data %>%
    # a value is valid if it satisfies at least one rule
    with_groups(c("ticket_id", "value"), mutate, valid_value = any(valid)) %>%
    # a ticket is valid if all its values are valid
    with_groups("ticket_id", mutate, valid_ticket = all(valid_value)) %>%
    filter(valid_ticket) %>%
    # mark all field_id field combos are invalid if one is invalid
    with_groups(c("field_id", "field"), summarize, valid = all(valid)) %>%
    # count valid combos for each field_id
    with_groups("field_id", mutate, n_valid = sum(valid))

  # initiate df to store mappings
  mapping <- data.frame(field_id=integer(), field = character())

  # when we have 1 valid combo only, add it to mapping and update valid_counts
  while(1 %in% valid_counts$n_valid) {
    mapping <- rbind(mapping, subset(valid_counts, valid & n_valid == 1, 1:2))
    valid_counts <- valid_counts %>%
      filter(!field_id %in% mapping$field_id,
             !field %in% mapping$field) %>%
      with_groups("field_id", mutate, n_valid = sum(valid))
  }

  # apply mapping to our ticket and compute result
  part2 <-
    ticket_data %>%
    filter(ticket_id == 1) %>%
    left_join(mapping, by = "field_id") %>%
    filter(startsWith(field, "departure")) %>%
    pull(value) %>%
    prod()

  list(part1 = part1, part2 = part2)
}
