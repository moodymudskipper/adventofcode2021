#' day 14
#'
#' @export
#' @importFrom utils tail head
day14 <- function() {
  # for notes
  address <- mask <- value <- . <- value_bin <- mask_split <- result_bin <-
    result <- address_bin <- address_split <- address_bin2 <-
    address_expanded <- value_split <- NULL


  `%>%` <- magrittr::`%>%`
  unglue_data <- unglue::unglue_data
  filter      <- dplyr::filter
  mutate      <- dplyr::mutate
  with_groups <- dplyr::with_groups
  last        <- dplyr::last
  pull        <- dplyr::pull
  slice_tail  <- dplyr::slice_tail
  map2_chr    <- purrr::map2_chr
  map         <- purrr::map
  map_chr     <- purrr::map_chr
  fill        <- tidyr::fill
  unnest      <- tidyr::unnest

  # data
  file <- system.file("extdata/day14.txt", package = "adventofcode2020")
  input <- readLines(file)

  data <-
    input %>%
    unglue_data(c("mask = {mask}", "mem[{address}] = {value}")) %>%
    fill(mask) %>%
    filter(!is.na(address))

  data

  # part 1

  data1 <-
    data %>%
    mutate(
      mask_split = strsplit(mask,""),
      value_bin = map_chr(value, . %>%
                        intToBits() %>%
                        rev() %>%
                        as.integer %>%
                        paste(collapse = '') %>%
                        paste0("0000", .)),
      value_split = strsplit(value_bin,"") ,
      result_bin = map2_chr(mask_split, value_split, ~
                           ifelse(.x != "X", .x, .y) %>% paste(collapse="")),
      result = strtoi(substr(result_bin,1,5), 2) * 2^31 +  strtoi(substr(result_bin,6,36), 2)
    )

  part1 <- data1 %>%
    with_groups("address_expanded", slice_tail, 1) %>%
    pull(result) %>%
    sum() %>%
    format(scientific = FALSE)

  # part 2
  data2 <-
    data %>%
    mutate(
      mask_split = strsplit(mask,""),
      address_bin = map_chr(address, . %>%
                            intToBits() %>%
                            rev() %>%
                            as.integer %>%
                            paste(collapse = '') %>%
                            paste0("0000", .)),
      address_split = strsplit(address_bin,""),
      address_bin2 = map2_chr(mask_split, address_split, ~
                     ifelse(.x == "X", "X", ifelse(.x == 0, .y, 1)) %>% paste(collapse="")),
      address_expanded = map(address_bin2, ~ {
        while(any(grepl("X",.))) {
          . <- c(sub("X", "1", .), sub("X", "0", .))
        }
        strtoi(substr(.,1,5), 2) * 2^31 +  strtoi(substr(.,6,36), 2)
      })) %>%
    unnest(address_expanded)

  part2 <- data2 %>%
    with_groups("address_expanded", slice_tail, 1) %>%
    pull(value) %>%
    as.numeric() %>%
    sum() %>%
    format(scientific = FALSE)

  list(part1 = part1, part2 = part2)
}
