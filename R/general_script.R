
aoc_solve <- function() {

  part1 <- part2 <- NULL
  part2 <- NA
  if(is.null(part2)) {
    inputs <- inputs1
    final_stage <- max(which(!is.na(inputs$result1))) + 1
  } else {

    inputs <- bind_rows(inputs1, inputs2)
    final_stage <- max(which(!is.na(inputs$result1))) + 1
    final_stage2 <- max(which(!is.na(inputs$result2))) + 1
  }
  aoc_compare <- function() {
    solution1 <- stage$result1
    if(is.null(part2) && (!is.na(solution1) || stage_id == final_stage)) {
      if(stage_id == final_stage) {
        clipr::write_clip(format(part1, scientific = FALSE))
        browseURL(problem_url)
        rstudioapi::sendToConsole(sprintf('aoc2(%s, year = "%s")', day, year), execute = FALSE)
        # cleaner with a return_from maybe
        # call in console should set a global var to signal part1 solved
        stop("Save page to download folder and execute call in console")
      }
      success1 <- format(part1, scientific = FALSE) == solution1
      if(success1) {
        cli::cli_bullets(c(v= sprintf("part1 stage %s", stage_id)))
      } else {
        cli::cli_bullets(c(x= sprintf("part1 stage %s", stage_id)))
        stop("failed!", call. = FALSE)
      }
    }

    solution2 <- stage$result2
    if(!is.null(part2) && (!is.na(solution2)) || stage_id == final_stage2) {
      if(stage_id == final_stage2) {
        clipr::write_clip(format(part2, scientific = FALSE))
        browseURL(problem_url)
        # cleaner with a return_from maybe
        stop("Done ?")
      }

      success2 <- format(part2, scientific = FALSE) == solution2
      if(success2) {
        cli::cli_bullets(c(v= sprintf("part2 stage %s", stage_id)))
      } else {
        cli::cli_bullets(c(x= sprintf("part2 stage %s", stage_id)))
        stop("failed!")
      }
    }
    invisible()
  }

  # for manual debugging
  stage_id <- 2
  while(stage_id <= nrow(inputs)) {
    stage <- lapply(as.list(inputs), `[[`, stage_id)
    # choose among pre computed formats
    input <- unglue::unglue_data(
      stage$input, "target area: x={x_min}..{x_max}, y={y_min}..{y_max}", convert = TRUE)

    #---------------------------------------------------------------------------
    # part1

    # we want the speed that will go the deepest allowed at the first point below 0
    # it also takes vy_max steps to go to the top, since we take back one unit per step
    n <- vy_max <- -input$y_min - 1
    part1 <- n * vy_max - n*(n-1)/2


    #---------------------------------------------------------------------------
    # part2 tidy

    library(tidyverse)
    max_steps <- 2*vy_max+2

    # all combinations that would york vertically
    y2<-
      crossing(n = 1:max_steps, vy = input$y_min:vy_max) %>%
      mutate(y = n * vy - n^2/2 + n/2) %>%
      filter(y >= input$y_min & y <= input$y_max)

    # all combinations that would work horizontally
    x2 <-
      crossing(n = 1:max_steps, vx = 1:input$x_max) %>%
      with_groups("vx", mutate, x = cummax(n * vx - n^2/2 + n/2)) %>%
      filter(x >= input$x_min & x <= input$x_max)

    # combine and count
    part2 <-
      inner_join(x2, y2, by = "n") %>%
      distinct(vx, vy) %>%
      nrow()

    #---------------------------------------------------------------------------
    # part2 base
    max_steps <- 2*vy_max+2
    n_seq <- 1: max_steps

    # all combinations that would york vertically
    y <- do.call(rbind, lapply(input$y_min:vy_max, function(vy) {
      data.frame(vy, n = n_seq, y = n_seq * vy - n_seq^2/2 + n_seq/2) |>
        subset(y >= input$y_min & y <= input$y_max)
    }))

    # all combinations that would work horizontally
    x <- do.call(rbind, lapply(1:input$x_max, function(vx) {
      data.frame(vx, n = n_seq, x = cummax(n_seq * vx - n_seq^2/2 + n_seq/2)) |>
        subset(x >= input$x_min & x <= input$x_max)
    }))

    # combine and count
    part2 <-
      merge(x, y, by = "n")[c("vx", "vy")] |>
      unique() |>
      nrow()

  }
}
