
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
  stage_id <- 3
  while(stage_id <= nrow(inputs)) {
    stage <- lapply(as.list(inputs), `[[`, stage_id)
    # choose among pre computed formats
    input <- stage$input
    i <- 0
    hash <- FALSE
    while(!hash) {
      i <- i + 1
      chr <- paste0(input, i)
      hash <- startsWith(digest::digest(chr, algo = "md5", serialize = F), "000000")
    }
    part1 <- i
    aoc_compare()
    stage_id <- stage_id + 1
  }


  part1 <- str

  list
}
