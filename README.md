
<!-- README.md is generated from README.Rmd. Please edit that file -->

# adventofcode2021

My solutions for adventofcode2021. You can install it with

``` r
remotes::install_github("moodymudskipper/adventofcode2021")
```

The package contains functions `day1`, `day2` etc, the README displays
the body of these functions.

## day 1

    function() {
      # data
      file <- system.file("extdata/day1.txt", package = "adventofcode2021")
      input <- scan(file, what = numeric(), sep = "\n", quiet = TRUE)

      # part 1
      part1 <- sum(diff(input) > 0)

      # part 2
      part2 <- sum(diff(diff(c(0, cumsum(input)), 3)) > 0)
      # should be :
      part2 <- sum(diff(input, 3) > 0)

      list(part1 = part1, part2 = part2)
    }

## day 2

    function() {
      # data
      file <- system.file("extdata/day2.txt", package = "adventofcode2021")
      input <- scan(file, what = character(), sep = "\n", quiet = TRUE)

      # part 1
      # transform code into complex arithmetic and eval
      code <- paste(input, collapse = "")
      code <- gsub("forward ", "+", code)
      code <- gsub("down (\\d+)", "+\\1i", code)
      code <- gsub("up (\\d+)", "-\\1i", code)
      pos <- eval(str2lang(code))
      part1 <- Re(pos) * Im(pos)

      # part1 again
      # same with vectors rather than complex values
      code <- paste(input, collapse = "")
      code <- gsub("forward (\\d+)", "+c(\\1,0)", code)
      code <- gsub("down (\\d+)", "+c(0,\\1)", code)
      code <- gsub("up (\\d+)", "-c(0,\\1)", code)
      pos <- eval(str2lang(code))
      part1 <- prod(pos)

      # part 2
      # convert instructions to functions and reduce
      code <- gsub("forward (\\d+)", "\\\\(x) x + c(\\1, x[3] * \\1, 0)", input)
      code <- gsub("down (\\d+)", "\\\\(x) x + c(0, 0, \\1)", code)
      code <- gsub("up (\\d+)", "\\\\(x) x - c(0, 0, \\1)", code)
      pos <- Reduce(function(x, f) f(x), lapply(code, \(f) eval(str2lang(f))), c(0,0,0))
      part2 <- prod(pos[1:2])

      list(part1 = part1, part2 = part2)
    }

## day 3

    function() {
      # data
      file  <- system.file("extdata/day3.txt", package = "adventofcode2021")
      input <- scan(file, what = character(), sep = "", quiet = TRUE)
      mat   <- do.call(rbind, strsplit(input, "")) == 1

      # part 1
      gamma <- colMeans(mat) >= .5
      epsilon <- !gamma
      powers <- 2^(rev(seq_along(gamma)-1))
      part1 <-  sum(powers * gamma) * sum(powers * epsilon)

      # part 2
      oxygen <- co2 <- mat
      for (i in seq(ncol(mat))) {
        if (is.matrix(oxygen))
          oxygen <- oxygen[oxygen[,i] == (mean(oxygen[,i]) >= .5),]
        if (is.matrix(co2))
          co2 <- co2[co2[,i] == (mean(co2[,i]) < .5),]
      }
      part2 <-  sum(powers * oxygen) * sum(powers * co2)

      list(part1 = part1, part2 = part2)
    }

## day 4

    function() {
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
      boards_tidy <- reshape(
        boards, idvar = c("num", "board"), direction = "long",
        varying = list(c("row", "col")), times = c("row", "col"), timevar = "dir")

      boards_tidy <- within(boards_tidy, {
        match <- match(num, draws)
        highest_match_by_line <- ave(match, board, dir, row, FUN = max)
        winning_match_by_board <- ave(highest_match_by_line, board, FUN = min)
      })

      # part 1
      ending_match <- min(boards_tidy$winning_match_by_board)
      winning_board <- subset(
        boards_tidy,
        board == board[winning_match_by_board == ending_match] & match > ending_match)
      part1 <- sum(winning_board$num) * draws[ending_match] / 2

      # part 2, same but min -> max
      ending_match <- max(boards_tidy$winning_match_by_board)
      winning_board <- subset(
        boards_tidy,
        board == board[winning_match_by_board == ending_match] & match > ending_match)
      part2 <- sum(winning_board$num) * draws[ending_match] / 2

      list(part1 = part1, part2 = part2)
    }

## day 5

    function() {

      # data
      file <- system.file("extdata/day5.txt", package = "adventofcode2021")
      input <- unglue::unglue_data(readLines(file), "{x1},{y1}->{x2},{y2}", convert = TRUE) + 1
      grid <- with(input, matrix(0, nrow = max(x1, x2), ncol = max(y1, y2)))

      # part1
      input1 <- subset(input, x1 == x2 | y1 == y2)
      update_grid <- function(grid, coords) with(coords, {
        grid[x1:x2, y1:y2] <- grid[x1:x2, y1:y2] + 1
        grid})
      grid1 <- Reduce(update_grid, x = split(input1, 1:nrow(input1)), init = grid)
      part1 <- sum(grid1 >= 2)

      # part 2
      update_grid <- function(grid, coords) with(coords, {
        if(x1 == x2 | y1 == y2)
          grid[x1:x2, y1:y2] <- grid[x1:x2, y1:y2] + 1
        else
          diag(grid[x1:x2, y1:y2]) <- diag(grid[x1:x2, y1:y2]) + 1
        grid})
      grid2 <- Reduce(update_grid, x = split(input, 1:nrow(input)), init = grid)
      part2 <- sum(grid2 >= 2)

      # alternate solution
      input <- unglue::unglue_data(readLines(file), "{x1},{y1}->{x2},{y2}", convert = TRUE) + 1

      solve <- \(x) x |>
        split(1:nrow(x)) |>
        lapply(\(row) data.frame(x = row$x1:row$x2, y = row$y1:row$y2, n = 1)) |>
        do.call(what = rbind) |>
        stats:::aggregate.formula(formula = n ~ x + y, FUN = sum) |>
        subset(n > 1) |>
        nrow()

      # part1
      input1 <- subset(input, x1 == x2 | y1 == y2)
      part1 <- solve(input1)

      # part2 (same but using unfiltered input)
      part2 <- solve(input)

      list(part1 = part1, part2 = part2)
    }

## day 6

    function() {
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

## day 7

    function() {
      # data
      file <- system.file("extdata/day7.txt", package = "adventofcode2021")
      input <- scan(file, what = numeric(), sep = ",", quiet = TRUE)
      counts <- transform(as.data.frame(table(input), stringsAsFactors = FALSE),
                          input = as.numeric(input))
      grid <- merge(seq(max(input)), counts, by = NULL) |>
        transform(dist = abs((x - input)))

      # part1
      part1 <- min(with(grid, tapply(dist * Freq, x, sum)))

      # part2
      part2 <- min(with(grid, tapply(dist * (dist+1) / 2 * Freq , x, sum)))

      list(part1 = part1, part2 = part2)
    }

## day 8

    function() {
      # data
      file <- system.file("extdata/day8ex.txt", package = "adventofcode2021")
      input <- readLines(file)

      pattern <- "{x0} {x1} {x2} {x3} {x4} {x5} {x6} {x7} {x8} {x9} | {y0} {y1} {y2} {y3}"
      input_df <- unglue::unglue_data(input, pattern)

      # part1
      part1 <- sum(nchar(unlist(input_df[11:14])) %in% c(2,3,4,7))

      # part2
      library(tidyverse)
      # establish number profiles
      numbers <- tibble(
        x0 = "abcefg", x1 = "cf", x2 = "acdeg", x3 = "acdfg", x4 = "bcdf",
        x5 = "abdfg", x6 = "abdefg", x7 = "acf", x8 = "abcdefg", x9 = "abcdfg")

      to_long <-
        . %>%
        mutate(line = 1:n()) %>%
        pivot_longer(-line, names_to = "pos", values_to = "segment") %>%
        separate_rows(segment, sep="") %>%
        filter(segment != "") %>%
        mutate(input = startsWith(pos, "x")) %>%
        with_groups(c(line, segment), mutate, n_segment = sum(input)) %>%
        with_groups(c(line, pos), summarise, key = list(sort(n_segment)))

      numbers_long <- to_long(numbers) %>% transmute(key, num = 0:9)
      input_long <-  to_long(input_df)

      part2 <- left_join(input_long, numbers_long, by = "key") %>%
        filter(startsWith(pos, "y")) %>%
        with_groups(line, summarise, num = sum(num * c(1000, 100, 10, 1))) %>%
        pull(num) %>%
        sum()

      list(part1 = part1, part2 = part2)
    }

## day 9

    function() {
      # data
      file <- system.file("extdata/day9.txt", package = "adventofcode2021")
      input_mat <- do.call(rbind,strsplit(readLines(file), ""))
      # pad with 9s
      input_mat <- cbind("9", rbind("9", input_mat, "9"), "9")
      n_rows <- nrow(input_mat)
      input_vec <- as.numeric(input_mat)

      # implement own base lag/lead padding with 9s
      lag  <- function(x, n = 1) c(rep(9, n), head(x, -n))
      lead <- function(x, n = 1) c(tail(x, -n), rep(9, n))

      ## part1
      lows_lgl <-
        input_vec < lag(input_vec) &
        input_vec < lead(input_vec) &
        input_vec < lag(input_vec, n_rows) &
        input_vec < lead(input_vec, n_rows)
      part1 <- sum(input_vec[lows_lgl] + 1)

      ## part2
      # every location next to a basin cell is from this basin, unless it's a 9
      # first set low points to different negative values so we can keep track
      input_vec[lows_lgl] <- -seq(sum(lows_lgl))
      nines <- input_vec == 9
      while(any(input_vec %in% 1:8)) {
        input_vec <- pmin(
          input_vec,
          lag(input_vec),
          lead(input_vec),
          lag(input_vec, n_rows),
          lead(input_vec, n_rows)
        )
        input_vec[nines] <- 9 # repair the walls
      }
      # visual check : matrix(input_vec, n_rows)
      part2 <- prod(tail(sort(tabulate(-input_vec)),3))

      list(part1 = part1, part2 = part2)
    }

## day10

    function() {
      # data
      file <- system.file("extdata/day10ex.txt", package = "adventofcode2021")
      input <- strsplit(readLines(file), "")

      ## part1
      # fun fact: these numbers are cumprod(c(3,19,21,21)), is there a reason ?
      mapper <- list("(" = -3, ")" = 3, "[" = -57, "]" = 57, "{" = -1197, "}" = 1197, "<" = -25137, ">" = 25137)
      check_line <- function(l) {
        l <- mapper[l]
        n <- length(l)
        i <- 2
        while(i <= n) {
          if(l[[i]] > 0) {
            if(l[[i]] != -l[[i-1]]) return(l[[i]]) # corrupted chunk!
            l[(i-1):i] <- NULL
            i <- i-1
            n <- n-2
          } else {
            i <- i + 1
          }
        }
        0
      }
      scores <- sapply(input, check_line)
      part1 <- sum(scores)

      ## part2
      input2 <- input[scores == 0]
      mapper2 <- list("(" = 1, ")" = -1, "[" = 2, "]" = -2, "{" = 3, "}" = -3, "<" = 4, ">" = -4)
      complete_line <- function(l) {
        l <- mapper2[l]
        n <- length(l)
        i <- 2
        while(i <= n) {
          if(l[[i]] < 0) {
            l[(i-1):i] <- NULL
            i <- i-1
            n <- n-2
          } else {
            i <- i + 1
          }
        }
        # reversed number in base 5
        sum(unlist(l) * 5^(seq_along(l)-1))
      }
      scores2 <- sapply(input2, complete_line)
      part2 <- median(scores2)

      list(part1 = part1, part2 = part2)
    }

## day11

    function() {
      # data
      file <- system.file("extdata/day11.txt", package = "adventofcode2021")
      input_mat <- do.call(rbind, strsplit(readLines(file), ""))
      # build walls of NAs so we don't have to bother with edge/corner cases <- lol
      input_mat <- cbind(NA, rbind(NA, input_mat, NA), NA)
      n_rows <- nrow(input_mat)
      input_vec <- as.numeric(input_mat)
      flashes <- i <- 0
      repeat {
        i <- i + 1
        input_vec <- input_vec + 1
        pos <- which(input_vec > 9)
        while(new_flashes <- length(pos)) {
          flashes <- flashes + new_flashes
          input_vec[pos] <- -Inf # protect flashed cells
          input_vec <- Reduce(
            \(vec, j) { vec[pos + j] <- vec[pos + j] + 1 ; vec},
            c((-n_rows-1):(-n_rows+1), -1, +1, (+n_rows-1):(+n_rows+1)),
            input_vec)
          pos <- which(input_vec > 9)
        }
        input_vec[input_vec == -Inf] <- 0
        if (i == 100) {
          part1 <- flashes
        }
        if(sum(input_vec, na.rm = TRUE) == 0) {
          part2 <- i
          break
        }
      }
      list(part1 = part1, part2 = part2)
    }

## day12

    function() {
      # data
      file <- system.file("extdata/day12.txt", package = "adventofcode2021")
      input <- unglue::unglue_data(readLines(file), "{from}-{to}")
      # paths go both ways
      input <-
        rbind(input, setNames(input, c("to", "from"))) |>
        subset(to != "start")
      edges <- with(input, split(to, from))
      nodes <- with(input, unique(c(from, to)))
      visited0 <- setNames(rep(FALSE, length(nodes)), nodes)
      visited0[nodes == toupper(nodes)] <- NA

      rec <- function(node = "start", visited = visited0, extra) {
        if(isTRUE(visited[node])) {
          if (!extra) return(0)
          visited[node] <- FALSE
          extra <- FALSE
        }
        if(node == "end") return(1)
        visited[node] <- !visited[node]
        sum(sapply(edges[[node]], rec, visited, extra))
      }
      part1 <- rec(extra = FALSE)
      part2 <- rec(extra = TRUE)

      list(part1 = part1, part2 = part2)
    }

## day13

    function() {
      # data
      file <- system.file("extdata/day13.txt", package = "adventofcode2021")
      coords1 <- coords2 <- na.omit(unglue::unglue_data(readLines(file), "{x},{y}", convert = TRUE))
      x_folds <- na.omit(unglue::unglue_vec(readLines(file), "fold along x={val}", convert = TRUE))
      y_folds <- na.omit(unglue::unglue_vec(readLines(file), "fold along y={val}", convert = TRUE))

      # part 1
      coords1$x <- abs(x_folds[1] - coords1$x) - 1
      part1 <- nrow(unique(coords1))

      # part 2
      for (x in x_folds) {
        coords2$x <- abs(x - coords2$x) - 1
      }
      for (y in y_folds) {
        coords2$y <- abs(y - coords2$y) - 1
      }
      coords2 <- unique(coords2)
      plot(transform(coords2, x = -x))

      list(part1 = part1, part2 = NULL)
    }

## day14

    function() {
      # data
      library(tidyverse)
      file <- system.file("extdata/day14.txt", package = "adventofcode2021")
      input <- readLines(file)
      pairs <- tibble(
        from = substring(input[1], 1:(nchar(input[1]) - 1), 2:nchar(input[1])),
        n = 1)
      first_letter <- substr(input[1], 1, 1)
      mapper <-
        unglue::unglue_data(input[-(1:2)], "{from} -> {to}") %>%
        mutate(to = pmap(list(from, to), ~ c(
          paste0(substr(.x, 1, 1), .y),
          paste0(.y, substr(.x, 2, 2))))) %>%
        unnest(to)

      solve <- function(steps) {
        for (i in 1:steps) {
          pairs <-
            left_join(pairs, mapper, by = "from") %>%
            count(from = to, wt = n)
        }
        counts <-
          pairs %>%
          separate(from, c("from1", "from2"), sep = 1) %>%
          count(letter = from2, wt = n) %>%
          mutate(n = ifelse(letter == first_letter, n+1, n))
        format(max(counts$n) - min(counts$n), scientific = FALSE)
      }

      part1 <- solve(10)
      part2 <- solve(40)

      list(part1 = part1, part2 = NULL)
    }

## day15

    function() {
      # data
      library(tidyverse)
      file <- system.file("extdata/day15.txt", package = "adventofcode2021")
      input_mat <- do.call(rbind, lapply(strsplit(readLines(file), ""), as.numeric))

      solve <- function(input_mat) {
        # build walls of Inf simulating cells that cannot have their score improved
        input_mat <- cbind(Inf, rbind(Inf, input_mat, Inf), Inf)
        n_rows <- nrow(input_mat)
        # start point
        points <- c(n_rows + 2)

        input_vec <- scores <- c(input_mat)
        #  at first we only know the lowest score to 1st cell is 0
        scores[] <- Inf
        scores[points] <- 0

        # keep going if scores improve
        while (length(points)) {
          old_scores <- scores
          # check if we can improve a score in any of the 4 directions
          for (delta in c(+1, -1, +n_rows, -n_rows)) {
            new_scores <- scores
            new_scores[points + delta] <- scores[points] + input_vec[points + delta]
            scores <- pmin(scores, new_scores) # keep best scores only
          }
          points <- which(scores != old_scores)
        }
        scores[length(scores)-n_rows-1]
      }

      # part1
      part1 <- solve(input_mat)

      # part2
      input_mat2 <- do.call(rbind, replicate(5, input_mat, simplify = F))
      input_mat2 <- do.call(cbind, replicate(5, input_mat2, simplify = F))
      additions <- (col(input_mat2) -1) %/% ncol(input_mat) + (row(input_mat2) -1) %/% nrow(input_mat)
      input_mat2 <- (input_mat2 + additions - 1) %% 9 + 1
      part2 <- solve(input_mat2)

      list(part1 = part1, part2 = NULL)
    }

## day16

    function() {
      # data
      library(tidyverse)
      file <- system.file("extdata/day16.txt", package = "adventofcode2021")
      mapper <- setNames(
        sapply(0:15, \(x) substr(paste(rev(as.integer(intToBits(x))), collapse=""), 29, 32)),
        c(0:9, LETTERS[1:6]))
      bin <- paste(mapper[strsplit(readLines(file), "")[[1]]], collapse = "")

      solve <- function(bin, packet_pos = 1, max_pos = 1) {
        len <- nchar(bin)
        version <- strtoi(substr(bin, 1, 3), base = 2)
        type    <- strtoi(substr(bin, 4, 6), base = 2)
        if(type == 4) {
          split_remainder <- strsplit(bin, "")[[1]][-(1:6)]
          n_litteral <-
            suppressWarnings(which.min(split_remainder == 1 | c(F, T, T, T, T)) + 4)
          value <- split_remainder[1:n_litteral][c(F, T, T, T, T)]
          value <- sum((value==1)*2^(rev(seq_along(value))-1))
          remainder <- substr(bin, 6 + n_litteral + 1, len)
          len <- len - nchar(remainder)
          subpackets <- NULL
          sum_of_versions <- version
        } else {
          length_type <- substr(bin, 7, 7)
          if(length_type == 0) {
            subpackets_length <- strtoi(substr(bin, 7+1, 7+15), base = 2)
            subpackets_bin <- substr(bin, 7+15+1, 7+15+subpackets_length)
            # we have a limit on chars but not on number of subpackets
            subpackets <- solve(subpackets_bin, packet_pos = 1, max_pos = Inf)
            remainder <- substr(bin, 8+15+subpackets_length, len)
            len <- len - nchar(remainder)
          } else {
            subpackets_n <- strtoi(substr(bin, 7+1, 7+11), base = 2)
            # we have a limit on number of subpackets, not on chars
            subpackets_bin <- substr(bin, 7+11+1, len)
            subpackets <- solve(subpackets_bin, packet_pos = 1, max_pos = subpackets_n)
            covered_len <- 7 + 11 + sum(sapply(subpackets, \(x) x$len))
            remainder <- substr(bin, covered_len + 1, len)
            len <- covered_len
          }
          value <- switch(
            as.character(type),
            "0" = sum(sapply(subpackets, \(x) x$value)),
            "1" = prod(sapply(subpackets, \(x) x$value)),
            "2" = min(sapply(subpackets, \(x) x$value)),
            "3" = max(sapply(subpackets, \(x) x$value)),
            "5" = subpackets[[1]]$value > subpackets[[2]]$value,
            "6" = subpackets[[1]]$value < subpackets[[2]]$value,
            "7" = subpackets[[1]]$value == subpackets[[2]]$value
          )
          sum_of_versions <- version + sum(sapply(subpackets, \(x) x$sum_of_versions))
        }
        # 1st subpacket
        res <- list(list(
          value = value,
          len = len,
          subpackets = subpackets,
          sum_of_versions = sum_of_versions))

        # other subpackets
        if(packet_pos != max_pos && remainder != "" && !grepl("^0+$", remainder)) {
          res <- c(res, solve(remainder, packet_pos = packet_pos + 1, max_pos = max_pos))
        }
        res
      }

      packet1 <- solve(bin)[[1]]
      part1 <- packet1$sum_of_versions
      part2 <- packet1$value

      list(part1 = part1, part2 = NULL) # 246225449979
    }

## day17

    function() {
      # data
      library(tidyverse)
      file <- system.file("extdata/day17.txt", package = "adventofcode2021")
      input <- unglue::unglue_data(
        readLines(file),
        "target area: x={x_min}..{x_max}, y={y_min}..{y_max}",
        convert = TRUE)

      # part1
      # we want to in one step as deep as allowed
      # it also takes vy steps to go to the top, since we take back one unit per step
      # and 1+..+n is n*(n-1)/2
      n <- vy_max <- -input$y_min - 1
      part1 <- n * vy_max - n*(n-1)/2

      # part2 tidy

      library(tidyverse)
      max_steps <- 2*vy_max+2

      # all combinations that would work vertically
      y2 <-
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

      list(part1 = part1, part2 = NULL) # 246225449979
    }

## day18

    function() {
      # data
      library(tidyverse)
      file <- system.file("extdata/day18.txt", package = "adventofcode2021")

      add_pair <- function(x, y) {
        snail_num <- paste0("[", x, ",", y, "]")
        chars <- strsplit(snail_num, "")[[1]] # only 1 digit nums at this stage
        repeat {
          first_5th_bracket <- which(cumsum(chars == "[") - cumsum(chars == "]")== 5)[1]
          all_num_pos <- grep("\\d", chars)
          if(!is.na(first_5th_bracket)) {
            # explode
            before_bracket <- which(grep("\\d", chars) > first_5th_bracket)[1] - 1
            num_pos <- all_num_pos[before_bracket + (0:3)]
            if(before_bracket == 0) num_pos <- c(NA, num_pos)
            if(!is.na(num_pos[1]))
              chars[num_pos[1]] <- as.numeric(chars[num_pos[1]]) + as.numeric(chars[num_pos[2]])
            if(!is.na(num_pos[4]))
              chars[num_pos[4]] <- as.numeric(chars[num_pos[4]]) + as.numeric(chars[num_pos[3]])
            chars <- c(chars[1:(first_5th_bracket-1)], "0", chars[(first_5th_bracket+5):length(chars)])
            next
          }
          all_nums <- as.numeric(chars[all_num_pos])
          first_big_num <- all_num_pos[all_nums >= 10][1]
          if(!is.na(first_big_num)) {
            #split
            num <- all_nums[all_nums >= 10][1]
            chars <- c(chars[1:(first_big_num-1)], "[", floor(num/2), "," , ceiling(num/2), "]", chars[(first_big_num+1):length(chars)])
            next
          }
          break
        }
        paste(chars, collapse = "")
      }

      # part1
      input <- readLines(file)
      reduced <- Reduce(add_pair, input)
      reduced_call <- gsub("\\[", "magn(", reduced)
      reduced_call <- gsub("\\]", ")", reduced_call)
      magn <- function(x, y) 3 * x + 2 * y
      part1 <- eval(str2lang(reduced_call))

      # part 2
      grid <- merge(input, input, by = NULL) |> subset(x != y)
      part2 <- with(grid, {
        reduced <- mapply(add_pair, x, y)
        reduced_call <- gsub("\\[", "magn(", reduced)
        reduced_call <- gsub("\\]", ")", reduced_call)
        max(sapply(parse(text=reduced_call), eval))
      })

      list(part1 = part1, part2 = NULL)
    }
