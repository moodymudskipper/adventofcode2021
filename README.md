
<!-- README.md is generated from README.Rmd. Please edit that file -->

# adventofcode2020

My solutions for adventofcode2020. You can install it with

``` r
remotes::install_github("moodymudskipper/adventofcode2020")
```

The package contains functions `day1`, `day2` etc, the readme displays
the body of these functions.

## day 1

    function() {
      # for notes
      Var1 <- Var2 <- Var3 <- . <- NULL
      # dependencies
      `%>%` <- magrittr::`%>%`
    
      # data
      file <- system.file("extdata/day1.txt", package = "adventofcode2020")
      input <- scan(file, what = numeric(), sep = "\n", quiet = TRUE)
    
      # part 1
      part1 <-
        input %>%
        expand.grid(.,.) %>%
        subset(Var1 < Var2 & Var1+Var2 == 2020) %>%
        prod()
    
      # part 2
      part2 <-
        input %>%
        expand.grid(.,.,.) %>%
        subset(Var1 < Var2 & Var2 < Var3 & Var1+Var2+Var3 == 2020) %>%
        prod()
    
      list(part1 = part1, part2 = part2)
    }

## day 2

    function() {
      # for notes
      letter <- password <- count <- pos1 <- pos2 <- NULL
    
      # dependencies
      `%>%` <- magrittr::`%>%`
      unglue_data <- unglue::unglue_data
    
      # data
      file <- system.file("extdata/day2.txt", package = "adventofcode2020")
      input <- readLines(file)
    
      # part 1
      part1 <-
        unglue_data(input, "{min}-{max} {letter}: {password}", convert = TRUE) %>%
        transform(count = mapply(
          function(l, ls) sum(l == ls),
          letter,
          strsplit(password, ""))) %>%
        subset(count >= min & count <= max) %>%
        nrow()
    
      # part 2
      part2 <-
        unglue_data(input, "{pos1}-{pos2} {letter}: {password}", convert = TRUE) %>%
        subset(mapply(
          function(l, ls, pos1, pos2) sum(l == ls[c(pos1,pos2)]) == 1,
          letter,
          strsplit(password, ""),
          pos1,
          pos2)) %>%
        nrow()
    
      list(part1 = part1, part2 = part2)
    }

## day 3

    function() {
      # data
      file <- system.file("extdata/day3.txt", package = "adventofcode2020")
      input <- readLines(file)
    
      # part 1
    
      # make input a logical matrix of trees
      trees_mat <- as.matrix(do.call(rbind, strsplit(input, ""))) == "#"
      # make sure it's at least 3 times wider then long
      mult <- ceiling(3 * nrow(trees_mat) / ncol(trees_mat))
      trees_mat <- do.call(cbind, replicate(mult, trees_mat, simplify = FALSE))
      # transpose so we reaching bottom means reaching end of matrix
      trees_mat <- t(trees_mat)
      part1 <- sum(trees_mat[seq(1, length(trees_mat), 3 + nrow(trees_mat))])
    
      # part 2
    
      # same as part 1
      trees_mat <- as.matrix(do.call(rbind, strsplit(input, ""))) == "#"
    
      # wrap part 1 in function (we keep trees_map global)
      count_trees <- function(right, down) {
        mult <- ceiling(right * nrow(trees_mat) / ncol(trees_mat))
        trees_mat <- do.call(cbind, replicate(mult, trees_mat, simplify = FALSE))
        # transpose so we reaching bottom means reaching end of matrix
        trees_mat <- t(trees_mat)
        sum(trees_mat[seq(1, length(trees_mat), right + nrow(trees_mat)*down)])
      }
    
      part2 <- prod(mapply(count_trees, c(1,3,5,7,1), c(1,1,1,1,2)))
    
      list(part1 = part1, part2 = part2)
    }

## day 4

    function() {
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

## day 5

    function() {
      # data
      file <- system.file("extdata/day5.txt", package = "adventofcode2020")
      input <- readLines(file)
    
      # part 1
      all_ids <- strtoi(chartr("FBLR", "0101", input), base = 2)
      part1 <- max(all_ids)
    
      # part 2
      part2 <- tail(setdiff(seq(max(all_ids)), all_ids), 1)
      list(part1 = part1, part2 = part2)
    }

## day 6

    function() {
      # for notes
      . <- NULL
    
      # dependencies
      `%>%` <- magrittr::`%>%`
    
      # data
      file <- system.file("extdata/day6.txt", package = "adventofcode2020")
      input <- paste(readLines(file), collapse = "\n")
    
      # part 1
      part1 <-
        strsplit(input, "\n\n")[[1]] %>%
        gsub("\n", "", .) %>%
        strsplit("") %>%
        lapply(unique) %>%
        lengths() %>%
        sum()
    
      # part 2
      part2 <-
        strsplit(input, "\n\n")[[1]] %>%
        strsplit("\n") %>%
        sapply(function(x) {
          l <- length(x)
          tab <- strsplit(x, "") %>%
            unlist() %>%
            table()
          sum(tab  == l)
        }) %>%
        sum()
    
      list(part1 = part1, part2 = part2)
    }

## day 7

    function() {
      # for notes
      key <- value <- color <- parent_color <- i <- NULL
    
      # dependencies
      `%>%` <- magrittr::`%>%`
      unglue_data <- unglue::unglue_data
      filter      <- dplyr::filter
      mutate_at   <- dplyr::mutate_at
      select      <- dplyr::select
      gather      <- tidyr::gather
      separate    <- tidyr::separate
      spread      <- tidyr::spread
    
      # data
      file <- system.file("extdata/day7.txt", package = "adventofcode2020")
      input <- readLines(file)
    
      # part 1
      patterns <- c(
        "{parent_color} bags contain no other bags.",
        "{parent_color} bags contain {n_1} {color_1} bag{}, {n_2} {color_2} bag{}, {n_3} {color_3} bag{}, {n_4} {color_4} bag{}.",
        "{parent_color} bags contain {n_1} {color_1} bag{}, {n_2} {color_2} bag{}, {n_3} {color_3} bag{}.",
        "{parent_color} bags contain {n_1} {color_1} bag{}, {n_2} {color_2} bag{}.",
        "{parent_color} bags contain {n_1} {color_1} bag{}."
      )
    
      clean_data <-
        input %>%
        unglue_data(patterns) %>%
        gather(key,value,-1) %>%
        separate(key, c("col", "i")) %>%
        spread(col, value) %>%
        select(-i) %>%
        filter(!is.na(color)) %>%
        mutate_at("n", as.numeric)
    
      find_parents <- function(col) {
        direct_parents <- filter(clean_data, color %in% col)$parent_color
        if(length(direct_parents))
          unique(c(direct_parents, find_parents(direct_parents)))
        else
          character(0)
      }
    
      part1 <- length(find_parents("shiny gold"))
    
      # part 2
      count_children <- function(col) {
        children <- filter(clean_data, parent_color == col)
        if(nrow(children))
          sum(children$n) + sum(children$n * sapply(children$color, count_children))
        else
          0
      }
    
      part2 <- count_children("shiny gold")
    
      list(part1 = part1, part2 = part2)
    }

## day 8

    function() {
      # dependencies
      `%>%`       <- magrittr::`%>%`
      unglue_data <- unglue::unglue_data
      mutate      <- dplyr::mutate
    
      # data
      file <- system.file("extdata/day8.txt", package = "adventofcode2020")
      input <- readLines(file)
    
      # part 1
      clean_data <- clean_data1 <-
        unglue_data(input, "{call} {value}", convert = TRUE) %>%
        mutate(passes = 0)
    
      i <- 1
      passes <- 0
      glob <- 0
      repeat {
        passes <- clean_data1[i, "passes"] + 1
        clean_data1[i, "passes"] <- passes
        if(passes == 2) break
        call  <- clean_data1[i, "call"]
        value <- clean_data1[i, "value"]
        if(call == "acc") {
          glob <- glob + value
          i <- i + 1
        } else if(call == "jmp") {
          i <- i + value
        } else {
          i <- i + 1
        }
      }
      part1 <- glob
    
      # part 2
    
      # return NULL if inf loop, glob otherwise
      test_code <- function(clean_data) {
        i <- 1
        passes <- 0
        glob <- 0
        repeat {
          passes <- clean_data[i, "passes"] + 1
          clean_data[i, "passes"] <- passes
          if(passes == 2) return(NULL)
          call  <- clean_data[i, "call"]
          value <- clean_data[i, "value"]
          if(call == "acc") {
            glob <- glob + value
            i <- i + 1
          } else if(call == "jmp") {
            i <- i + value
          } else {
            i <- i + 1
          }
          if(i == nrow(clean_data)) return(glob)
        }
      }
    
      for (j in seq(nrow(clean_data))) {
        call  <- clean_data[j, "call"]
        clean_data_modif <- clean_data
        if(call == "nop") {
          clean_data_modif[j, "call"] <- "jmp"
          res <- test_code(clean_data_modif)
          if(!is.null(res)) break
        } else   if(call == "jmp") {
          clean_data_modif[j, "call"] <- "nop"
          res <- test_code(clean_data_modif)
          if(!is.null(res)) break
        }
      }
      part2 <- res
    
      list(part1 = part1, part2 = part2)
    }

## day 9

    function() {
      # for notes
      . <- Var1 <- Var2 <- NULL
    
      # dependencies
      `%>%` <- magrittr::`%>%`
    
      # data
      file <- system.file("extdata/day9.txt", package = "adventofcode2020")
      input <- scan(file, what = numeric(), sep = "\n", quiet = TRUE)
    
      # part 1
      i <- 26
      while(input[i] %in% (
        input[(i-25):(i-1)] %>% expand.grid(.,.) %>% subset(Var1 != Var2) %>% rowSums())
      ) i <- i +1
      part1 <- input[i]
    
      # part 2
      j <- 1
      while(!part1 %in% (tmp <- cumsum(input[j:(i-1)]))) j <- j+1
      range <- input[j:(which(part1 == tmp) + j - 1)]
      part2 <- max(range) + min(range)
    
      list(part1 = part1, part2 = part2)
    }

## day 10

    function() {
    
      # data
      file <- system.file("extdata/day10.txt", package = "adventofcode2020")
      input <- scan(file, what = numeric(), sep = "\n", quiet = TRUE)
    
      # part 1
    
      diffs <- c(diff(c(0,sort(input))), 3)
      tab   <- tabulate(diffs)
      part1 <- prod(tab[c(1,3)])
    
      # jumps are 3 at most, and tab shows we have no jump of 2, thus we can
      # find rles of 1s and find all possible combinations of 1,2 and 3 that we
      # can build from those.
      # we find that the max running length of 1s is 4, small enough to do manually :
      # max(rle(diff(sort(input)))$lengths) # 4
      # 4 can be expressed in 7 ways :
      # 4 = 1 + 1 + 1 + 1
      # 4 = 1 + 1 + 2
      # 4 = 1 + 2 + 1
      # 4 = 2 + 1 + 1
      # 4 = 2 + 2
      # 4 = 1 + 3
      # 4 = 3 + 1
      # 3 can be expressed in 4 ways
      # 3 = 1 + 1 + 1
      # 3 = 1 + 2
      # 3 = 2 + 1
      # 3 = 3
      # 2 can be expressed in 2 ways
      # 2 = 1 + 1
      # 2 = 2
    
      # part 2
      multiplier <- c(1,2,4,7)
      rle_1s     <- with(rle(diffs), lengths[values == 1])
      part2 <- prod(multiplier[rle_1s])
      part2 <- format(part2, scientific = FALSE) # to see all digits
    
      list(part1 = part1, part2 = part2)
    }

## day 11

part 2 is wrong, works only on example.

    function() {
      # for notes
      . <- NULL
    
      # dependencies
      `%>%` <- magrittr::`%>%`
    
      # data
      file <- system.file("extdata/day11.txt", package = "adventofcode2020")
      input <- readLines(file)
    
    #   # example
    #   input <- "L.LL.LL.LL
    # LLLLLLL.LL
    # L.L.L..L..
    # LLLL.LL.LL
    # L.LL.LL.LL
    # L.LLLLL.LL
    # ..L.L.....
    # LLLLLLLLLL
    # L.LLLLLL.L
    # L.LLLLL.LL"
    # input <- strsplit(input, "\n")[[1]]
      mat <- as.matrix(do.call(rbind, strsplit(input, "")))
      bkp_mat <-  mat
      nr <- nrow(mat)
      nc <- ncol(mat)
    
      # part 1
    
      repeat {
        bkp_mat1 <- mat
        taken_mat <- mat == "#"
        # build 8 padded matrices
        left_padded     <- cbind(FALSE, taken_mat[,-nc])
        right_padded    <- cbind(taken_mat[,-1], FALSE)
        top_padded      <- rbind(FALSE, taken_mat[-nr,])
        bottom_padded   <- rbind(taken_mat[-1,], FALSE)
        top_left_padded       <- rbind(FALSE, left_padded[-nr,])
        bottom_left_padded    <- rbind(left_padded[-1,], FALSE)
        top_right_padded      <- rbind(FALSE, right_padded[-nr,])
        bottom_right_padded   <- rbind(right_padded[-1,], FALSE)
    
        # sum `#` of padded matrices gives number of neighbouring `#`
        n_adj_taken <-
          left_padded + right_padded + top_padded + bottom_padded +
          top_left_padded + bottom_left_padded + top_right_padded + bottom_right_padded
    
        mat[] <- ifelse(mat == "L" & !n_adj_taken, "#",
                        ifelse(mat == "#" & n_adj_taken >= 4, "L", mat))
        if(identical(mat, bkp_mat1))
          break
      }
      part1 <- sum(mat == "#")
    
      # part 2
      mat <- bkp_mat
      # create an indicator for all diagonals in the matrix
      downward_diag_ind <- row(mat) - col(mat)
      upward_diag_ind   <- row(mat) + col(mat)
      mat_try_list <- list()
      repeat {
        bkp_mat2 <- mat
        mat_try_list[[length(mat_try_list) + 1]] <- mat
    
        # "#" must be contagious to rightmost "." for left_padded, and so on
        # we first build rles for all directions
        hor_rle <- apply(mat, 1, rle)
        ver_rle <- apply(mat, 2, rle)
        upward_diag_rle   <- lapply(split(mat, upward_diag_ind), rle)
        downward_diag_rle <- lapply(split(mat, downward_diag_ind), rle)
    
        # build 8 padded matrices
        left_padded     <-
          lapply(hor_rle, function(x) {
            l <- length(x$values)
          x$values[-1][x$values[-l] == "#" & x$values[-1] == "."] <- "#"
          inverse.rle(x)
        }) %>%
          do.call(rbind, .) %>%
          {cbind(".", .[,-nc])} %>%
          `==` ("#")
    
        right_padded     <-
          lapply(hor_rle, function(x) {
            l <- length(x$values)
            x$values[-l][x$values[-1] == "#" & x$values[-l] == "."] <- "#"
            inverse.rle(x)
          }) %>%
          do.call(rbind, .) %>%
          {cbind(.[,-1], ".")} %>%
          `==` ("#")
    
        top_padded     <-
          lapply(ver_rle, function(x) {
            l <- length(x$values)
            x$values[-1][x$values[-l] == "#" & x$values[-1] == "."] <- "#"
            inverse.rle(x)
          }) %>%
          do.call(cbind, .) %>%
          {rbind(".", .[-nc,])} %>%
          `==` ("#")
    
        bottom_padded     <-
          lapply(ver_rle, function(x) {
            l <- length(x$values)
            x$values[-l][x$values[-1] == "#" & x$values[-l] == "."] <- "#"
            inverse.rle(x)
          }) %>%
          do.call(cbind, .) %>%
          {rbind(.[-1,], ".")} %>%
          `==` ("#")
    
        top_left_padded        <- mat
        split(top_left_padded, downward_diag_ind) <-
          lapply(downward_diag_rle, function(x) {
            l <- length(x$values)
            x$values[-1][x$values[-l] == "#" & x$values[-1] == "."] <- "#"
            inverse.rle(x)
          })
        top_left_padded <-
          top_left_padded %>%
          {rbind(".", .[-nc,])} %>%
          {cbind(".", .[,-nc])} %>%
          `==` ("#")
    
        bottom_left_padded      <- mat
        split(bottom_left_padded, upward_diag_ind) <-
          lapply(upward_diag_rle, function(x) {
            l <- length(x$values)
            x$values[-1][x$values[-l] == "#" & x$values[-1] == "."] <- "#"
            inverse.rle(x)
          })
        bottom_left_padded <-
          bottom_left_padded %>%
          {rbind(.[-1,], ".")} %>%
          {cbind(".", .[,-nc])} %>%
          `==` ("#")
    
        top_right_padded        <- mat
        split(top_right_padded, upward_diag_ind) <-
          lapply(upward_diag_rle, function(x) {
            l <- length(x$values)
            x$values[-l][x$values[-1] == "#" & x$values[-l] == "."] <- "#"
            inverse.rle(x)
          })
        top_right_padded <-
          top_right_padded %>%
          {rbind(".", .[-nc,])} %>%
          {cbind(.[,-1], ".")} %>%
          `==` ("#")
    
        bottom_right_padded        <- mat
        split(bottom_right_padded, downward_diag_ind) <-
          lapply(downward_diag_rle, function(x) {
            l <- length(x$values)
            x$values[-l][x$values[-1] == "#" & x$values[-l] == "."] <- "#"
            inverse.rle(x)
          })
        bottom_right_padded <-
          bottom_right_padded %>%
          {rbind(.[-1,], ".")} %>%
          {cbind(.[,-1], ".")} %>%
          `==` ("#")
    
        n_adj_taken <- Reduce(`+`, list(
          left_padded, right_padded, top_padded, bottom_padded,
          top_left_padded, bottom_left_padded, top_right_padded, bottom_right_padded))
        #n_adj_taken <- F
        mat[] <- ifelse(mat == "L" & !n_adj_taken, "#",
                        ifelse(mat == "#" & n_adj_taken >= 5, "L", mat))
    
        if(identical(mat, bkp_mat2))
          break
      }
      part2 <- sum(mat == "#") # should not be 2219
    
      list(part1 = part1, part2 = part2)
    }

## day 12

    function() {
      # for notes
      angle_incr <- NULL
    
      # dependencies
      `%>%`       <- magrittr::`%>%`
      unglue_data <- unglue::unglue_data
      case_when   <- dplyr::case_when
      mutate      <- dplyr::mutate
    
      # data
      file <- system.file("extdata/day12.txt", package = "adventofcode2020")
      input <- readLines(file)
      data <- unglue_data(input, "{dir=.}{amount}", convert = TRUE)
    
      # part1
    
      data1 <- data %>%
        mutate(
          amount = as.numeric(amount),
          angle_incr = case_when(
            dir == "L" ~ amount,
            dir == "R" ~ -amount,
            TRUE ~ 0
          ),
          angle_total = cumsum(angle_incr),
          y_incr = case_when(
            dir == "N" ~ amount,
            dir == "S" ~ -amount,
            dir == "F" ~ amount * sin(angle_total * pi/180)),
          x_incr = case_when(
            dir == "E" ~ amount,
            dir == "W" ~ -amount,
            dir == "F" ~ amount * cos(angle_total * pi/180))
        )
      part1 <- abs(sum(data1$y_incr, na.rm = TRUE)) + abs(sum(data1$x_incr, na.rm = TRUE))
    
      # part2
      waypoint <- c(10,1)
      boat <- c(0,0)
    
      for(i in seq(nrow(data))) {
        dir <- data[i, "dir"]
        amount <- data[i, "amount"]
        if(dir == "L")
          waypoint <- switch(
            as.character(amount),
            "90"  = c(-waypoint[2],  waypoint[1]),
            "180" = c(-waypoint[1], -waypoint[2]),
            "270" = c(waypoint[2],  -waypoint[1]))
        else if(dir == "R")
          waypoint <- switch(
            as.character(amount),
            "90"  = c(waypoint[2],  -waypoint[1]),
            "180" = c(-waypoint[1], -waypoint[2]),
            "270" = c(-waypoint[2],  waypoint[1]))
        else if(dir == "N")
          waypoint[2] <- waypoint[2] + amount
        else if(dir == "S")
          waypoint[2] <- waypoint[2] - amount
        else if(dir == "E")
          waypoint[1] <- waypoint[1] + amount
        else if(dir == "W")
          waypoint[1] <- waypoint[1] - amount
        else if(dir == "F")
          boat <- boat + amount * waypoint
      }
      part2 <- sum(abs(boat))
    
      list(part1 = part1, part2 = part2)
    }

## day 13

I skipped part 2 here

    function() {
      # dependencies
      `%>%` <- magrittr::`%>%`
    
      # data
      file <- system.file("extdata/day13.txt", package = "adventofcode2020")
      input <- readLines(file)
    
      # part 1
    
      start <- as.numeric(input[1])
      ids   <- as.numeric(setdiff(strsplit(input[2],",")[[1]], "x"))
      next_dep <- ids - start %% ids
      i <- which.min(next_dep)
      part1 <- ids[i] * next_dep[i]
    
      # part 2
    
      part2 <- NULL
    
      list(part1 = part1, part2 = part2)
    }

## day 14

    function() {
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

## day 15

    function() {
      # for notes
      last_pos_i <- NULL
    
      input <- "1,0,15,2,10,13"
    
      # part 1
    
      data    <- rev(as.numeric(strsplit(input, ",")[[1]]))
      start <- length(data)+1
      for (i in start:2020) {
        new  <- c(which(data[1] == data[-1]), 0)[1]
        data <- c(new, data)
      }
      part1 <- new
    
      # part 2
    
      # build the data
      data    <- as.integer(strsplit(input, ",")[[1]])
    
      # build a vector long enough to store the last positions of every value
      # the index is the number, the value will be the position (0 when initiating)
      # note : character indexing was too slow
      last_pos  <- integer(3e7)
    
      # we need to store positions of zeroes too, so we offset the data by 1
      data <- data + 1
    
      # and we fill in the data that we have, except the last number, because
      # we'll need to test its previous last position
      last_pos[head(data,-1)] <- seq_along(head(data,-1))
      last_num <- tail(data,1)
    
      start <- length(data)
      end <- 3e7 - 1
      for (i in start:end) {
        last_pos_of_last_num <- last_pos[last_num]
        # update the position of the last num
        last_pos[last_num] <- i
        last_num <- if(last_pos_i) i - last_pos_i + 1 else 1
        if(last_pos_of_last_num) {
          # compute the new num as the offset (don't forget to add 1)
          last_num <-  i - last_pos_of_last_num + 1
        } else {
          # update the position of the last num and update the last num to 1 (0+1)
          last_pos[last_num] <- i
          last_num <- 1
        }
      }
      part2 <- last_num - 1
    
      list(part1 = part1, part2 = part2)
    }

## day 16

    function() {
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

## day 17

    function() {
      input <- c("####...#",
                 "......#.",
                 "#..#.##.",
                 ".#...#.#",
                 "..###.#.",
                 "##.###..",
                 ".#...###",
                 ".##....#")
    
      data <- do.call(rbind, strsplit(input, "")) == "#"
    
      # a wrapper with better arg order and defaults
      replicate2 <- function(x, n) replicate(n, x, simplify = FALSE)
    
      # convert list to dots to avoid horrible do.call calls
      as_dots <- function(x) {
        f <- function(...) environment()$...
        do.call(f, as.list(x))
      }
    
      solve <- function(data0, n_dim, n_cycles) {
        # create empty array, we make it cubic to simplify later computations
        side0 <- nrow(data0)
        side  <- 2*n_cycles + side0 + 2
        mid <- 2:(side-1) # indices to build central square/cube
        data <- array(dim = rep(side, n_dim)) & FALSE
    
        # fill in initial data
        init_ind <- c(
          replicate2(n_cycles+1+seq(side0), 2),   # skip margins
          rep(n_cycles+1+(side0+1)/2, n_dim - 2)) # keep central layer
        `...` <- as_dots(init_ind)
        data[...] <- data0
    
        # build offset cubes
        `...` <- as_dots(replicate2(1:3, n_dim))
        meta_indices <- expand.grid(...)
    
        for(i in seq(n_cycles)) {
          `...` <- as_dots(meta_indices)
          offset_cubes <- Map(..., f = function(...){
            `...` <- as_dots(list(mid-1, mid, mid+1)[c(...)])
            data[...]})
    
          # compute active neighbours and update data
          `...` <- as_dots(replicate2(mid, n_dim))
          data_mid <- data[...]
          act_nb <- Reduce("+", offset_cubes) - data_mid
    
          `...` <- as_dots(replicate2(mid, n_dim))
          data[...] <- (data_mid & act_nb %in% 2:3) | (!data_mid & act_nb == 3)
        }
        sum(data)
      }
    
      list(part1 = solve(data, 3, 6), part2 =solve(data, 4, 6))
    }

## day 18

    function() {
    
      file <- system.file("extdata/day18.txt", package = "adventofcode2020")
      input <- readLines(file)
    
      # part 1
      `-` <- `*`
      input <- gsub("\\*", "\\-", input)
      part1 <- sum(eval(as.call(c(c, parse(text = input)))))
    
      # part 2
      `*` <- `+`
      input <- gsub("\\+", "\\*", input)
      part2 <- sum(eval(as.call(c(c, parse(text = input)))))
      format(part2, scientific = FALSE)
    
      list(part1, part2)
    }

## day 19

    function() {
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
