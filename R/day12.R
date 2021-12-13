#' day 12
#'
#' @export
day12 <- function() {
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
