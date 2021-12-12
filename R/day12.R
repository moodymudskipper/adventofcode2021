#' day 11
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
  visited <- setNames(rep(FALSE, length(nodes)), nodes)
  visited[nodes == toupper(nodes)] <- NA

  rec <- function(node, visited = visited, extra) {
    if(isTRUE(visited[node])) {
      if (!extra) return(0)
      visited[node] <- FALSE
      extra <- FALSE
    }
    if(node == "end") return(1)
    visited[node] <- !visited[node]
    sum(sapply(edges[[node]], rec, visited, extra))
  }
  part1 <- solve(extra = FALSE)
  part2 <- solve(extra = TRUE)

  list(part1 = part1, part2 = part2)
}
