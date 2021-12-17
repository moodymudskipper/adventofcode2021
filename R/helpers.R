# print.aoc_fun <- function(x, ...) {
#   cat(attr(x, "example"), "\n")
#   invisible(x)
# }
#
# `+.aoc_fun` <- function(x, ...) {
#   attributes(x) <- NULL
#   x
# }
#
# add_example <- function(fun, example) {
#   code <- paste(deparse(substitute(example)), collapse= "\n")
#   value <- capture.output(example)
#   fun_nm <- as.character(substitute(fun))
#   attr(fun, "example") <- paste(
#     collapse = "\n",
#     c(cli::col_blue(code),
#       cli::col_yellow(value))
#   )
#   class(fun) <- "aoc_fun"
#   assign(fun_nm, fun, parent.frame())
# }
#
# str2vec <- function(x, sep = "") strsplit(x, sep)[[1]]
# add_example(str2vec, str2vec("abc"))
#
# str2num <- function(x, chr, ..., sep = "") {
#   x <- strsplit(x, sep)[[1]]
#   chr <- strsplit(chr, sep)[[1]]
#   nums <- c(...)
#   nums <- setNames(nums, chr)
#   nums[x]
# }
# add_example(str2num, str2num("bab", "ab", 1, -1))
