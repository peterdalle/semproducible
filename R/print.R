#' Print semproducible code
#'
#' Prints semproducible code to the console with colors.
#'
#' @param x semproducible code to print.
#' @param ... other arguments passed on to the \code{\link[base:cat]{cat}}
#' function.
#'
#' @return
#' @export
#'
#' @examples
#' library(semproducible)
#'
#' code <- semproducible(iris, drop_non_numeric=TRUE)
#'
#' # Note: print() can be omitted since it is implied
#' code
print.semproducible <- function(x, ...) {
  cat(color_coding_code(x), sep="", ...)
}


color_coding_code <- function(x) {
  lines <- strsplit(x, "\n")[[1]]
  new_lines <- vector("list", length(lines))
  for (i in seq.int(1, length(lines))) {
    line <- lines[i]
    if (startsWith(lines[i], "#")) {
      # Color code comments gray
      line <- crayon::silver(line)
    }
    new_lines[[i]] <- paste0(line, "\n")
  }
  unlist(new_lines)
}
