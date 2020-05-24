#' Save generated code to a file
#'
#' Helper function that saves generated R code to a file. Does not overwrite
#' existing files unless \code{overwrite} is explicitly set to \code{TRUE}.
#'
#' @param code character string with R code.
#' @param file character string of the filename to be saved.
#' @param overwrite whether the existing files should be overwritten.
#' @export
#'
#' @examples
#' \dontrun{
#' # Create random data.
#' data <- data.frame(x = rnorm(100), y = rnorm(100))
#'
#' # Generate R code.
#' code <- semproducible(data, formula="y ~ x")
#'
#' # Save R code to file.
#' save_code(code, "create_data.r")
#'
#' # Save again, but the file exists so overwrite=TRUE is necessary.
#' save_code(code, "create_data.r", overwrite = TRUE)
#' }
save_code <- function(code, file, overwrite=FALSE) {
  if (file.exists(file) & !overwrite) {
    stop(paste0("File '", file, "' already exist.",
                " Use 'overwrite = TRUE' parameter to overwrite existing file."),
         call. = FALSE)
  }
  fileConn <- file(file)
  writeLines(code, fileConn)
  close(fileConn)
}
