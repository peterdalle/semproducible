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
#'\dontrun{
#' data <- data.frame(x = rnorm(100), y = rnorm(100))
#'
#' code <- semproducible(data, formula="y ~ x")
#'
#' save_code(code, "create_data.R")
#'
#' save_code(code, "create_data.R", overwrite = TRUE)
#'}
save_code <- function(code, file, overwrite=FALSE) {
  if (file.exists(file) & !overwrite) {
    stop(paste0("File '", file, "' already exist.",
                " Use 'overwrite = TRUE' parameter to overwrite existing file."),
         call. = FALSE)
  }
  fileConn <- file(file)
  writeLines(code, fileConn)
  close(fileConn)
  if (!is_semproducible(code)) {
    warning("The code was not a 'semproducible' object, but was saved successfully.")
  }
}
