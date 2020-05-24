#' Template for the generated R code
#'
#' Default R code template used by \code{\link{semproducible}} when generating
#' the code.
#'
#' The template is a string with R code that contain placeholders in the form
#' of variables like \code{\%\%NAME\%\%}. The variables are replaced with
#' actual content of the SEM model during code generation.
#'
#' Placeholder variables:
#'
#' \itemize{
#' \item{\code{\%\%FORMULA\%\%}} = \code{\link[lavaan:sem]{lavaan}} formula.
#' \item{\code{\%\%OBSERVATIONS\%\%}} = number of observations of the actual data.
#' \item{\code{\%\%TARGET\%\%}} = variable name of the covariance matrix.
#' \item{\code{\%\%VARIABLES\%\%}} = column names (variable names) of
#' the covariance matrix.
#' \item{\code{\%\%VALUES\%\%}} = actual values of the covariance matrix.
#' \item{\code{\%\%LAVAAN_CALL\%\%}} = \code{\link[lavaan:sem]{lavaan}} function
#' call.
#' }
#'
#' @return a character string with the R code template.
#' @export
#'
#' @examples
#' \dontrun{
#' # Create random data.
#' data <- data.frame(x = rnorm(100), y = rnorm(100))
#'
#' # Modify code template.
#' template <- paste(code_template(), "\n# A comment placed at end of code")
#'
#' # Generate R code with the template.
#' code <- semproducible(data, formula="y ~ x", template = template)
#' }
code_template <- function() {
  code <- "library(tibble)
library(lavaan)

# Number of observations.
observations <- %%OBSERVATIONS%%

# Covariance matrix.
%%TARGET%% <- tribble(%%VARIABLES%%
%%VALUES%%)

# Convert tibble to matrix (that lavaan can handle).
%%TARGET%% <- as.matrix(%%TARGET%%)

# Rows should have names too.
rownames(%%TARGET%%) <- colnames(%%TARGET%%)

# SEM model in lavaan syntax.
model <- '%%FORMULA%%'

# Fit SEM model.
fit <- %%LAVAAN_CALL%%

# Show results.
summary(fit)"
  return(code)
}
