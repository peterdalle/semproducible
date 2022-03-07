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
#' \item{\code{\%\%VARIABLES\%\%}} = column names (variable names) of
#' the covariance matrix.
#' \item{\code{\%\%VALUES\%\%}} = actual values of the covariance matrix.
#' \item{\code{\%\%LAVAAN_CALL\%\%}} = \code{\link[lavaan:sem]{lavaan}} function
#' call.
#' \item{\code{\%\%COVMAT_VARIABLE\%\%}} = variable name of the covariance matrix.
#' \item{\code{\%\%FIT_VARIABLE\%\%}} = variable name of the lavaan object that
#' is created from \code{\link[lavaan:sem]{lavaan}}.
#' }
#'
#' @return a character string with the R code template.
#' @export
#'
#' @examples
#' data <- data.frame(x = rnorm(100), y = rnorm(100))
#'
#' code <- semproducible(data, formula="y ~ x", template = code_template())
#'
#' # Modify code template
#' template <- paste(code_template(), "\n# Adds a comment at end")
#'
#' code <- semproducible(data, formula="y ~ x", template = template)
#'
#' # Look at the new code
#' cat(code)
code_template <- function() {
  return("library(tibble)
library(lavaan)

# Number of observations.
observations <- %%OBSERVATIONS%%

# Covariance matrix.
%%COVMAT_VARIABLE%% <- tribble(%%VARIABLES%%
%%VALUES%%)

# Convert tibble to matrix (that lavaan can handle).
%%COVMAT_VARIABLE%% <- as.matrix(%%COVMAT_VARIABLE%%)

# Rows should have names too.
rownames(%%COVMAT_VARIABLE%%) <- colnames(%%COVMAT_VARIABLE%%)

# SEM model in lavaan syntax.
formula <- '%%FORMULA%%'

# Fit SEM model.
%%FIT_VARIABLE%% <- %%LAVAAN_CALL%%

# Show results.
summary(%%FIT_VARIABLE%%)")
}


get_default_lavaan_call <- function() {
  return(paste("lavaan::sem(model = formula,",
               "sample.cov = %%COVMAT_VARIABLE%%,",
               "sample.nobs = observations)"))
}


replace_code_template_placeholders <- function(code, params) {
  code <- gsub("%%LAVAAN_CALL%%", params$call, code)
  code <- gsub("%%OBSERVATIONS%%", params$num_observations, code)
  code <- gsub("%%FORMULA%%", params$formula, code)
  code <- gsub("%%VARIABLES%%", params$var_names, code)
  code <- gsub("%%VALUES%%", params$values, code)
  code <- gsub("%%COVMAT_VARIABLE%%", params$covmat_variable, code)
  code <- gsub("%%FIT_VARIABLE%%", params$fit_variable, code)
  return(code)
}
