#' Code generator for reproducible covariance matrix
#'
#' Generate R code from your data frame or an existing covariance matrix.
#' This is useful when you need to create a reproducible covariance matrix
#' that can be used by a structural equation model (SEM) in lavaan.
#'
#' You supply a data frame with numeric variables (or a covariance matrix that
#' you have already prepared). The function then generates R code that
#' can reproduce the SEM model as a correlation matrix or a covariance matrix.
#'
#' @param x a data frame with numeric variables, or a matrix with correlations
#' or covariances.
#' @param digits number of decimal digits. The default (NULL) will show all
#' available digits as specified by R options. The higher the number of
#' decimal digits, the more accurate the reproducible model will be.
#' @param early_line_break use early line break. This will show a single value
#' per line instead of several values per line. This is only of importance
#' if you plan to print your code or include it in a manuscript or article
#' with a fixed width. Defaults to FALSE.
#' @param method method for creating the matrix, either "cor" for correlations
#' or "cov" for covariances (default). This argument is ignored if "x"
#' is a matrix.
#' @param target_variable target variable name for the generated code. Defaults
#' to "data", but can be anything you want.
#' @param formula optional argument that specifies the lavaan formula syntax
#' that should be included in the code.
#' @param drop_non_numeric drop columns from the data frame that are not
#' numeric. This is useful if you have characters of factors as columns which
#' should not be included in the covariance matrix. Defaults to FALSE.
#' @return Character with the generated R code to reproduce the covariance
#' matrix and the necessary lavaan code to run it.
#' @export
#'
#' @examples
#' \dontrun{
#' # Create random data.
#' set.seed(5543)
#' data <- data.frame(x = rnorm(100),
#'                    y = rnorm(100),
#'                    z = rnorm(100),
#'                    w = rnorm(100),
#'                    q = rnorm(100))
#'
#' # Generate R code for the data.
#' cat(semproducible(data, formula="y ~ x"))
#'
#' # Generate R code for the data, with 5 decimal digits.
#' cat(semproducible(data, formula="y ~ x", digits=5))
#'
#' # Generate the same code, but keep each value on one line for easier reading.
#' cat(semproducible(data, formula="y ~ x", digits=5, early_line_break=TRUE))
#'
#' # Save R code to a file.
#' code <- semproducible(data, formula="y ~ x")
#' fileConn <- file("create_data.r")
#' writeLines(code, fileConn)
#' close(fileConn)
#' }
semproducible <- function(x,
                          digits = NULL,
                          early_line_break = FALSE,
                          method = "cov",
                          formula = "YOUR lavaan MODEL HERE",
                          target_variable = "data",
                          drop_non_numeric = FALSE) {
  # Check inputs for errors.
  if ("matrix" %in% class(x)) {
    cor_mat <- x
  } else if ("data.frame" %in% class(x)) {
  } else {
    stop("x must be of type 'matrix' or 'data.frame'.")
  }

  # TODO: Add drop_non_numeric parameter.

  # Create correlation matrix or covariance matrix.
  if ("data.frame" %in% class(x)) {
    if (method == "cor") {
      cor_mat <- cor(x, use="complete.obs")
    } else if (method == "cov") {
      cor_mat <- cov(x, use="complete.obs")
    } else {
      stop("Method must be 'cor' or 'cov'.")
    }
  }

  if ("data.frame" %in% class(x) & method == "cov") {
    # Check that all columns are numeric, throw error if not.
    non_numeric_cols <- NCOL(x) - sum(sapply(x, is.numeric))
    if (non_numeric_cols != 0) {
      stop(paste("x contain", non_numeric_cols, "non-numeric column(s).",
                 "All columns must be numeric.",
                 "You can use the 'drop_non_numeric = TRUE'",
                 "parameter if you want to drop all non-numeric variables",
                 "automatically."))
    }
  }

  # Get the number of observations in the data frame or matrix.
  num_observations <- NROW(x)

  # Indenting covariance values.
  #indent <- paste0(rep(" ", 14), collapse = "")
  indent <- " "

  # Generate variable names.
  var_names <- ""
  for (variable in row.names(cor_mat)) {
    var_names <- stringi::stri_c(var_names, "~", variable, ", ")
  }
  var_names <- base::substr(var_names, 1, nchar(var_names) - 2)


  # TODO: Use utils::capture.output instead? Well, no because
  # then we probably can't format it to fit 80 chars width.

  # Generate values.
  values <- ""
  for (i in seq.int(ncol(cor_mat))) {
    if (!early_line_break) {
      values <- stringi::stri_c(values, indent, sep="")
    }
    for (j in seq(ncol(cor_mat))) {
      if (is.null(digits)) {
        value <- cor_mat[i, j]
      } else {
        value <- round(cor_mat[i, j], digits=digits)
      }
      values <- stringi::stri_c(values, value, ", ", sep="")
      if (early_line_break) {
        values <- stringi::stri_c(values, "\n")
      }
    }
    if (!early_line_break) {
      values <- stringi::stri_c(values, indent, "\n")
    }
  }
  # Remove superfluous comma (,) at the end of values.
  values <- trimws(values, which = "right")
  values <- substr(values, 1, nchar(values) - 1)

  # Code template.
  code <- "library(tibble)
library(lavaan)

# Number of observations.
num_observations <- %%OBSERVATIONS%%

# Covariance matrix.
%%TARGET%% <- tribble(%%VARIABLES%%,
%%VALUES%%)

# Convert tibble to matrix (that lavaan can handle).
%%TARGET%% <- as.matrix(%%TARGET%%)

# Rows should have a name too.
rownames(%%TARGET%%) <- colnames(%%TARGET%%)

# SEM model in lavaan syntax.
my_model <- '%%FORMULA%%'

# Fit SEM model.
fit <- lavaan::sem(model = my_model,
                   sample.cov = %%TARGET%%,
                   sample.nobs = num_observations)

# Show results.
summary(fit)"

  # Replace placeholders with variable names and variable values.
  code <- gsub("%%OBSERVATIONS%%", num_observations, code)
  code <- gsub("%%FORMULA%%", formula, code)
  code <- gsub("%%TARGET%%", target_variable, code)
  code <- gsub("%%VARIABLES%%", var_names, code)
  code <- gsub("%%VALUES%%", values, code)
  return(code)
}
