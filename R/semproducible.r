#' Code generator for reproducible covariance matrix
#'
#' Generate R code from your data frame or an existing covariance matrix.
#' This is useful when you need to create a reproducible covariance matrix
#' that can be used by a structural equation model (SEM) in \pkg{lavaan}.
#'
#' You supply a data frame with numeric variables (or a covariance matrix that
#' you have already prepared). The function then generates R code that
#' can reproduce the SEM model as a covariance matrix.
#'
#' @param x a data frame with numeric variables, or a covariance matrix.
#' @param digits number of decimal digits. The default (NULL) will show all
#' available digits as specified by R options. The higher the number of
#' decimal digits, the more accurate the reproducible model will be.
#' @param use character string for computing the
#' covariances in the presence of missing values. This must be one of the
#' strings "everything", "all.obs", "complete.obs", "na.or.complete", or
#' "pairwise.complete.obs". This value is passed on to the \code{use} parameter
#' of the \code{\link{cov}} function
#' @param target_variable character string with arbitrary target variable name
#' for the generated covariance matrix. Defaults to "cov_mat".
#' @param formula character string with lavaan formula syntax that should be
#' included in the code.
#' @param drop_non_numeric whether or not non-numeric columns should be dropped
#' from the data frame. This is useful if you have characters of factors as
#' columns which should not be included in the covariance matrix. Defaults to
#' FALSE.
#' @param vars_per_line number of variables (or values) per line. Many
#' variables per line will increase the width of the code.
#' @param eval whether or not the generated code and lavaan model will be
#' executed. If eval is set to TRUE, a message will tell you whether the
#' code executed withour errors or not.
#' @param print whether or not to print the code to the screen or return the
#' code as a character string (default). Printing to screen is useful during
#' development.
#' @param template a character string with a custom code template that
#' is used when generating the R code. See the \code{\link{code_template}}
#' function for instructions on how to write your own template. A NULL value
#' (default) will use the default template from \code{\link{code_template}}.
#' @return a character string with the generated R code.
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
#' # Use 5 decimal digits.
#' cat(semproducible(data, formula="y ~ x", digits=5))
#'
#' # Restrict number of values per line to 4.
#' cat(semproducible(data, formula="y ~ x", digits=5, vars_per_line=4))
#'
#' # Save code to a file.
#' code <- semproducible(data, formula="y ~ x")
#' save_code(code, "create_data.r")
#' }
semproducible <- function(x,
                          digits = NULL,
                          use = "complete.obs",
                          formula = "YOUR lavaan MODEL HERE",
                          target_variable = "cov_mat",
                          drop_non_numeric = FALSE,
                          vars_per_line = 9,
                          eval = FALSE,
                          print = FALSE,
                          template = NULL) {
  # Check inputs for errors.
  if ("matrix" %in% class(x)) {
    cor_mat <- x
  } else if (is.data.frame(x)) {
  } else {
    stop("x must be of type 'matrix' or 'data.frame'.")
  }

  # Check for non-numeric columns (doesn't apply to matrices).
  if (is.data.frame(x)) {
    # Check that all columns are numeric, throw error if not.
    non_numeric_cols <- NCOL(x) - sum(sapply(x, is.numeric))
    if (drop_non_numeric & non_numeric_cols != 0) {
      # Drop all non-numeric columns.
      x_raw <- x
      x <- x[sapply(x, is.numeric)]
      num_columns_dropped <- NCOL(x_raw) - NCOL(x)
      columns_dropped <- paste(names(setdiff(x_raw, x)), collapse=" ")
      message(paste("Dropped", num_columns_dropped, "non-numeric column(s):",
                    columns_dropped, sep=" "))
    } else if (non_numeric_cols != 0) {
      # Get name of non-numeric columns.
      non_numeric_colums <- paste(names(x[!sapply(x, is.numeric)]),
                                  collapse=" ")
      # Throw error.
      stop(paste("x contain ", non_numeric_cols, " non-numeric column(s).",
                 " Use 'drop_non_numeric = TRUE' to remove them",
                 " automatically, or remove them manually.\n",
                 "Columns: ", trimws(non_numeric_colums), sep=""),
           call. = FALSE)
    }
  }

  # Create covariance matrix if it's not already.
  if (is.data.frame(x)) {
    cor_mat <- cov(x, use=use)
  }

  # Get the number of observations in the data frame or matrix.
  num_observations <- NROW(x)

  # Indenting covariance values.
  #indent <- paste0(rep(" ", 14), collapse = "")
  indent <- " "

  # Generate variable names.
  var_names <- ""
  var_i <- 0
  for (variable in row.names(cor_mat)) {
    var_i <- var_i + 1
    var_names <- stringi::stri_c(var_names, "~", variable, ", ")
    if (var_i >= vars_per_line) {
      # Add line break after a specific number of variables.
      var_names <- stringi::stri_c(var_names, "\n")
      var_i <- 0
    }
  }

  # Generate values.
  values <- ""
  var_i <- 0
  for (i in seq.int(ncol(cor_mat))) {
    for (j in seq(ncol(cor_mat))) {
      var_i <- var_i + 1
      if (is.null(digits)) {
        value <- cor_mat[i, j]
      } else {
        value <- round(cor_mat[i, j], digits=digits)
      }
      values <- stringi::stri_c(values, value, ", ", sep="")
      if (var_i >= vars_per_line) {
        # Add line break after a specific number of variables.
        values <- stringi::stri_c(values, "\n")
        var_i <- 0
      }
    }
  }
  # Remove superfluous comma (,) at the end of values.
  values <- trimws(values, which = "right")
  values <- substr(values, 1, nchar(values) - 1)

  # Use default or custom code template.
  if (is.null(template)) {
    code <- code_template()
  } else {
    code <- template
  }

  # Replace placeholders with variable names and variable values.
  code <- gsub("%%OBSERVATIONS%%", num_observations, code)
  code <- gsub("%%FORMULA%%", formula, code)
  code <- gsub("%%TARGET%%", target_variable, code)
  code <- gsub("%%VARIABLES%%", var_names, code)
  code <- gsub("%%VALUES%%", values, code)

  # Run the code to check for errors?
  if (eval) {
    if (is.null(formula) || formula == "YOUR lavaan MODEL HERE") {
      stop("No lavaan formula specified, cannot evaluate lavaan model.",
           call. = FALSE)
    }
    if (is.null(code)) {
      stop("No code was generated, cannot evaluate code or lavaan model.",
           call. = FALSE)
    }
    tryCatch(base::eval(parse(text = code)), error = function(e) stop(e))
    message("Code and lavaan model evaluated successfully.")
  }

  # Print to screen or return.
  if (print) {
    cat(code, "\n")
  } else {
    return(code)
  }
}


#' Save code to a file
#'
#' Save generated code to a file. Does not overwrite existing files unless
#' \code{overwrite} is set to \code{TRUE}.
#'
#' @param code character string with R code.
#' @param filename character string with filename of the file to be saved.
#' @param overwrite whether or not to overwrite an existing file.
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
#' # If the file already exists, you will get an error.
#' # You need to explicitly overwrite existing files.
#' save_code(code, "create_data.r", overwrite = TRUE)
#' }
save_code <- function(code, filename, overwrite=FALSE) {
  if (file.exists(filename) & !overwrite) {
    stop(paste0("File '", filename, "' already exist.",
    " Use 'overwrite = TRUE' parameter to overwrite existing file."),
    call. = FALSE)
  }
  fileConn <- file(filename)
  writeLines(code, fileConn)
  close(fileConn)
}


#' Code template for R code
#'
#' Default code template with R code that is used when generating. For example,
#' variables in the template code are replaced with the actual values during
#' execution.
#'
#' The following variables are placeholders and replaced with actual values:
#'
#' \%\%FORMULA\%\% will be replaced with the lavaan formula.
#'
#' \%\%OBSERVATIONS\%\% will be replaced with the number of observations of the
#' actual data.
#'
#' \%\%TARGET\%\% will be replaced with the variable name of the covariance
#' matrix.
#'
#' \%\%VARIABLES\%\% will be replaced with the column names (variable names) of
#' the covariance matrix.
#'
#' \%\%VALUES\%\% will be replaced with the actual values of the covariance
#' matrix.
#'
#' @return a character string with the code template.
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
fit <- lavaan::sem(model = model,
                   sample.cov = %%TARGET%%,
                   sample.nobs = observations)

# Show results.
summary(fit)"
  return(code)
}
