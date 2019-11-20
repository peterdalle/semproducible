#' Code generator for reproducible SEM models
#'
#' Generate R code from your data frame, an existing covariance matrix, or
#' a lavaan object. The R code reproduces your structural equation model (SEM)
#' with minimal effort.
#'
#' Semproducible is useful when you need to create a reproducible covariance
#' matrix that can be used by a structural equation model (SEM) in
#' \code{\link[lavaan:sem]{lavaan}}.
#'
#' You supply a data frame with numeric variables (or a covariance matrix that
#' you have already prepared). The function then generates R code that
#' can reproduce the SEM model as a covariance matrix.
#'
#' You can also directly supply a lavaan SEM model, and semproducible will use
#' the fitted (observed) covariance matrix and produce R code that reproduce the
#' model, using the estimator of your lavaan model. Note, however, that this
#' will not give you all the possible models that you could have run with all
#' the data, but only uses the variables that are passed to lavaan.
#'
#' @param x a data frame with numeric variables, a covariance matrix, or a
#' lavaan object.
#' @param digits number of decimal digits of the covariance matrix. The
#' default (\code{NULL}) will show all available digits as specified by R
#' options. The higher the number of decimal digits, the more accurate the
#' reproducible model will be.
#' @param use character string for computing the covariances in the presence
#' of missing values. This must be "everything", "all.obs", "complete.obs",
#' "na.or.complete", or "pairwise.complete.obs". This value is passed on to
#' the \code{use} parameter of the \code{\link[stats:cor]{cov}} function
#' @param target_variable character string with arbitrary target variable name
#' for the generated covariance matrix. Defaults to \code{cov_mat}.
#' @param formula character string with a custom lavaan formula syntax
#' (e.g. \code{y ~ x}) that should be included in the code. If formula is
#' \code{NULL}, and \code{x} is a lavaan object, then a formula is created from
#' the lavaan object.
#' @param drop_non_numeric whether or not non-numeric columns should be dropped
#' from the data frame. This is useful if you have characters of factors as
#' columns which should not be included in the covariance matrix. Defaults to
#' \code{FALSE}.
#' @param vars_per_line number of variables (or values) per line. Many
#' variables per line will increase the width of the code.
#' @param eval whether or not the generated code and lavaan model will be
#' executed. If eval is set to \code{TRUE}, a message will tell you whether the
#' code executed withour errors or not.
#' @param print whether or not to print the code to the screen or return the
#' code as a character string (default). Printing to screen is useful during
#' development.
#' @param template a character string with a custom code template that
#' is used when generating the R code. See
#' \code{\link{code_template}} for instructions on how to write
#' your own template. A NULL value (default) will use the default template
#' from \code{\link{code_template}}.
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
#'
#' # Reproduce the entire lavaan object.
#' # Example from http://lavaan.ugent.be/tutorial/cfa.html
#' library(lavaan)
#' HS.model <- ' visual  =~ x1 + x2 + x3
#'               textual =~ x4 + x5 + x6
#'               speed   =~ x7 + x8 + x9 '
#' fit <- cfa(HS.model, data=HolzingerSwineford1939)
#' semproducible(fit)
#' }
semproducible <- function(x,
                          digits = NULL,
                          use = "complete.obs",
                          formula = NULL,
                          target_variable = "cov_mat",
                          drop_non_numeric = FALSE,
                          vars_per_line = 9,
                          eval = FALSE,
                          print = FALSE,
                          template = NULL) {
  # Check inputs for errors.
  if ("matrix" %in% class(x)) {
    input_type <- "matrix"
    cov_mat <- x
  } else if ("lavaan" %in% class(x)) {
    input_type <- "lavaan"
  } else if (is.data.frame(x)) {
    input_type <- "data.frame"
  } else {
    stop("x must be of type 'matrix', 'data.frame' or 'lavaan'.")
  }

  # Check for non-numeric columns (doesn't apply to matrices).
  if (input_type == "data.frame") {
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
  if (input_type == "data.frame") {
    cov_mat <- stats::cov(x, use=use)
  }

  # Default lavaan call.
  lavaan_call <- paste("lavaan::sem(model = model,",
                       "sample.cov = %%TARGET%%,",
                       "sample.nobs = observations)")

  # Get the number of observations in the data frame or matrix.
  num_observations <- NROW(x)

  # Extract lavaan objects.
  if (input_type == "lavaan") {
    cov_mat <- lavaan::lavInspect(x, what = "sampstat")
    if (class(cov_mat) == "list") {
      cov_mat <- cov_mat$cov
      num_observations <- lavaan::nobs(x)
      # Get laavan call, replace name of data variable.
      lavaan_call <- x@call
      lavaan_call$data <- NULL
      lavaan_call$sample.nobs <- "%NOBS%"
      lavaan_call$sample.cov <- "%COV%"
      lavaan_call <- paste0(utils::capture.output(lavaan_call), collapse="\n")
      lavaan_call <- gsub('"%NOBS%"', "observations", lavaan_call)
      lavaan_call <- gsub('"%COV%"', target_variable, lavaan_call)
    }

    # Extract lavaan parameter estimates and generate formula.
    if (is.null(formula)) {
      est <- lavaan::parameterEstimates(x)
      formula <- paste(stringi::stri_c(est$lhs, " ", est$op, " ", est$rhs),
                       collapse="\n")
    }
  }

  # Indenting covariance values.
  #indent <- paste0(rep(" ", 14), collapse = "")
  indent <- " "

  # Generate variable names.
  var_names <- ""
  var_i <- 0
  for (variable in row.names(cov_mat)) {
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
  for (i in seq.int(ncol(cov_mat))) {
    for (j in seq(ncol(cov_mat))) {
      var_i <- var_i + 1
      if (is.null(digits)) {
        value <- cov_mat[i, j]
      } else {
        value <- round(cov_mat[i, j], digits=digits)
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
  code <- gsub("%%LAVAAN_CALL%%", lavaan_call, code)
  code <- gsub("%%OBSERVATIONS%%", num_observations, code)
  code <- gsub("%%FORMULA%%", formula, code)
  code <- gsub("%%TARGET%%", target_variable, code)
  code <- gsub("%%VARIABLES%%", var_names, code)
  code <- gsub("%%VALUES%%", values, code)

  # Run the code to check for errors?
  if (eval) {
    if (is.null(formula)) {
      stop("No lavaan formula specified, cannot evaluate lavaan model.",
           call. = FALSE)
    }
    if (is.null(code)) {
      stop("No code was generated, cannot evaluate code or lavaan model.",
           call. = FALSE)
    }
    tryCatch(utils::capture.output(base::eval(parse(text = code))),
             error = function(e) stop(e))
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
#' Save generated R code to a file.
#'
#' Does not overwrite existing files unless
#' \code{overwrite} is set to \code{TRUE}.
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
#' # If the file already exists, you will get an error.
#' # You need to explicitly overwrite existing files.
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


#' Code template for R code
#'
#' Default R code template used by \code{\link{semproducible}} when generating
#' the code.
#'
#' For example, variables in the template code are replaced with the actual
#' values during execution.
#'
#' The following variables are placeholders and will be replaced with the
#' actual values:
#'
#' \code{\%\%FORMULA\%\%} = lavaan formula.
#'
#' \code{\%\%OBSERVATIONS\%\%} = number of observations of the actual data.
#'
#' \code{\%\%TARGET\%\%} = variable name of the covariance matrix.
#'
#' \code{\%\%VARIABLES\%\%} = column names (variable names) of
#' the covariance matrix.
#'
#' \code{\%\%VALUES\%\%} = actual values of the covariance matrix.
#'
#' \code{\%\%LAVAAN_CALL\%\%} = lavaan function call.
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
fit <- %%LAVAAN_CALL%%

# Show results.
summary(fit)"
  return(code)
}

