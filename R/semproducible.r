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
#' @param method method for creating the matrix, either "cor" for correlations
#' or "cov" for covariances (default). This argument is ignored if "x"
#' is a matrix.
#' @param use optional character string giving a method for computing
#' covariances in the presence of missing values. This must be one of the
#' strings "everything", "all.obs", "complete.obs", "na.or.complete", or
#' "pairwise.complete.obs". This value is passed on to the cov() method.
#' @param target_variable target variable name for the generated covariance
#' matrix. Defaults to "cov_mat".
#' @param formula optional argument that specifies the lavaan formula syntax
#' that should be included in the code.
#' @param drop_non_numeric drop columns from the data frame that are not
#' numeric. This is useful if you have characters of factors as columns which
#' should not be included in the covariance matrix. Defaults to FALSE.
#' @param vars_per_line number of variables (or values) per line. Many
#' variables per line will increase the width of the code.
#' @param eval whether or not the generated code and lavaan model will be
#' executed. If eval is set to TRUE and the code fails, you will get an error
#' message. If the code successfully run without errors, the generated code
#' will simply be returned without any message.
#' @param print whether or not to print the code to the screen or return the
#' code as a character string (default). Printing to screen is useful for
#' debugging.
#' @param template a character string with a custom code template that
#' is used when generating the R code. See the code_template() function for
#' instructions on how to write your own template.
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
#' # Generate the same code, but restrict number of values on one line.
#' cat(semproducible(data, formula="y ~ x", digits=5, vars_per_line=4))
#'
#' # Save R code to file.
#' code <- semproducible(data, formula="y ~ x")
#' save_code(code, "create_data.r")
#' }
semproducible <- function(x,
                          digits = NULL,
                          method = "cov",
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
  } else if ("data.frame" %in% class(x)) {
  } else {
    stop("x must be of type 'matrix' or 'data.frame'.")
  }

  # Check for non-numeric columns.
  if ("data.frame" %in% class(x) & method == "cov") {
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

  # Create correlation matrix or covariance matrix.
  if ("data.frame" %in% class(x)) {
    if (method == "cor") {
      cor_mat <- cor(x, use=use)
    } else if (method == "cov") {
      cor_mat <- cov(x, use=use)
    } else {
      stop("Method must be 'cor' or 'cov'.")
    }
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

  # Code template.
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
    # Run the generated code and stop if any errors are thrown.
    tryCatch(base::eval(parse(text = code)), error = function(e) stop(e))
    message("Code and lavaan model evaluated successfully.")
  }

  if (print) {
    # Only print the code to standard output (screen).
    cat(code, "\n")
  } else {
    # Only return the code.
    return(code)
  }
}


#' Save code to a file
#'
#' @param code R code to save (character).
#' @param filename filename of the file (character).
#'
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


#' Code template that is used by semproducible to generate R code
#'
#' Use this template as a start for creating a custom template for your needs.
#'
#' There are several variables in the template that is replaced with the
#' actual values. For instance:
#'
#' %%FORMULA%% will be replaced with the lavaan formula.
#'
#' %%OBSERVATIONS%% will be replaced with the number of observations of the
#' actual data.
#'
#' %%TARGET%% will be replaced with the variable name of the covariance matrix.
#'
#' %%VARIABLES%% will be replaced with the column names (variable names) of the
#' covariance matrix.
#'
#' %%VALUES%% will be replaced with the actual values of the covariance matrix.
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
