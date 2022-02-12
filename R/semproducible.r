#' Generate R code to reproduce a latent variable model (lavaan)
#'
#' Generate R code from your data frame, an existing covariance matrix, or
#' a \code{lavaan} object.
#'
#' The R code reproduces your structural equation model
#' (SEM) to support open science with minimal effort, and makes it easy to
#' supply your code to a journal article, or to share your model with others
#' without sharing your full dataset that may contain sensitive information.
#'
#' Semproducible is useful when you need to create a reproducible covariance
#' matrix that can be used by a structural equation model (SEM) in
#' \code{\link[lavaan:sem]{lavaan}}.
#'
#' You supply a data frame with numeric variables (or a covariance matrix that
#' you have already prepared). \code{semproducible} then generates R code that
#' can reproduce the SEM model as a covariance matrix.
#'
#' You can also directly supply a lavaan SEM model, and semproducible will use
#' the fitted (observed) covariance matrix and produce R code that reproduce the
#' model, using the estimator of your lavaan model. Note, however, that this
#' will not give you all the possible models that you could have ran with all
#' the data, but only the variables that are passed to lavaan.
#'
#' Save the code to a file using the
#' \code{\link[semproducible:save_code]{save_code}} function.
#'
#' @param x a \code{\link[base:data.frame]{data.frame}} with numeric variables,
#' a \code{\link[base:matrix]{matrix}} with covariances, or a fitted
#' \code{\link[lavaan:sem]{lavaan}} object that you want to reproduce.
#' @param formula character string with a custom lavaan formula syntax
#' (e.g. \code{y ~ x}) that should be included in the code. If formula is
#' \code{NULL}, and \code{x} is a lavaan object, then a formula is generated
#' automatically from the lavaan object.
#' @param covmat_variable character string with arbitrary variable name
#' for the generated covariance matrix. Defaults to \code{"cov_mat"}.
#' @param fit_variable character string with arbitrary target variable name
#' for the lavaan object. Defaults to \code{"fit"}.
#' @param digits number of decimal digits of the covariance matrix. The
#' default (\code{NULL}) will show all available digits as specified by your R
#' options. The higher the number of decimal digits, the more accurate the
#' reproducible model will be.
#' @param drop_non_numeric whether non-numeric columns should be dropped
#' from the data frame. This is useful if you have characters or factors as
#' columns, which should0 be excluded from the covariance matrix. Defaults to
#' \code{FALSE}.
#' @param comments whether the generated code should include comments that
#' describe the code. Defaults to TRUE.
#' @param vars_per_line number of variables/values per line. Use a low
#' value to decrease the width of the generated code.
#' @param eval whether the generated code and \code{\link[lavaan:sem]{lavaan}}
#' model will be executed during generation. If \code{TRUE}, a message will
#' tell you whether the code executed with errors or not.
#' @param use character string for computing the covariances in the presence
#' of missing values. This value is simply passed on to the \code{use}
#' parameter of the \code{\link[stats:cor]{cov}} function. The value must be
#' \code{everything}, \code{all.obs}, \code{complete.obs},
#' \code{na.or.complete}, or \code{pairwise.complete.obs}.
#' @param template a character string with a custom code template that
#' is used when generating the R code. The default value is
#' \code{\link{code_template}} which contains code for lavaan.
#' @return a character string with the generated R code.
#' @export
#'
#' @examples
#'\dontrun{
#' library(semproducible)
#' library(lavaan)
#'
#' # Create random data
#' set.seed(5543)
#' data <- data.frame(x = rnorm(100),
#'                    y = rnorm(100),
#'                    z = rnorm(100),
#'                    w = rnorm(100),
#'                    q = rnorm(100))
#'
#' code <- semproducible(data, formula="y ~ x")
#'
#' code <- semproducible(data, formula="y ~ x", digits=5)
#'
#' code <- semproducible(data, formula="y ~ x", digits=5, vars_per_line=4)
#'
#' # View code
#' cat(code)
#'
#' save_code(code, "create_data.r")
#'
#' # Example: http://lavaan.ugent.be/tutorial/cfa.html
#' HS.model <- ' visual  =~ x1 + x2 + x3
#'               textual =~ x4 + x5 + x6
#'               speed   =~ x7 + x8 + x9 '
#' fit <- cfa(HS.model, data=HolzingerSwineford1939)
#' code <- semproducible(fit)
#'}
semproducible <- function(x, formula = NULL, covmat_variable = "cov_mat",
                          fit_variable = "fit",
                          digits = NULL, drop_non_numeric = FALSE,
                          comments = TRUE, vars_per_line = 9, eval = FALSE,
                          use = "complete.obs",
                          template = code_template()) {
  if (!(is.matrix(x) | is.data.frame(x) | inherits(x, "lavaan"))) {
    stop("x must be of type 'matrix', 'data.frame' or 'lavaan'.", call. = FALSE)
  }
  if (!(is.character(template) & length(template) == 1)) {
      stop("Argument 'template' must be a character string of length 1.",
           call. = FALSE)
  }
  if (is.null(covmat_variable) | covmat_variable == "") {
    stop("Argument 'covmat_variable' cannot be NULL or empty.", call. = FALSE)
  }
  params <- list()
  params$call <- get_default_lavaan_call()
  params$num_observations <- NROW(x)
  params$formula <- ifelse(is.null(formula), "", formula)
  if (is.data.frame(x)) {
    x <- check_non_numeric_columns_and_drop(x, drop_non_numeric)
    cov_mat <- stats::cov(x, use=use)
  } else if (is.matrix(x)) {
    cov_mat <- x
  } else if (inherits(x, "lavaan")) {
    params <- extract_lavaan_parameters(x, formula, covmat_variable)
    cov_mat <- params$cov_mat
  }
  if (!comments) {
    template <- strip_code_comments(template)
  }
  params$fit_variable <- fit_variable
  params$covmat_variable <- covmat_variable
  params$var_names <- generate_variable_names(cov_mat, vars_per_line)
  params$values <- generate_values(cov_mat, vars_per_line, digits)
  code <- replace_code_template_placeholders(template, params)
  run_and_eval_code(code, eval, params$formula)
  return(code)
}


check_non_numeric_columns_and_drop <- function(x, drop_non_numeric) {
  non_numeric_cols <- NCOL(x) - sum(sapply(x, is.numeric))
  if (drop_non_numeric & non_numeric_cols != 0) {
    x <- drop_non_numeric_columns(x)
  } else if (non_numeric_cols != 0) {
    non_numeric_colums <- paste(names(x[!sapply(x, is.numeric)]),
                                collapse=" ")
    stop(paste("x contain ", non_numeric_cols, " non-numeric column(s).",
               " Use 'drop_non_numeric = TRUE' to remove them",
               " automatically, or remove them manually.\n",
               "Columns: ", trimws(non_numeric_colums), sep=""),
         call. = FALSE)
  }
  return(x)
}


drop_non_numeric_columns <- function(x) {
  x_raw <- x
  x <- x[sapply(x, is.numeric)]
  num_columns_dropped <- NCOL(x_raw) - NCOL(x)
  columns_dropped <- paste(names(base::setdiff(x_raw, x)), collapse=" ")
  message(paste("Dropped", num_columns_dropped, "non-numeric column(s):",
                columns_dropped, sep=" "))
  return(x)
}


extract_lavaan_parameters <- function(x, formula, covmat_variable) {
  cov_mat <- lavaan::lavInspect(x, what = "sampstat")
  num_observations <- lavaan::nobs(x)
  #if (inherits(cov_mat, "list")) {
    cov_mat <- cov_mat$cov
    call <- x@call
    call$data <- NULL
    call$sample.nobs <- "%_NOBS%"
    call$sample.cov <- "%_COV%"
    call <- paste0(utils::capture.output(call), collapse="\n")
    call <- gsub('"%_NOBS%"', "observations", call)
    call <- gsub('"%_COV%"', covmat_variable, call)
  #}
  if (is.null(formula)) {
   # call <- get_default_lavaan_call()
    est <- lavaan::parameterEstimates(x)
    formula <- paste(stringi::stri_c(est$lhs, " ", est$op, " ", est$rhs),
                     collapse="\n")
  }
  return(list(call = call,
              formula = formula,
              cov_mat = cov_mat,
              num_observations = num_observations))
}


strip_code_comments <- function(code) {
  code <- gsub("#[^\\\n]*", "", code)
  code <- gsub("\n\n", "\n", code)
  code
}


generate_variable_names <- function(cov_mat, vars_per_line) {
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
  return(var_names)
}


generate_values <- function(cov_mat, vars_per_line, digits) {
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
  return(values)
}


run_and_eval_code <- function(code, eval, formula) {
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
}
