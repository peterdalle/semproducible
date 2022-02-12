test_that("generating R code from original data works", {
  set.seed(5653)
  data <- data.frame(x = rnorm(100),
                     y = rnorm(100),
                     z = rnorm(100),
                     w = rnorm(100),
                     q = rnorm(100))

  # Generate R code for the data.
  code <- semproducible(data,
                        covmat_variable = "tmp_var",
                        formula = "y ~ x + z + w")

  # Make sure the output type is correct.
  expect_equal(class(code), "character")

  # Run the generate code.
  eval(parse(text = code))

  # Make sure the data type is correct.
  expect_equal(class(tmp_var)[[1]], "matrix")

  # All values should be the same (digits will be different on different
  # systems so we need to use rounding to get the same number of digits).
  total_true <- sum(
    round(as.data.frame(cov(data)), 5) == round(as.data.frame(tmp_var), 5))

  # There should be 25 identical values.
  expect_equal(25, total_true)
})


test_that("different vars_per_line work", {
  set.seed(5653)
  data <- data.frame(x = rnorm(100),
                     y = rnorm(100),
                     z = rnorm(100),
                     w = rnorm(100),
                     q = rnorm(100))

  # Test different vars_per_line (from -1 to 30) and run the code.
  for (i in -1:30) {
    eval(parse(text = semproducible(data,
                                    covmat_variable = "tmp_var_test",
                                    formula = "y ~ x + z + w",
                                    vars_per_line = i)))

    # Variable should be a matrix if code successfully ran.
    expect_equal(class(tmp_var_test)[[1]], "matrix")
    expect_type(tmp_var_test, "double")

    # Variable should be lavaan object.
    expect_equal(class(fit)[1], "lavaan")

    # Clean up so we don't accidentally test old variables.
    rm(tmp_var_test)
    rm(fit)
  }
})


test_that("no comments in generated code works", {
  set.seed(5653)
  data <- data.frame(x = rnorm(100),
                     y = rnorm(100),
                     z = rnorm(100),
                     w = rnorm(100),
                     q = rnorm(100))

  code <- semproducible(data, covmat_variable="tmp_var_test", comments=TRUE,
                        formula = "y ~ x + z + w")
  eval(parse(text = code))
  expect_true(stringi::stri_detect_fixed(code, "#"))

  code <- semproducible(data, covmat_variable="tmp_var_test", comments=FALSE,
                        formula = "y ~ x + z + w")
  eval(parse(text = code))
  expect_false(stringi::stri_detect_fixed(code, "#"))

  expect_equal(class(fit)[1], "lavaan")

  rm(tmp_var_test)
  rm(fit)
})


test_that("covariance matrix as input works", {
  set.seed(5653)
  data <- data.frame(x = rnorm(100),
                     y = rnorm(100),
                     z = rnorm(100),
                     w = rnorm(100),
                     q = rnorm(100))

  # Same thing again, but with covariance matrix input instead.
  cov_mat <- cov(data)

  code <- semproducible(cov_mat, covmat_variable="tmp_var2",
                        formula = "y ~ x + z + w",
                        vars_per_line = 1)

  # Make sure the output type is correct.
  expect_equal(class(code), "character")

  # Run the generated code.
  eval(parse(text = code))

  # Make sure the data type is correct.
  expect_equal(class(tmp_var2)[[1]], "matrix")

  # All values should be the same (digits will be different on different
  # systems so we need to use rounding to get the same number of digits).
  total_true <- sum(
    round(as.data.frame(cov(data)), 5) == round(as.data.frame(tmp_var2), 5))

  # There should be 25 identical values.
  expect_equal(25, total_true)
})


test_that("non-numeric input parameters are handled correctly", {
  set.seed(5653)
  data <- data.frame(x = rnorm(100),
                     y = rnorm(100),
                     z = rnorm(100),
                     w = rnorm(100),
                     q = rnorm(100),
                     chr = rep("a", 100))

  # Should throw an error about non-numeric columns.
  expect_error(semproducible(data,
                             covmat_variable="tmp_var3",
                             formula = "y ~ x + z + w"))

  # Should work: drop_non_numeric = TRUE.
  expect_message(code <- semproducible(data,
                        covmat_variable="tmp_var3",
                        formula = "y ~ x + z + w",
                        drop_non_numeric = TRUE))

  # Make sure the output type is correct.
  expect_equal(class(code), "character")

  # Run the generated code.
  eval(parse(text = code))

  # Make sure the data type is correct.
  expect_equal(class(tmp_var3)[[1]], "matrix")

  # All values should be the same (digits will be different on different
  # systems so we need to use rounding to get the same number of digits).
  data_no_numeric <- data[, c("x", "y", "z", "w", "q")]
  total_true <- sum(
    round(as.data.frame(cov(data_no_numeric)), 5) == round(
      as.data.frame(tmp_var3), 5))

  # There should be 25 identical values.
  expect_equal(25, total_true)
})


test_that("saving code files work", {
  data <- data.frame(x = rnorm(100), y = rnorm(100))

  code <- semproducible(data, covmat_variable="tmp_var4",
                        formula = "y ~ x",
                        vars_per_line = 4)

  tmp_file <- tempfile(fileext = ".r")

  save_code(code, file = tmp_file)

  # This should give a warning that file already exists.
  expect_error(save_code(code, file = tmp_file))

  # This should work.
  save_code(code, file = tmp_file, overwrite = TRUE)

  # Cleanup test file.
  file.remove(tmp_file)
})


test_that("evaluating code works", {
  data <- data.frame(x = rnorm(100), y = rnorm(100))

  # This should work
  code <- semproducible(data, covmat_variable="tmp_var5",
                        formula = "y ~ x",
                        vars_per_line = 4,
                        digits = 3)

  # Use wrong variable names, should cause complaints by lavaan.
  expect_error(code <- semproducible(data, covmat_variable="tmp_var5",
                        formula = "y ~ VAR_DOES_NOT_EXIST",
                        vars_per_line = 4,
                        eval = TRUE))

})


test_that("using fitted lavaan object works", {
  data <- data.frame(x = rnorm(100), y = rnorm(100))

  fit <- lavaan::sem(model = "y ~ x", data = data)

  # This should work
  code <- semproducible(fit, covmat_variable="tmp_var6",
                        formula = "y ~ x",
                        vars_per_line = 3,
                        digits = 6)

  # Run the generated code.
  eval(parse(text = code))

  # Generated covariance matrix should be identical to original.
  expect_equal(tmp_var6[c("x", "y"), c("x", "y")],
               cov(data)[c("x", "y"), c("x", "y")], tolerance = 0.05)

})


test_that("more complex fitted lavaan object works", {
  formula <- "visual  =~ x1 + x2 + x3
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9"

  fit <- lavaan::sem(model = formula, data = lavaan::HolzingerSwineford1939)

  # This should work
  code <- semproducible(fit, covmat_variable="tmp_var7",
                        formula = formula,
                        vars_per_line = 3,
                        digits = NULL)

  # Run the generated code.
  eval(parse(text = code))

  # Generated covariance matrix should be identical to original.
  data <- lavaan::HolzingerSwineford1939[c("x1", "x2", "x3",
                                           "x4", "x5", "x6",
                                           "x7", "x8", "x9")]
  expect_equal(tmp_var7, cov(data), tolerance = 0.005)
})


test_that("compare lavaan covariance with semproducible covariances", {
  set.seed(8324)

  # Compare random covariance matrices.
  for (i in 1:10) {
    data <- data.frame(x = rnorm(1000),
                       y = rnorm(1000),
                       z = rnorm(1000),
                       w = rnorm(1000),
                       q = rnorm(1000))

    # Make lavaan model.
    formula = "y ~ x + z + w + q"
    lavaan_model <- lavaan::sem(formula, data)

    # Reproduce model with semproducible.
    code <- semproducible(data, covmat_variable = "tmp_var8",
                          formula = formula, digits = 10)
    eval(parse(text = code))

    # Compare lavaan + semproducible output.
    lavaan_cov <- lavaan::lavInspect(lavaan_model, what="sampstat")$cov
    expect_equal(lavaan_cov["y", "y"], tmp_var8["y", "y"], tolerance=0.01)
    expect_equal(lavaan_cov["x", "y"], tmp_var8["x", "y"], tolerance=0.01)
    expect_equal(lavaan_cov["z", "y"], tmp_var8["z", "y"], tolerance=0.01)
    expect_equal(lavaan_cov["w", "y"], tmp_var8["w", "y"], tolerance=0.01)
    expect_equal(lavaan_cov["q", "y"], tmp_var8["q", "y"], tolerance=0.01)
    expect_equal(lavaan_cov["q", "q"], tmp_var8["q", "q"], tolerance=0.01)
  }
})


test_that("growth model works with data as input", {
  set.seed(273)
  # Example from bottom of page: https://lavaan.ugent.be/tutorial/growth.html

  # a linear growth model with a time-varying covariate
  formula <- "
  # intercept and slope with fixed coefficients
    i =~ 1*t1 + 1*t2 + 1*t3 + 1*t4
    s =~ 0*t1 + 1*t2 + 2*t3 + 3*t4
  # regressions
    i ~ x1 + x2
    s ~ x1 + x2
  # time-varying covariates
    t1 ~ c1
    t2 ~ c2
    t3 ~ c3
    t4 ~ c4"
  lavaan_model <- lavaan::growth(formula, data=lavaan::Demo.growth)

  code <- semproducible(lavaan::Demo.growth, formula=formula, covmat_variable="tmp_var9")

  eval(parse(text = code))

  # Compare lavaan + semproducible output.
  lavaan_cov <- lavaan::lavInspect(lavaan_model, what="sampstat")$cov
  expect_equal(lavaan_cov["t1", "t2"], tmp_var9["t1", "t2"], tolerance=0.01)
  expect_equal(lavaan_cov["t3", "c3"], tmp_var9["t3", "c3"], tolerance=0.01)
})


test_that("growth model works with model as input", {
  set.seed(1234)
  # Example from bottom of page: https://lavaan.ugent.be/tutorial/growth.html

  # a linear growth model with a time-varying covariate
  formula <- "
  # intercept and slope with fixed coefficients
    i =~ 1*t1 + 1*t2 + 1*t3 + 1*t4
    s =~ 0*t1 + 1*t2 + 2*t3 + 3*t4
  # regressions
    i ~ x1 + x2
    s ~ x1 + x2
  # time-varying covariates
    t1 ~ c1
    t2 ~ c2
    t3 ~ c3
    t4 ~ c4"

  lavaan_model <- lavaan::growth(formula, data=lavaan::Demo.growth)

  code <- semproducible(lavaan_model, formula=formula, covmat_variable="tmp_var10")

  eval(parse(text = code))

  # Compare lavaan + semproducible output.
  lavaan_cov <- lavaan::lavInspect(lavaan_model, what="sampstat")$cov
  expect_equal(lavaan_cov["t1", "t2"], tmp_var10["t1", "t2"], tolerance=0.01)
  expect_equal(lavaan_cov["t3", "c3"], tmp_var10["t3", "c3"], tolerance=0.01)

  expect_equal(anova(fit)$Df[2], anova(lavaan_model)$Df[2], tolerance=0)
  expect_equal(anova(fit)$Pr[2], anova(lavaan_model)$Pr[2], tolerance=0.3) # here's some larger differences
})


test_that("example model #1 works", {
  # https://stats.stackexchange.com/questions/523678/lavaan-sem-through-time

  set.seed(1234)
  n <- 100

  ### T1
  X_t1 <- rnorm(n)
  x1_t1 <- X_t1 + rnorm(n)
  x2_t1 <- X_t1 + rnorm(n)
  x3_t1 <- X_t1 + rnorm(n)
  M_t1 <- 0.5*X_t1 + rnorm(n)
  Y_t1 <- 0.7*M_t1 + rnorm(n)
  DF1 <- data.frame(x1_t1, x2_t1, x3_t1, Y_t1, M_t1)

  ### T2
  X_t2 <- rnorm(n)
  x1_t2 <- X_t2 + rnorm(n)
  x2_t2 <- X_t2 + rnorm(n)
  x3_t2 <- X_t2 + rnorm(n)
  M_t2 <- 0.5*X_t2 + rnorm(n)
  Y_t2 <- 0.7*M_t2 + rnorm(n)
  DF2 <- data.frame(x1_t2, x2_t2, x3_t2, Y_t2, M_t2)

  # Dataframe
  DF <- cbind(DF1, DF2)

  model_free <- '
            #Time 1

            # latent variable
              X_t1 =~ x1_t1 + x2_t1 + x3_t1
            # direct effect
              Y_t1 ~ c*X_t1
            # mediator
              M_t1 ~ a*X_t1
              Y_t1 ~ b*M_t1
            # indirect effect (a*b)
              ab := a*b
            # total effect
              total_t1 := c + (a*b)

             # Time 2

            # latent variable
              X_t2 =~ x1_t2 + x2_t2 + x3_t2
            # direct effect
              Y_t2 ~ d*X_t2
            # mediator
              M_t2 ~ e*X_t2
              Y_t2 ~ f*M_t2
            # indirect effect (e*f)
              ef := e*f
            # total effect
              total_t2 := d + (e*f)'

  model_constrained <- '
            #Time 1

            # latent variable
              X_t1 =~ x1_t1 + x2_t1 + x3_t1
            # direct effect
              Y_t1 ~ c*X_t1
            # mediator
              M_t1 ~ a*X_t1
              Y_t1 ~ b*M_t1
            # indirect effect (a*b)
              ab := a*b
            # total effect
              total_t1 := c + (a*b)

            # Time 2

            # latent variable
              X_t2 =~ x1_t2 + x2_t2 + x3_t2
            # direct effect
              Y_t2 ~ c*X_t2
            # mediator
              M_t2 ~ a*X_t2
              Y_t2 ~ b*M_t2
            # indirect effect (a*b)
              ab := a*b
            # total effect
              total_t2 := c + (a*b)'

  # Original
  fit_free <- lavaan::sem(model_free, data = DF)
  fit_constrained <- lavaan::sem(model_constrained, data = DF)
  comparison1 <- lavaan::lavTestLRT(fit_free, fit_constrained)

  # Reproduce
  code_free <- semproducible(fit_free, model_free,
                             covmat_variable="covmat_free",
                             fit_variable="rep_free")
  code_constrained <- semproducible(fit_constrained, model_constrained,
                                    covmat_variable="covmat_constrained",
                                    fit_variable="rep_constrained")
  eval(parse(text = code_free))
  eval(parse(text = code_constrained))
  comparison2 <- lavaan::lavTestLRT(rep_free, rep_constrained)

  # Assert
  expect_equal(comparison1$Df, comparison2$Df)
  expect_equal(comparison1$Chisq, comparison2$Chisq)
})
