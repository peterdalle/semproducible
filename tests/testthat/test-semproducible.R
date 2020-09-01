test_that("generating R code from original data works", {
  set.seed(5653)
  data <- data.frame(x = rnorm(100),
                     y = rnorm(100),
                     z = rnorm(100),
                     w = rnorm(100),
                     q = rnorm(100))

  # Generate R code for the data.
  code <- semproducible(data,
                        target_variable = "tmp_var",
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
                                    target_variable = "tmp_var_test",
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


test_that("covariance matrix as input works", {
  set.seed(5653)
  data <- data.frame(x = rnorm(100),
                     y = rnorm(100),
                     z = rnorm(100),
                     w = rnorm(100),
                     q = rnorm(100))

  # Same thing again, but with covariance matrix input instead.
  cov_mat <- cov(data)

  code <- semproducible(cov_mat, target_variable="tmp_var2",
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
                             target_variable="tmp_var3",
                             formula = "y ~ x + z + w"))

  # Should work: drop_non_numeric = TRUE.
  expect_message(code <- semproducible(data,
                        target_variable="tmp_var3",
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

  code <- semproducible(data, target_variable="tmp_var4",
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
  code <- semproducible(data, target_variable="tmp_var5",
                        formula = "y ~ x",
                        vars_per_line = 4,
                        digits = 3)

  # Use wrong variable names, should cause complaints by lavaan.
  expect_error(code <- semproducible(data, target_variable="tmp_var5",
                        formula = "y ~ VAR_DOES_NOT_EXIST",
                        vars_per_line = 4,
                        eval = TRUE))

})


test_that("using fitted lavaan object works", {
  data <- data.frame(x = rnorm(100), y = rnorm(100))

  fit <- lavaan::sem(model = "y ~ x", data = data)

  # This should work
  code <- semproducible(fit, target_variable="tmp_var6",
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
  code <- semproducible(fit, target_variable="tmp_var7",
                        formula = "y ~ x",
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

  # Compare 100 random covariance matrices.
  for (i in 1:100) {
    data <- data.frame(x = rnorm(1000),
                       y = rnorm(1000),
                       z = rnorm(1000),
                       w = rnorm(1000),
                       q = rnorm(1000))

    # Make lavaan model.
    formula = "y ~ x + z + w + q"
    lavaan_model <- lavaan::sem(formula, data)

    # Reproduce model with semproducible.
    code <- semproducible(data, target_variable = "tmp_var8",
                          formula = formula, digits = 10)
    eval(parse(text = code))

    # Compare lavaan + semproducible output.
    lavaan_cov <- lavInspect(lavaan_model, what="sampstat")$cov
    expect_equal(lavaan_cov["y", "y"], tmp_var8["y", "y"], tolerance=0.01)
    expect_equal(lavaan_cov["x", "y"], tmp_var8["x", "y"], tolerance=0.01)
    expect_equal(lavaan_cov["z", "y"], tmp_var8["z", "y"], tolerance=0.01)
    expect_equal(lavaan_cov["w", "y"], tmp_var8["w", "y"], tolerance=0.01)
    expect_equal(lavaan_cov["q", "y"], tmp_var8["q", "y"], tolerance=0.01)
    expect_equal(lavaan_cov["q", "q"], tmp_var8["q", "q"], tolerance=0.01)
  }
})
