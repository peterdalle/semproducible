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
  expect_equal(class(tmp_var), "matrix")

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
    cat("Testing vars_per_line =", i, "\n")
    eval(parse(text = semproducible(data,
                                    target_variable = "tmp_var_test",
                                    formula = "y ~ x + z + w",
                                    vars_per_line = i)))

    # Variable should be a matrix if code successfully ran.
    expect_equal(class(tmp_var_test), "matrix")
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
  expect_equal(class(tmp_var2), "matrix")

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
  expect_equal(class(tmp_var3), "matrix")

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

  save_code(code, filename=tmp_file)

  # This should give a warning that file already exists.
  expect_error(save_code(code, filename=tmp_file))

  # This should work.
  save_code(code, filename=tmp_file, overwrite = TRUE)

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

  # This is a very hard (even unfair!) test because any change in the
  # length of the generated code will throw this exception.
  expect_equal(nchar(code), 533)

  # Use wrong variable names, should cause complaints by lavaan.
  expect_error(code <- semproducible(data, target_variable="tmp_var5",
                        formula = "y ~ VAR_DOES_NOT_EXIST",
                        vars_per_line = 4,
                        eval = TRUE))

})
