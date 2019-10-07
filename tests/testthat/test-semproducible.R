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
                        early_line_break = TRUE)

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
                             formula = "y ~ x + z + w",
                             early_line_break = TRUE))

  # Should work: drop_non_numeric = TRUE.
  expect_warning(code <- semproducible(data,
                        target_variable="tmp_var3",
                        formula = "y ~ x + z + w",
                        early_line_break = TRUE,
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
                        early_line_break = TRUE)

  tmp_file <- tempfile(fileext = ".r")

  save_code(code, filename=tmp_file)

  # This should give a warning that file already exists.
  expect_error(save_code(code, filename=tmp_file))

  # This should work.
  save_code(code, filename=tmp_file, overwrite = TRUE)

  # Cleanup test file.
  file.remove(tmp_file)
})
