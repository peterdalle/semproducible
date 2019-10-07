# semproducible - code generator for reproducible SEM models

**semproducible** is an R package that can easily make your SEM models reproducible by generating all the necessary data and R code, with just one line of code.

Open data is great, but sometimes you cannot make raw data publicly accessible for legal or ethical reasons. This is where semproducible comes in.

Give semproducible a data frame with your data and it will convert your data to a covariance matrix and generate R code that is necessary to run your SEM model using the popular R package `lavaan`. Just copy and paste the generated code into your manuscript and you have a SEM model that others can reproduce and play around with in seconds.

Benefits of semproducible:

- only one line of code is needed to get started
- you can create reproducible models without making your raw data publicly accessible
- the generated code can fit on one page in a journal article
- other researchers run your model and try out alternative model specifications

In short, semproducible offers a good balance between open science, reproducibility, and integrity.

# Install

```r
devtools::install_github("peterdalle/semproducible")
```

# How it works

1. Give semproducible a data frame.
2. Semproducible creates a covariance matrix of your data frame.
3. Semproducible generates code for both the covariance matrix and code to run the SEM model using the `lavaan` package.

# Example

Let's use a toy example for demonstration purposes.

```r
library(semproducible)

# We can only use numeric columns, so we exclude character columns.
df <- iris[, 1:4]

# Use the data frame with iris data and specify a lavaan model.
code <- semproducible(df, formula = "Sepal.Length ~ Sepal.Width + Petal.Length")

# Show the code that semproducible generated.
cat(code)
```

Generated code output:

```r
library(tibble)
library(lavaan)

# Number of observations.
num_observations <- 150

# Covariance matrix.
data <- tribble(~Sepal.Length, ~Sepal.Width, ~Petal.Length, ~Petal.Width,
              0.685693512304251, -0.0424340044742729, 1.27431543624161, 0.516270693512304,               
              -0.0424340044742729, 0.189979418344519, -0.329656375838926, -0.12163937360179,               
              1.27431543624161, -0.329656375838926, 3.11627785234899, 1.29560939597315,               
              0.516270693512304, -0.12163937360179, 1.29560939597315, 0.581006263982103)

# Convert data frame to matrix (that lavaan can handle).
data <- as.matrix(data)

# Rows should have a name too.
rownames(data) <- colnames(data)

# SEM model in lavaan syntax.
my_model <- 'Sepal.Length ~ Sepal.Width + Petal.Length'

# Fit SEM model.
fit <- lavaan::sem(my_model,
                   sample.cov = data,
                   sample.nobs = num_observations)

# Show results.
summary(fit)
```

Now, let's run the generated code above.

Output when using the covariance matrix in lavaan:

```
lavaan 0.6-5 ended normally after 19 iterations

  Estimator                                         ML
  Optimization method                           NLMINB
  Number of free parameters                          3
                                                      
  Number of observations                           150
                                                      
Model Test User Model:
                                                      
  Test statistic                                 0.000
  Degrees of freedom                                 0

Parameter Estimates:

  Information                                 Expected
  Information saturated (h1) model          Structured
  Standard errors                             Standard

Regressions:
                   Estimate  Std.Err  z-value  P(>|z|)
  Sepal.Length ~                                      
    Sepal.Width       0.596    0.069    8.677    0.000
    Petal.Length      0.472    0.017   27.849    0.000

Variances:
                   Estimate  Std.Err  z-value  P(>|z|)
   .Sepal.Length      0.109    0.013    8.660    0.000
```

The code seems to work, but how does it compare to the original dataset?

Let's run a model on the original data and compare its output:

```r
fit_original <- lavaan::sem("Sepal.Length ~ Sepal.Width + Petal.Length", iris[, 1:4])

summary(fit_original)
```

Output using original data in lavaan:

```
lavaan 0.6-5 ended normally after 19 iterations

  Estimator                                         ML
  Optimization method                           NLMINB
  Number of free parameters                          3
                                                      
  Number of observations                           150
                                                      
Model Test User Model:
                                                      
  Test statistic                                 0.000
  Degrees of freedom                                 0

Parameter Estimates:

  Information                                 Expected
  Information saturated (h1) model          Structured
  Standard errors                             Standard

Regressions:
                   Estimate  Std.Err  z-value  P(>|z|)
  Sepal.Length ~                                      
    Sepal.Width       0.596    0.069    8.677    0.000
    Petal.Length      0.472    0.017   27.849    0.000

Variances:
                   Estimate  Std.Err  z-value  P(>|z|)
   .Sepal.Length      0.109    0.013    8.660    0.000
```

As you can see, the two outputs are identical, and we have successfully reproduced the model.

# Questions

## Why not get the covariance matrix directly from lavaan?

You can get the observed sample statistics using the [`lavInspect()`](https://rdrr.io/cran/lavaan/man/lavInspect.html) method, like so:

```r
library(lavaan)
fit <- lavaan::sem("Sepal.Length ~ Sepal.Width + Petal.Length", iris[, 1:4])
lavInspect(fit, what = "sampstat")
```

Which will output the following:

```
$cov
             Spl.Ln Spl.Wd Ptl.Ln
Sepal.Length  0.681              
Sepal.Width  -0.042  0.189       
Petal.Length  1.266 -0.327  3.096
```

However, `$cov` only gives you the covariance matrix of the *fitted* model, not all possible models that you could have run with all the data.

## How do I save my code to a file?

Use the `save_code()` function:

```r
save_code(code, "my_file.r")
```

If `my_file.r` already exists, the function will not continue. You need to explicitly add the `overwrite = TRUE` parameter to overwrite existing files:

```r
save_code(code, "my_file.r", overwrite = TRUE)
```

## Do I need to specify all columns?

The default behavior is that semproducible requires you to specify which columns that will be used in the covariance matrix.

Look at the `iris` dataset, for example:

```r
head(iris)
```

```
  Sepal.Length Sepal.Width Petal.Length Petal.Width Species
1          5.1         3.5          1.4         0.2  setosa
2          4.9         3.0          1.4         0.2  setosa
3          4.7         3.2          1.3         0.2  setosa
4          4.6         3.1          1.5         0.2  setosa
5          5.0         3.6          1.4         0.2  setosa
6          5.4         3.9          1.7         0.4  setosa
```

The column `Species` is a factor, which means that this will give you an error:

```r
code <- semproducible(iris)
```

But this will work:

```r
code <- semproducible(iris, drop_non_numeric = TRUE)
```

The column `Species` is now automatically dropped, and you get a warning message of what happened:

```
Warning message:
1 non-numeric column(s) dropped: Species
```