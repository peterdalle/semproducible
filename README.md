# semproducible - code generator for reproducible SEM models

**semproducible** is an R package that can easily make your [lavaan](http://lavaan.ugent.be/) models reproducible by generating all the necessary data and R code, with just one line of code.

The easiest way to reproduce your SEM model is to pass your lavaan object as an argument to semproducible: 

```r
semproducible(fit)
```

That's it!

![](demo.gif)

# Benefits of semproducible

- only one line of code is needed to reproduce your model
- you don't need to make your raw data publicly accessible
- the generated code can fit in a journal article or appendix
- other researchers can try alternative model specifications

In short, semproducible offers a good balance between open science, reproducibility, and integrity.

# Install

```r
devtools::install_github("peterdalle/semproducible")
```

# How it works

You can use semproducible in two ways.

The first way is to reproduce an existing model:

1. Pass your `lavaan` model to semproducible.
2. Semproducible extracts the fitted (observed) covariance matrix from your model and the formula syntax.
3. Semproducible generates all R code for both the covariance matrix and code necessary to run the model.

The second way is to reproduce all possible models in order to explore [researcher degrees of freedom](https://en.wikipedia.org/wiki/Researcher_degrees_of_freedom) or perform a multiverse or [sensitivity analysis](https://en.wikipedia.org/wiki/Sensitivity_analysis):

1. Give semproducible a data frame.
2. Semproducible creates a covariance matrix of your data frame.
3. Semproducible generates R code for both the covariance matrix and code necessary to run the SEM model using `lavaan`.

# Examples

## 1. Reproduce existing model

```r
library(lavaan)
library(semproducible)

# Example model from http://lavaan.ugent.be/tutorial/sem.html
model <- "# latent variables
            ind60 =~ x1 + x2 + x3
            dem60 =~ y1 + y2 + y3 + y4
            dem65 =~ y5 + y6 + y7 + y8
          # regressions
            dem60 ~ ind60
            dem65 ~ ind60 + dem60
          # residual covariances
            y1 ~~ y5
            y2 ~~ y4 + y6
            y3 ~~ y7
            y4 ~~ y8
            y6 ~~ y8"

# Fit SEM model.
fit <- sem(model, data = PoliticalDemocracy)

# Generate code to reproduce model.
code <- semproducible(fit, formula = model)

# Show the generated code.
cat(code)
```

## 2. Reproduce all possible models

Reproduce all possible SEM models with variables that was *not* included in the model:

```r
library(semproducible)

# We can only use numeric columns, so we exclude character columns.
df <- iris[, 1:4]

# Use the data frame with iris data and specify a lavaan model.
code <- semproducible(df, formula = "Sepal.Length ~ Sepal.Width + Petal.Length")

# Show the generated code.
cat(code)
```

# Look at the generated code

If you run example 2 above, the generated code will look something like this:

```r
library(tibble)
library(lavaan)

# Number of observations.
observations <- 150

# Covariance matrix.
cov_mat <- tribble(~Sepal.Length, ~Sepal.Width, ~Petal.Length, ~Petal.Width,
              0.685693512304251, -0.0424340044742729, 1.27431543624161, 0.516270693512304,               
              -0.0424340044742729, 0.189979418344519, -0.329656375838926, -0.12163937360179,               
              1.27431543624161, -0.329656375838926, 3.11627785234899, 1.29560939597315,               
              0.516270693512304, -0.12163937360179, 1.29560939597315, 0.581006263982103)

# Convert data frame to matrix (that lavaan can handle).
cov_mat <- as.matrix(cov_mat)

# Rows should have names too.
rownames(cov_mat) <- colnames(cov_mat)

# SEM model in lavaan syntax.
model <- 'Sepal.Length ~ Sepal.Width + Petal.Length'

# Fit SEM model.
fit <- lavaan::sem(model, sample.cov = cov_mat, sample.nobs = observations)

# Show results.
summary(fit)
```

Let's run the generated code above. The output should look similar to this:

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

The code seems to work, but how does it compare to the original dataset? Let's run a model on the original data and compare its output:

```r
fit_original <- lavaan::sem("Sepal.Length ~ Sepal.Width + Petal.Length", iris[, 1:4])

summary(fit_original)
```

As you can see (if you run the code), the output is identical to the previous one, and we have successfully reproduced the model.

# Questions

- [How do I save my code to a file?](https://github.com/peterdalle/semproducible/wiki#how-do-i-save-my-code-to-a-file)
- [Do I need to specify all columns?](https://github.com/peterdalle/semproducible/wiki#do-i-need-to-specify-all-columns)
- [How do I control the width of the code?](https://github.com/peterdalle/semproducible/wiki#how-do-i-control-the-width-of-the-code)
- [Can I use semproducible in a `tidyverse` pipeline?](https://github.com/peterdalle/semproducible/wiki#can-i-use-semproducible-in-a-tidyverse-pipeline)
- [How do I know that my generated code is correct?](https://github.com/peterdalle/semproducible/wiki#how-do-i-know-that-my-generated-code-is-correct)

# Documentation

Load semproducible and then run `?semproducible` in the R console to view the documentation.

# Support

Report problems or request a new feature by [submitting a new issue](https://github.com/peterdalle/semproducible/issues/new).

# Contribute

You can help with:

- Test semproducible in your project and report any bugs.
- Read the documentation and verify that it corresponds to actual behavior.

# License

[MIT](LICENSE)
