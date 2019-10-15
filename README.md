# semproducible - code generator for reproducible SEM models

**semproducible** is an R package that can easily make your SEM models reproducible by generating all the necessary data and R code, with just one line of code.

## Benefits of semproducible

- only one line of code is needed to get started
- you can create reproducible models without making your raw data publicly accessible
- the generated code can fit on a page in a journal article
- other researchers can run your model and try out alternative model specifications

In short, semproducible offers a good balance between open science, reproducibility, and integrity.

# Install

```r
devtools::install_github("peterdalle/semproducible")
```

# How it works

1. Give semproducible a data frame.
2. Semproducible creates a covariance matrix of your data frame.
3. Semproducible generates R code for both the covariance matrix and code necessary to run the SEM model using `lavaan`.

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

As you can see if you run the code, the output is identical to the previous one, and we have successfully reproduced the model.

# Questions

## Why not get the covariance matrix directly from lavaan?

You can get the observed sample statistics using the [`lavInspect()`](https://rdrr.io/cran/lavaan/man/lavInspect.html) method, like so:

```r
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

The column `Species` is a factor, which means that you will get an error if used with semproducible.

However, use the `drop_non_numeric = TRUE` to automatically drop columns that is not numeric:

```r
code <- semproducible(iris, drop_non_numeric = TRUE)
```

The column `Species` is now dropped, which a message informs you of:

```
Dropped 1 non-numeric column(s): Species 
```

## How do I control the width of the code?

If you have a large data frame you want to fit into the appendix of a journal article, you can control the number of values per line of the generated code with `vars_per_line = 2` for two variables/values per line.

You can also set `digits = 4` to round the number of decimals to four (default behavior is as many decimals as your current R session uses).

```r
semproducible(iris, drop_non_numeric = TRUE, vars_per_line = 2, digits = 4)
```

Output:

```
[...]

# Covariance matrix.
cov_mat <- tribble(~Sepal.Length, ~Sepal.Width, 
~Petal.Length, ~Petal.Width, 

0.6857, -0.0424, 
1.2743, 0.5163, 
-0.0424, 0.19, 
-0.3297, -0.1216, 
1.2743, -0.3297, 
3.1163, 1.2956, 
0.5163, -0.1216, 
1.2956, 0.581)

[...]
```

## Can I use semproducible in a `tidyverse` pipeline?

Yes, semproducible can be used together with pipes (the `%>%` operator):

```r
library(tidyverse)

iris %>%  
    select(Sepal.Length, Sepal.Width, Petal.Length) %>% 
    semproducible(formula = "Sepal.Length ~ Sepal.Width + Petal.Length") %>%
    cat()
```

## How do I know that my generated code is correct?

Add the `eval = TRUE` argument and the generated code will automatically run and evaluate. 

```r
code <- semproducible(iris[, 1:4], formula = "Sepal.Length ~ Sepal.Width + Petal.Length", eval = TRUE)
```

If the code fails, you will get an error message.

If the code runs successfully, semproducible will simply inform you of so and return your code (note: semproducible does not evaluate model fit or model convergence).