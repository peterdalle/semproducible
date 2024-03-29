% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/semproducible.R
\name{is_semproducible}
\alias{is_semproducible}
\title{Checks if object is a semproducible object}
\usage{
is_semproducible(x)
}
\arguments{
\item{x}{object to check.}
}
\value{
TRUE if the object is a semproducible object, otherwise FALSE.
}
\description{
Checks if object is a semproducible object
}
\examples{
library(semproducible)

code <- semproducible(iris, drop_non_numeric=TRUE)

is_semproducible(code)
}
