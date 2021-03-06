% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pooled_logistic.R
\name{POplugin}
\alias{POplugin}
\title{Fits a proportional odds model via pooled logistic regression.}
\usage{
POplugin(form, data, weights = 1)
}
\arguments{
\item{form}{The model formula}

\item{data}{The data set used to fit the model}

\item{weights}{Either equal to 1 (no weights) or a vector of length equal to nrow(data)}
}
\value{
A list with the fitted glm, the original data, levels of the outcome, and the outcome name
}
\description{
The outcome in \code{data} (indicated in the \code{form} object) should be an ordered factor.
}
