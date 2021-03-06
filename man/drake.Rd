% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/drake.R
\name{drake}
\alias{drake}
\title{Drake main function}
\usage{
drake(
  sample,
  continuous.targets,
  discrete.targets,
  mean.targets = NULL,
  max.weights = 25,
  min.weights = 1/max.weights,
  maxit = 1000,
  initial.weights = rep(1, nrow(sample)),
  max.discrete.diff = 5e-04,
  max.mean.diff = 0.001,
  max.con.diff = 0.01,
  subset = rep(T, nrow(sample)),
  debug = F,
  cap.every.var = F
)
}
\description{
Drake main function
}
