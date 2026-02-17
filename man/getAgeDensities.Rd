\name{getAgeDensities}
\alias{getAgeDensities}
\title{Build stratified age density targets from a weighted sample}
\usage{
getAgeDensities(
  data,
  weight.var,
  var.suffix,
  target.suffix,
  continuous.var = "age",
  bw.multiple = 2,
  min.bw = 0,
  stratify.var = "turnout",
  stratify.vals = c("Voted", "DNV")
)
}
\description{
Constructs stratified kernel density targets for a continuous variable (by
default age) using a weighted sample and specified strata.
}
\arguments{
\item{data}{Data frame with the continuous variable, stratifying variable, and
weights.}
\item{weight.var}{Name of the weight column.}
\item{var.suffix}{Suffix appended to the continuous variable name.}
\item{target.suffix}{Suffix appended to the target variable name.}
\item{continuous.var}{Base name of the continuous variable.}
\item{bw.multiple}{Multiplier for the maximum stratum bandwidth.}
\item{min.bw}{Minimum allowable bandwidth.}
\item{stratify.var}{Base name of the stratifying variable.}
\item{stratify.vals}{Vector of stratum values to include.}
}
\value{
A nested list of density objects suitable for use as a stratified
\code{continuous.targets} entry in \code{drake()}.
}
\details{
The function computes density estimates within each stratum, chooses the
largest bandwidth, multiplies it by \code{bw.multiple}, and recomputes
stratum densities with the shared bandwidth for comparability.
}
\examples{
set.seed(1)

n <- 200
sample <- data.frame(
  age = rnorm(n, 45, 12),
  turnout = sample(c("Voted", "DNV"), n, replace = TRUE),
  w = runif(n)
)

getAgeDensities(sample, weight.var = "w", var.suffix = "", target.suffix = "")
}
