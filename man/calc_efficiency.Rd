\name{calc_efficiency}
\alias{calc_efficiency}
\title{Calculate weight efficiency}
\usage{
calc_efficiency(x, base = 1)
}
\description{
Computes a weight efficiency statistic (similar to Kish efficiency) for a
vector of survey weights.
}
\arguments{
\item{x}{Numeric vector of weights.}
\item{base}{Baseline weight (default 1).}
}
\value{
A numeric scalar giving efficiency as a percentage.
}
\details{
The calculation follows the formulation used in the \pkg{numerator} package
and reports efficiency as 100 * (index^2) / (index.sq * sum(base)).
}
\examples{
calc_efficiency(c(1, 1, 1, 1))
calc_efficiency(c(0.5, 1, 2, 1))
}
