\name{checkContinuous}
\alias{checkContinuous}
\title{Check convergence for continuous targets}
\usage{
checkContinuous(sample, var, con.target, weights, debug = FALSE)
}
\description{
Computes the maximum absolute difference between a weighted sample density
and a target density (or stratified target densities).
}
\arguments{
\item{sample}{Data frame containing the variable and weights.}
\item{var}{Name of the continuous variable to evaluate.}
\item{con.target}{A \code{density} object or a nested list of densities
indexed by a stratifying variable.}
\item{weights}{Column name (string) containing weights in \code{sample}.}
\item{debug}{If \code{TRUE}, enter the debugger.}
}
\value{
Numeric scalar giving the maximum absolute density difference. For stratified
targets, the maximum across strata is returned.
}
\details{
The function normalizes weights to sum to one, computes a kernel density
estimate on the target grid, and returns the total absolute difference.
}
\examples{
set.seed(1)

sample <- data.frame(x = rnorm(200), w = runif(200))
con.target <- density(rnorm(1000))

checkContinuous(sample, "x", con.target, "w")
}
