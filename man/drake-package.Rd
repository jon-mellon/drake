\docType{package}
\name{drake-package}
\alias{drake-package}
\title{drake: Density Raking for Surveys}
\description{
The \pkg{drake} package implements density raking to align survey samples to
population margins. It supports discrete targets (marginal proportions),
continuous targets (kernel density estimates), and mean targets, with optional
stratified continuous targets. The implementation emphasizes speed through
cached density indexing and a C++ root-finding routine for mean constraints.
}
\details{
Key components:
\itemize{
  \item \code{\link{drake}}: main raking routine.
  \item \code{\link{rakeTable}}: 2D raking for contingency tables.
  \item \code{\link{checkContinuous}}: convergence check for continuous targets.
  \item Mortality and population utilities: \code{\link{popBySingleYear}},
  \code{\link{indexMortality}}, \code{\link{calculateMortality2}},
  \code{\link{getExpectedMortality}}.
}

Implementation notes:
\itemize{
  \item Continuous targets use kernel density ratios to update weights.
  \item Discrete targets use iterative proportional fitting on weighted
    marginals.
  \item Mean targets use a monotone weight transformation solved by a C++
    root-finder (Brent-style) for speed and stability.
  \item Weights are re-scaled to sum to the number of valid rows after each
    full iteration, with optional caps to limit extreme weights.
}
}
\seealso{
\code{\link{drake}}, \code{\link{rakeTable}}, \code{\link{getAgeDensities}}
}
\author{
Jonathan Mellon
}
