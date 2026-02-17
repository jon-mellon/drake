\name{rakeTable}
\alias{rakeTable}
\title{Rake a contingency table to target margins}
\usage{
rakeTable(tab, cs, rs, iterations = 100)
}
\description{
Iterative proportional fitting for a two-way table. Column and row margins are
scaled alternately until the requested margins are reached (approximately).
}
\arguments{
\item{tab}{Numeric matrix or table to be raked.}
\item{cs}{Target column sums (numeric vector, length \code{ncol(tab)}).}
\item{rs}{Target row sums (numeric vector, length \code{nrow(tab)}).}
\item{iterations}{Number of raking iterations to perform.}
}
\value{
A numeric matrix with adjusted cell values and the requested margins (up to
numerical tolerance).
}
\details{
Each iteration rescales columns to match \code{cs} and then rows to match
\code{rs}. If margins are incompatible with the table structure (e.g., zeros
in impossible cells), convergence may be slow or incomplete.
}
\examples{
mat <- matrix(c(10, 20, 30, 40), nrow = 2, byrow = TRUE)
col.targets <- c(50, 50)
row.targets <- c(40, 60)

raked <- rakeTable(mat, col.targets, row.targets, iterations = 50)
rowSums(raked)
colSums(raked)
}
