\name{calcAgeDensity2}
\alias{calcAgeDensity2}
\title{Age density target from single-year population counts}
\usage{
calcAgeDensity2(
  country,
  date,
  bw.mult,
  min.age = 18,
  max.age = 80,
  pops = NULL,
  survey.n = 3000
)
}
\description{
Constructs a kernel density target for age using single-year population counts
for a given country and date.
}
\arguments{
\item{country}{ISO2 country code ("NIR" is mapped to "GB").}
\item{date}{Date used to select the population year.}
\item{bw.mult}{Bandwidth multiplier.}
\item{min.age}{Minimum age to include.}
\item{max.age}{Maximum age; ages above are capped to this value.}
\item{pops}{Population data frame. If \code{NULL}, data are fetched via
\code{popBySingleYear(country = country)}.}
\item{survey.n}{Synthetic sample size for the target distribution.}
}
\value{
A \code{density} object representing the target age distribution.
}
\details{
Population counts are converted into a synthetic age vector (length
\code{survey.n}) which is then smoothed with a kernel density estimate. The
resulting density is suitable for use as a continuous raking target.
}
\examples{
\dontrun{
# Requires population data from popBySingleYear()
ages <- calcAgeDensity2("GB", as.Date("2020-06-01"), bw.mult = 1.5)
}
