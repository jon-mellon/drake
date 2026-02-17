\name{ageDensityUNWPP}
\alias{ageDensityUNWPP}
\title{Age density target from UN WPP population data}
\usage{
ageDensityUNWPP(
  year,
  country,
  min.age,
  max.age,
  data.age.vec,
  pops,
  bw.mult = 2
)
}
\description{
Builds a kernel density target for age using UN WPP population counts and a
bandwidth derived from observed sample ages.
}
\arguments{
\item{year}{Reference year for the population data.}
\item{country}{ISO2 country code.}
\item{min.age}{Minimum age to include.}
\item{max.age}{Maximum age; ages above are capped to this value.}
\item{data.age.vec}{Vector of sample ages used to derive a bandwidth.}
\item{pops}{Population data frame (e.g., from \code{popBySingleYear()}).}
\item{bw.mult}{Multiplier for the sample-derived bandwidth.}
}
\value{
A \code{density} object representing the target age distribution.
}
\details{
The function uses the sample age distribution to derive a bandwidth and then
constructs a synthetic population age vector from UN WPP counts. The resulting
kernel density estimate is returned for use as a continuous target.
}
\examples{
\dontrun{
# Requires population data from popBySingleYear()
ages <- sample(18:80, 1000, replace = TRUE)

pops <- popBySingleYear(source = "wpp", country = "GB")

target <- ageDensityUNWPP(
  year = 2020,
  country = "GB",
  min.age = 18,
  max.age = 80,
  data.age.vec = ages,
  pops = pops
)
}
