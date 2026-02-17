\name{calcPropNewEligUN2}
\alias{calcPropNewEligUN2}
\title{Proportion newly eligible using UN population data}
\usage{
calcPropNewEligUN2(
  country,
  prev.date,
  current.date,
  min.age.current,
  min.age.prev,
  pops
)
}
\description{
Estimates the proportion of the population newly eligible to vote between two
reference dates based on single-year population counts.
}
\arguments{
\item{country}{ISO2 country code ("NIR" is mapped to "GB").}
\item{prev.date}{Previous election or reference date.}
\item{current.date}{Current election or reference date.}
\item{min.age.current}{Minimum age for eligibility at \code{current.date}.}
\item{min.age.prev}{Minimum age for eligibility at \code{prev.date}.}
\item{pops}{Population data frame with single-year ages (e.g., from
\code{popBySingleYear()}).}
}
\value{
Numeric scalar giving the proportion newly eligible.
}
\details{
The function shifts ages to reflect the time gap between elections and the
eligibility threshold change. It interpolates a cumulative eligibility curve
and returns the share of the population newly entering the eligible range.
}
\examples{
\dontrun{
# Requires population data from popBySingleYear()
prop <- calcPropNewEligUN2(
  country = "GB",
  prev.date = as.Date("2017-06-08"),
  current.date = as.Date("2019-12-12"),
  min.age.current = 18,
  min.age.prev = 18,
  pops = popBySingleYear(source = "wpp", country = "GB")
)
}
