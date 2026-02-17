\name{getExpectedMortality}
\alias{getExpectedMortality}
\title{Expected mortality between two dates}
\usage{
getExpectedMortality(
  country,
  date1,
  date2,
  min.age,
  pops = NULL,
  hmd.user,
  hmd.pw,
  noisy = FALSE
)
}
\description{
Computes the expected mortality rate between two dates for the population
aged \code{min.age} and above in a given country.
}
\arguments{
\item{country}{ISO2 country code.}
\item{date1}{Start date.}
\item{date2}{End date.}
\item{min.age}{Minimum age to include in the calculation.}
\item{pops}{Population data. If \code{NULL}, data are fetched via
\code{popBySingleYear(country = country)}.}
\item{hmd.user}{HMD username.}
\item{hmd.pw}{HMD password.}
\item{noisy}{If \code{TRUE}, emit informational messages.}
}
\value{
Numeric scalar giving the expected mortality rate.
}
\details{
The function combines population-by-age counts with gender-specific mortality
probabilities derived from HMD data via \code{calculateMortality2()}.
If \code{country} is \code{"NIR"}, population counts are drawn from GB WPP
data while mortality indexing uses the NIR code.
}
\examples{
\dontrun{
# Requires HMD credentials and population data.
rate <- getExpectedMortality(
  country = "GB",
  date1 = as.Date("2015-05-07"),
  date2 = as.Date("2017-06-08"),
  min.age = 18,
  hmd.user = "your_user",
  hmd.pw = "your_pw"
)
}
