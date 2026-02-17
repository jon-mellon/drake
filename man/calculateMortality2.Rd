\name{calculateMortality2}
\alias{calculateMortality2}
\title{Calculate mortality for a sample using HMD tables}
\usage{
calculateMortality2(
  data,
  country,
  elec.gap,
  suffix,
  elec1.year,
  debug = FALSE,
  hmd.user,
  hmd.pw,
  mort.countries,
  noisy = FALSE
)
}
\description{
Calculates expected mortality for each row in a sample by matching birth year
and gender to a mortality index derived from HMD data.
}
\arguments{
\item{data}{Data frame containing age and gender columns. Columns are expected
to be named \code{age{suffix}} and \code{gender{suffix}}.}
\item{country}{ISO2 country code.}
\item{elec.gap}{Time gap (years) between two reference points.}
\item{suffix}{Suffix appended to age and gender column names.}
\item{elec1.year}{Reference year for the first election or baseline.}
\item{debug}{If \code{TRUE}, enter the debugger.}
\item{hmd.user}{HMD username.}
\item{hmd.pw}{HMD password.}
\item{mort.countries}{Mapping from ISO2 to HMD country identifiers.}
\item{noisy}{If \code{TRUE}, emit informational messages.}
}
\value{
Numeric vector of mortality probabilities aligned with \code{data}.
}
\details{
The function downloads single-year mortality rates from the Human Mortality
Database (HMD), constructs a cohort mortality index using
\code{indexMortality()}, and merges the resulting probabilities back onto the
sample by year of birth and gender.
}
\examples{
\dontrun{
# Requires HMD credentials.
mtab <- calculateMortality2(
  data = data.frame(age = c(30, 40), gender = c("Male", "Female")),
  country = "GB",
  elec.gap = 5,
  suffix = "",
  elec1.year = 2015,
  hmd.user = "your_user",
  hmd.pw = "your_pw",
  mort.countries = getMortCountries()
)
}
