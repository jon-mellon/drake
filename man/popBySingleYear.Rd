\name{popBySingleYear}
\alias{popBySingleYear}
\title{Population by single-year age}
\usage{
popBySingleYear(
  source = c("wpp", "hmd"),
  country,
  mort.countries = getMortCountries(),
  hmd.user = NULL,
  hmd.pw = NULL
)
}
\description{
Fetches population-by-single-year age data from either UN WPP or the Human
Mortality Database (HMD).
}
\arguments{
\item{source}{Data source: \code{"wpp"} for UN WPP or \code{"hmd"} for HMD.}
\item{country}{ISO2 country code.}
\item{mort.countries}{Mapping from ISO2 to HMD identifiers (see
\code{getMortCountries()}).}
\item{hmd.user}{HMD username (required for \code{source = "hmd"}).}
\item{hmd.pw}{HMD password (required for \code{source = "hmd"}).}
}
\value{
A data frame with columns \code{Time}, \code{AgeGrp}, \code{PopFemale},
\code{PopMale}, \code{PopTotal}, and \code{country}.
}
\details{
The UN WPP pathway uses \code{tidywpp::get_wpp}. The HMD pathway downloads the
\code{Population} table and returns counts in thousands.
}
\examples{
\dontrun{
# UN WPP example
pops <- popBySingleYear(source = "wpp", country = "GB")

# HMD example (requires credentials)
# pops <- popBySingleYear(source = "hmd", country = "GB", hmd.user = "u", hmd.pw = "p")
}
