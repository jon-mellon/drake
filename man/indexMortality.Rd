\name{indexMortality}
\alias{indexMortality}
\title{Index cohort mortality from HMD tables}
\usage{
indexMortality(mortality.table, index.year = 1967, valid.years, elec.gap, noisy = TRUE)
}
\description{
Builds a cohort mortality index from single-year mortality rates, returning
cumulative mortality for each year-of-birth and gender after a specified gap.
}
\arguments{
\item{mortality.table}{Data frame of mortality rates (e.g., from
\code{HMDHFDplus::readHMDweb} with \code{item = "Mx_1x1"}).}
\item{index.year}{Reference year for indexing.}
\item{valid.years}{Vector of year-of-birth values to include.}
\item{elec.gap}{Years to project forward from the index year.}
\item{noisy}{If \code{TRUE}, emit informational messages.}
}
\value{
A data frame with columns \code{mortality}, \code{gender}, and \code{yob}.
}
\details{
The function extends open-age intervals, fills missing cohorts, and computes
cumulative mortality using cohort survival rates. A loess smoother interpolates
cumulative mortality at non-integer gaps when needed.
}
\examples{
\dontrun{
# Requires an HMD mortality table.
mtab <- HMDHFDplus::readHMDweb("GBR_NP", "user", "pw", item = "Mx_1x1")
idx <- indexMortality(mtab, index.year = 2010, valid.years = 1940:2000, elec.gap = 5)
}
