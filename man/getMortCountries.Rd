\name{getMortCountries}
\alias{getMortCountries}
\title{Map ISO2 country codes to HMD country identifiers}
\usage{
getMortCountries()
}
\description{
Returns a named character vector mapping ISO2 country codes to Human Mortality
Database (HMD) identifiers used by \code{HMDHFDplus::readHMDweb}.
}
\value{
Named character vector of HMD country codes (with \code{NA} for unsupported
countries).
}
\examples{
getMortCountries()["GB"]
}
