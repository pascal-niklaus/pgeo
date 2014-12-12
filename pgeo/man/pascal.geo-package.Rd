\name{pascal.geo-package}
\alias{pascal.geo-package}
\alias{pascal.geo}
\docType{package}
\title{
Conversion of Swiss Grid to WGS-84 and vice versa.
}
\description{
  These functions convert coordinates between WGS-84 (latitude/longitude) and the Swiss Grid (X:northing/Y:easting). 
  The conversion is approximate with an accuracy of a few meters, based on equations provided by SwissTopo.
}
\details{
\tabular{ll}{
Package: \tab pascal.geo\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2014-03-21\cr
License: \tab GPL\cr
}
Conversion between WGS-84 and Swiss Grid, using functions such as WGStoCH and CHtoWGS.
}
\author{
Pascal A. Niklaus

Maintainer: Pascal A. Niklaus <pascal.niklaus@ieu.uzh.ch>
}
\references{
}
\keyword{ package }
\seealso{
}
\examples{
WGStoCH(lat=47,lon=7)
}
