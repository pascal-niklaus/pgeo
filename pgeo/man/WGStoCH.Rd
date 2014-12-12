\name{WGStoCH}
\alias{WGStoCH}
\alias{CHtoWGS}
\alias{WGStoCHN}
\alias{WGStoCHE}
\alias{CHtoWGSlat}
\alias{CHtoWGSlon}
\title{
  Conversion between Swiss Grid and WGS-84
}
\description{
  These functions convert coordinates between WGS-84 (latitude/longitude) and the Swiss Grid (X:northing/Y:easting). 
  The conversion is approximate with an accuracy of a few meters, based on equations provided by SwissTopo.
}
\usage{
  WGStoCH(lat, lon = NULL)
  WGStoCHN(lat, lon = NULL)
  WGStoCHE(lat, lon = NULL)
  CHtoWGS(N,E = NULL)
  CHtoWGSlat(N,E = NULL)
  CHtoWGSlon(N,E = NULL)
}
\arguments{
  \item{lat}{Latitude, in degrees, or a data frame containing latitude and longitude as separate columns.
  }
  \item{lon}{Longitude, in degrees, or NULL if the longitude has been passed as together with the latitude as a data frame.
  }
  \item{N}{Northing, in kilometers (Swiss Grid), or a data frame containing northing and easting as separate columns.
  }
  \item{E}{Easting, in kilometers (Swiss Grid), or NULL of the easting has been passed together with the northing as a data frame.
  }
}
\details{
  These functions return the converted coordinates. 
  All variants of CHtoWGS and WGStoCH optionally take a data frame with two columns as first argument, 
  and will return the respective result in a data frame with two columns. The content of the data frame
  is interpreted according to the column names (\code{N} and \code{E} or \code{lat} and \code{long}) or,
  if other names are used, according to the default argument order.
}
\value{
  Returns WGS-84 latitude or longitude, or Swiss Grid norting or easting. 
  If both coordinates are returned, they are stored in a data frame with the respective column names.
}
\author{
  Pascal A. Niklaus (pascal.niklaus@ieu.uzh.ch)
}
\note{
  These functions are slightly modified from the equations provided by
  http://www.swisstopo.admin.ch/internet/swisstopo/en/home/products/software/products/skripts.html
}
\seealso{
  An online converter can be found here:
  http://www.swisstopo.admin.ch/internet/swisstopo/en/home/apps/calc/navref.html
}
\examples{

require(pascal.geo);

WGStoCH(lat=47,lon=7);

#        N      E
# 1 205532 566639

CHtoWGS(N=205532,E=566639)

#   lat lon
# 1  47   7

WGStoCH(data.frame(lat=c(47,47.2),lon=7))

#        N      E
# 1 205532 566639
# 2 227766 566764

WGStoCHN(data.frame(lat=c(47,47.2),lon=7))
# [1] 205532 227766

}
\keyword{misc}

