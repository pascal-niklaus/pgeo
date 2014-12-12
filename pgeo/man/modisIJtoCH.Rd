\name{modisIJtoCH}
\alias{modisIJtoCH}
\alias{CHtomodisIJ}
\title{
  Convert MODIS (line,sample) to Swiss Grid coordinates and vice versa.
}
\description{
  These functions converts MODIS line/sample coordinates (250m pixel size) from tile H18V04 to Swiss Grid coordinates in meters
  and vice versa.
  The accuracy of the conversion is within a few meters. 
}
\usage{
  modisIJtoCH(I,J=NULL)
  CHtomodisIJ(N,E=NULL)  
}
\arguments{
  \item{I}{Modis tile line (O..4799), or a data frame containing tile line and sample in separate columns.
  }
  \item{J}{MODIS tile sample (0..4799), or NULL if the sample has been passed together with the line as a data frame.
  }
  \item{N}{Northing, in kilometers (Swiss Grid), or a data frame containing northing and easting as separate columns.
  }
  \item{E}{Easting, in kilometers (Swiss Grid), or NULL if the easting has been passed together with the northing as a data frame.
  }
}
\details{
  These functions were developed by first converting modis line/sample coordinates to WGS-84 latitude and longitude using the MODIS tile mapper
  (http://landweb.nascom.nasa.gov/cgi-bin/developer/tilemap.cgi), then converting these to Swiss Grid norting and easting using the approximate equations 
  provided by SwissTopo (http://www.swisstopo.admin.ch/internet/swisstopo/en/home/products/software/products/skripts.html), and fitting a corresponding
  linear model (up to third order polynomial terms of I,J) until an accuracy of a few meters was reached.  
  Note that the conversion from modis line/sample coordinates to Swiss Grid is fast, whereas conversion in the opposite direction is very slow since
  it bases on an optimization procedure, internally calling R's \code{optim} function.  
  
  line/sample can be passed as separate arguments, or as a single argument combined into a data frame. The first 
}
\value{
  Returns a data frame with Swiss Grid norting and easting in columns designated N and E,
  or a data frame with MODIS line and sample in columns designated I and J.   
}
\author{
  Pascal A. Niklaus (pascal.niklaus@ieu.uzh.ch)
}
\note{
}
\seealso{
}
\examples{
require(pascal.geo);

modisIJtoCH(1463,2437)
#        N      E
# 1 199995 600038

# the following is very slow, use only for a few data points!
CHtomodisIJ(199995, 600038)
#      I    J
# 1 1463 2437

CHtoWGS(199995, 600038)
#      lat     lon
# 1 46.951 7.43914

# Compare this to the tile mapper output for the forward mapping of this coordinate:
# http://landweb.nascom.nasa.gov/cgi-bin/developer/tilemap.cgi
# Map Projection/Grid: Sinusoidal
# Pixel size: 0.25km
# Mapping type: Tile/image coordinates 
# Forward mapping (geographic coordinates in degrees): latitude, longitude
# [sn q fwd tp] lat 46.951000  long 7.439140  =>  vert tile 4  horiz tile 18  line 1463.02  samp 2437.00
}
\keyword{misc}

