#' Conversion between Swiss Grid and WGS-84
#'
#' These functions convert coordinates between WGS-84 (latitude/longitude) and the Swiss Grid (X:northing/Y:easting). 
#' The conversion is approximate with an accuracy of a few meters, based on equations provided by SwissTopo.
#' These functions return the converted coordinates. 
#'
#' All variants of CHtoWGS and WGStoCH optionally take a data frame with two columns as first argument, 
#' and will return the respective result in a data frame with two columns. The content of the data frame
#' is interpreted according to the column names (\code{N} and \code{E} or \code{lat} and \code{long}) or,
#' if other names are used, according to the default argument order.
#'
#' @param lat Latitude, in degrees, or a data frame containing latitude and longitude as separate columns.
#' @param lon Longitude, in degrees, or NULL if the longitude has been passed as together with the 
#'            latitude as a data frame.
#' @param N Northing, in kilometers (Swiss Grid), or a data frame containing northing and 
#'          easting as separate columns.
#' @param E Easting, in kilometers (Swiss Grid), or NULL of the easting has been passed together 
#'          with the northing as a data frame.
#' @return Returns WGS-84 latitude or longitude, or Swiss Grid norting or easting. 
#'        If both coordinates are returned, they are stored in a data frame with the respective column names.
#' @examples
#' 
#' library(pgeo);
#'
#' WGStoCH(lat=47,lon=7);
#' ##        N      E
#' ## 1 205532 566639
#'
#' CHtoWGS(N=205532,E=566639)
#' ##   lat lon
#' ## 1  47   7
#'
#' WGStoCH(data.frame(lat=c(47,47.2),lon=7))
#' ##        N      E
#' ## 1 205532 566639
#' ## 2 227766 566764
#'
#' WGStoCHN(data.frame(lat=c(47,47.2),lon=7))
#' ## [1] 205532 227766
#' @author Pascal Niklaus \email{pascal.niklaus@@ieu.uzh.ch}  
#' @rdname WGSCH
#' @export
WGStoCH <- function(lat,lon=NULL) 
{
  if(length(dim(lat))==2) {
    if(all(c("lat","lon") %in% names(lat))) {
      lon<-lat$lon;
      lat<-lat$lat;
    } else {   
      lon<-lat[,2];
      lat<-lat[,1];
    }
  }
  stopifnot(length(lat)==length(lon) && is.numeric(lat) && is.numeric(lon));
  data.frame(N=WGStoCHN(lat,lon),E=WGStoCHE(lat,lon));
}

#' @rdname WGSCH
#' @export
CHtoWGS <- function(N,E=NULL) 
{
  if(length(dim(N))==2) {
    if(all(c("N","E") %in% names(N))) {
      E<-N$E;
      N<-N$N;
    } else {
      E<-N[,2];
      N<-N[,1];
    }
  }
  stopifnot(length(N)==length(E) && is.numeric(N) && is.numeric(E));
  data.frame(lat=CHtoWGSlat(N,E),lon=CHtoWGSlon(N,E));
}

#' @rdname WGSCH
#' @export
WGStoCHE <- function(lat, lon=NULL)
{
  if(length(dim(lat))==2) {
    if(all(c("lat","lon") %in% names(lat))) {
      lon<-lat$lon;
      lat<-lat$lat;
    } else {   
      lon<-lat[,2];
      lat<-lat[,1];
    }
  }
  lat_aux <- (3600*lat - 169028.66)/10000
  lon_aux <- (3600*lon - 26782.5)/10000
  
  600072.37 + 
  211455.93 * lon_aux - 
  10938.51 * lon_aux * lat_aux -
  0.36 * lon_aux * (lat_aux^2) -
  44.54 * (lon_aux^3);
}

#' @rdname WGSCH
#' @export
WGStoCHN <- function(lat, lon=NULL)
{
  if(length(dim(lat))==2) {
    if(all(c("lat","lon") %in% names(lat))) {
      lon<-lat$lon;
      lat<-lat$lat;
    } else {   
      lon<-lat[,2];
      lat<-lat[,1];
    }
  }
  lat_aux <- (3600*lat - 169028.66)/10000
  lon_aux <- (3600*lon - 26782.5)/10000

  200147.07 +
  308807.95 * lat_aux + 
  3745.25 * (lon_aux^2) +
  76.63 * (lat_aux^2) -
  194.56 * (lon_aux^2) * lat_aux +
  119.79 * (lat_aux^3);
}

#' @rdname WGSCH
#' @export
CHtoWGSlat <- function (N, E=NULL)
{
  if(length(dim(N))==2) {
    if(all(c("N","E") %in% names(N))) {
      E<-N$E;
      N<-N$N;
    } else {
      E<-N[,2];
      N<-N[,1];
    }
  }
  y_aux <- (E - 600000)/1000000
  x_aux <- (N - 200000)/1000000
  
  100/36 * {16.9023892 +
          3.238272 * x_aux -
          0.270978 * (y_aux^2) -
          0.002528 * (x_aux^2) -
          0.0447   * (y_aux^2) * x_aux -
          0.0140   * (x_aux^3)};
}

#' @rdname WGSCH
#' @export
CHtoWGSlon <- function (N, E=NULL)
{
  if(length(dim(N))==2) {
    if(all(c("N","E") %in% names(N))) {
      E<-N$E;
      N<-N$N;
    } else {
      E<-N[,2];
      N<-N[,1];
    }
  }
  y_aux <- (E - 600000)/1000000
  x_aux <- (N - 200000)/1000000
  
  100/36* {2.6779094 +
           4.728982 * y_aux +
           0.791484 * y_aux * x_aux +
           0.1306   * y_aux * (x_aux^2) -
           0.0436   * (y_aux^3)};
}

