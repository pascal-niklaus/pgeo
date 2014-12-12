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

