#' Convert MODIS (line,sample) to Swiss Grid coordinates and vice versa
#'
#' These functions converts MODIS line/sample coordinates (250, 500 or 1000m pixel size) from 
#' tile H18V04 to Swiss Grid coordinates in meters and vice versa.
#' The accuracy of the conversion is within a few meters. 
#'
#' These functions were developed by first converting modis line/sample coordinates to WGS-84 
#' latitude and longitude using the MODIS tile mapper
#' (\url{http://landweb.nascom.nasa.gov/cgi-bin/developer/tilemap.cgi}), then converting these to 
#' Swiss Grid norting and easting using the approximate equations provided by SwissTopo 
#' (\url{http://www.swisstopo.admin.ch/internet/swisstopo/en/home/products/software/products/skripts.html}), 
#' and fitting a corresponding linear model (up to third order polynomial terms of I,J) until an 
#' accuracy of a few meters was reached.  
#' Note that the conversion from MODIS line/sample coordinates to Swiss Grid is fast, whereas 
#' conversion in the opposite direction is very slow since it bases on an optimization procedure, 
#' internally calling R's \code{optim} function.  
#' 
#' line/sample can be passed as separate arguments, or as a single argument combined into a data frame. 
#' If the columns are not named \code{I} and \code{J}, the first is assumed to contain I and 
#' the second to contain J.
#'
#' @param I MODIS tile line (O..4799), or a data frame containing tile line and sample in separate columns. \code{I} can be fractional.
#' @param J MODIS tile sample (0..4799), or NULL if the sample has been passed together with the line as a data frame. \code{J} can be fractional.
#' @param N Northing, in kilometers (Swiss Grid), or a data frame containing northing and easting as separate columns.
#' @param E Easting, in kilometers (Swiss Grid), or NULL if the easting has been passed together with the 
#'          northing as a data frame.
#' @param pixel Modis pixel size in meters. Defaults to 250. Can also be 500 or 1000.
#' @return  Returns a data frame with Swiss Grid northing and easting in columns designated N and E,
#'          or a data frame with MODIS line and sample in columns designated I and J.   
#' @examples
#' require(pgeo);
#'
#' modisIJtoCH(1463,2437)
#' ##        N      E
#' ## 1 199995 600038
#'
#' # the following is very slow, use only for a few data points!
#' CHtomodisIJ(199995, 600038)
#' ##      I    J
#' ## 1 1463 2437
#'
#' CHtoWGS(199995, 600038)
#' ##      lat     lon
#' ## 1 46.951 7.43914
#'
#' # Compare this to the tile mapper output for the forward mapping of this coordinate:
#' # http://landweb.nascom.nasa.gov/cgi-bin/developer/tilemap.cgi
#' # Map Projection/Grid: Sinusoidal
#' # Pixel size: 0.25km
#' # Mapping type: Tile/image coordinates 
#' # Forward mapping (geographic coordinates in degrees): latitude, longitude
#' # [sn q fwd tp] lat 46.951000  long 7.439140  =>  vert tile 4  horiz tile 18  line 1463.02  samp 2437.00
#' @rdname modisCH
#' @export
modisIJtoCH <- function(I,J=NULL,pixel=250) {
  if(length(dim(I))==2 && is.null(J)) {
    if(all(c("I","J") %in% names(I))) {
      J<-I$J;
      I<-I$I;
    } else {
      J<-I[,2];
      I<-I[,1];
    }
  }

  if(pixel==250) {  
    I<-I-1463;
    J<-J-2437;

    data.frame(
    N =  (200000 -4.734387481
          -231.6057042*I
          -0.001956104727*J
          +8.564994985e-05*I^2
          +0.004522403442*J^2
          -5.441174078e-08*I^3
          -0.0008587197765*I*J
          +5.450447129e-08*I^2*J
          -1.753951748e-07*I * J^2),    
    E =  (600000 + 38.32148185
          -22.04763502*I
          +232.3333052*J
          +0.0003763913463*I^2
          -3.037466675e-07*J^2
          -5.898755241e-08*J^3
          -2.946377439e-05*I*J
          +1.511884919e-07*I^2*J
          +2.217079808e-08*I*J^2));
  } else if(pixel==500) {
    I<-I-730;
    J<-J-1200;
    data.frame(
        N = ( 200000 + 580.299014353816
          +I * -463.150029821094
          +J * -0.659952850893845
          +I^2 * 0.000336272263047911
          +J^2 * 0.0180913677275943
          +I^3 * -4.35231222686723e-07
          +I * J * -0.00338475381066359
          +I^2 * J * 4.36035907632673e-07
          +I * J^2 * -1.40316143017419e-06 ),
        E = ( 600000 -8386.72290726289
            +I * -44.096768394133
            +J * 464.666339994653
            +I^2 * 0.00148349149573048
            +J^2 * 2.43973683358157e-05
            +J^3 * -4.71894109241974e-07
            +I*J * -0.000127349259576026
            +I^2*J * 1.20951872572593e-06
            +I*J^2 * 1.77355098239527e-07 ));
  }  else if(pixel==1000) {
    I<-I-365;
    J<-J-612;
    data.frame(
      N = ( 200000 + 343.371760296795
          +I * -926.46691861346
          +J * 0.449589578103396
          +I^2 * 0.00138530579794754
          +J^2 * 0.0723627265736565
          +I^3 * -3.48180036069301e-06
          +I * J * -0.0138125164096409
          +I^2 * J * 3.48769113762746e-06
          +I * J^2 * -1.12236771456949e-05 ),
      E = ( 600000 + 2975.56309010675
          +I * -88.1965572340521
          +J * 929.333253502733
          +I^2 * 0.00605205556356213
          +J^2 * -4.08994747380051e-05
          +J^3 * -3.77469913194586e-06
          +I * J * -0.000469801402281904
          +I^2 * J * 9.67527092679532e-06
          +I * J^2 * 1.41877564034081e-06 )); 
  } else {
    stop("Unknown MODIS grid: ",grid);
  }
}
  
#' @rdname modisCH
#' @export
CHtomodisIJ <- function(N,E=NULL,pixel=250) {
  RSS <- function(x) {      
    sum((modisIJtoCH(x[1],x[2],pixel=pixel)-c(N0,E0))^2);
  }
  
  if(length(dim(N))==2 && is.null(E)) {
    if(all(c("N","E") %in% names(N))) {
      E<-N$E;
      N<-N$N;
    } else {
      E<-N[,2];
      N<-N[,1];
    }
  }
  
  ## need to create these since the assign in the following tapply will look for them
  ## kind of ugly...
  N0<-0;
  E0<-0;

  r <- data.frame(t(apply(cbind(N,E),1,function(x) { 
                       assign("N0",x[1],inherits=TRUE);
                       assign("E0",x[2],inherits=TRUE); 
                       optim(c(1463/(pixel/250),2437/(pixel/250)),RSS,method="BFGS")$par; 
                     }
        ))) 
  names(r) <- c("I","J");
  r;
}


