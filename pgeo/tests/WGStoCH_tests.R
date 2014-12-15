library(pgeo)

## http://landweb.nascom.nasa.gov/cgi-bin/developer/tilemap.cgi
## http://www.swisstopo.admin.ch/internet/swisstopo/en/home/apps/calc/navref.html

filename <- system.file("extdata","validation.csv",package="pgeo")

d <- read.csv(filename,comment.char="#")

approxEq <- function(x1,x2,prec=1e-5) {
  all(abs(x1-x2)/rowMeans(cbind(abs(x1),abs(x2)))<prec);
}

########################### Testing WGStoCH conversion ########################

cat("### Testing WGStoCH ...\n");

deviation <- numeric(nrow(d))
for(i in seq_along(rownames(d))) {
  # testing base function and recording deviation
  tmp<- WGStoCH(lat=d$lat[i],lon=d$lon[i])
  stopifnot(approxEq(c(d$N[i],d$E[i]),c(tmp$N,tmp$E)))
  deviation[i]<-sqrt((d$N[i]-tmp$N)^2+(d$E[i]-tmp$E)^2)
  
  # testing alternative parameter passing styles
  tmp<- WGStoCH(data.frame(P1=d$lat[i],P2=d$lon[i]))
  stopifnot(approxEq(c(d$N[i],d$E[i]),c(tmp$N,tmp$E)))

  tmp<- WGStoCH(data.frame(lon=d$lon[i],lat=d$lat[i]))
  stopifnot(approxEq(c(d$N[i],d$E[i]),c(tmp$N,tmp$E)))

  # testing separate function for E and N component  
  tmp<- WGStoCHE(data.frame(lat=d$lat[i],lon=d$lon[i]))
  stopifnot(approxEq(d$E[i],tmp))

  tmp<- WGStoCHN(data.frame(lon=d$lon[i],lat=d$lat[i]))
  stopifnot(approxEq(d$N[i],tmp))
}

cat("    Deviation in obtained CH1903 coordinates (in meters):\n",
    "    - mean =",mean(deviation),"\n",
    "    - max =",max(deviation),"\n")

cat("### Testing CHtoWGS ...\n");

deviation <- numeric(nrow(d))
for(i in seq_along(rownames(d))) {
  # testing base function and recording deviation
  tmp<- CHtoWGS(E=d$E[i],N=d$N[i])
  stopifnot(approxEq(c(d$lat[i],d$lon[i]),c(tmp$lat,tmp$lon)))
  # approx angle in degrees
  deviation[i]<-sqrt((d$lat[i]-tmp$lat)^2+(d$lon[i]-tmp$lon)^2)  
  
  # testing alternative parameter passing styles
  tmp<- CHtoWGS(data.frame(E=d$E[i],N=d$N[i]))
  stopifnot(approxEq(c(d$lat[i],d$lon[i]),c(tmp$lat,tmp$lon)))

  tmp<- CHtoWGS(data.frame(P1=d$N[i],P2=d$E[i]))
  stopifnot(approxEq(c(d$lat[i],d$lon[i]),c(tmp$lat,tmp$lon)))

  # testing separate function for E and N component  
  tmp<- CHtoWGSlat(data.frame(N=d$N[i],E=d$E[i]))
  stopifnot(approxEq(d$lat[i],tmp))

  tmp<- CHtoWGSlon(data.frame(N=d$N[i],E=d$E[i]))
  stopifnot(approxEq(d$lon[i],tmp))
}

cat("    Deviation in obtained (lat,lon) coordinates (in degrees):\n",
    "    - mean =",mean(deviation),"\n",
    "    - max =",max(deviation),"\n")

    
  