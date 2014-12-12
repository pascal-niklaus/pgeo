library(pgeo)

## approximate conversions obtained from
## http://www.swisstopo.admin.ch/internet/swisstopo/en/home/apps/calc/navref.html
## using altitude = 400m (does not matter much)

approxEq <- function(x1,x2,prec=1e-5) {
  all(abs(x1-x2)/rowMeans(cbind(abs(x1),abs(x2)))<prec);
}

lat <- 47;
lon <- 7;

N <- 205531.520
E <- 566639.444

d<- WGStoCH(lat=lat,lon=lon)
stopifnot(approxEq(c(N,E),c(d$N,d$E)))

d<- CHtoWGS(N=N,E=E)
stopifnot(approxEq(c(lat,lon),c(d$lat,d$lon)))

N0 <- WGStoCHN(lat=lat,lon=lon)
N1 <- WGStoCHN(data.frame(lat=lat,lon=lon))
N2 <- WGStoCHN(data.frame(lon=lon,lat=lat))
N3 <- WGStoCHN(data.frame(A=lat,B=lon))

stopifnot(approxEq(c(N0,N1,N2,N3),N));

E0 <- WGStoCHE(lat=lat,lon=lon)
E1 <- WGStoCHE(data.frame(lat=lat,lon=lon))
E2 <- WGStoCHE(data.frame(lon=lon,lat=lat))
E3 <- WGStoCHE(data.frame(A=lat,B=lon))

stopifnot(approxEq(c(E0,E1,E2,E3),E));

llat0 <- CHtoWGSlat(N=N,E=E)
llat1 <- CHtoWGSlat(data.frame(N=N,E=E))
llat2 <- CHtoWGSlat(data.frame(E=E,N=N))
llat3 <- CHtoWGSlat(data.frame(A=N,B=E))

stopifnot(approxEq(c(llat0,llat1,llat2,llat3),lat));

llon0 <- CHtoWGSlon(N=N,E=E)
llon1 <- CHtoWGSlon(data.frame(N=N,E=E))
llon2 <- CHtoWGSlon(data.frame(E=E,N=N))
llon3 <- CHtoWGSlon(data.frame(A=N,B=E))

stopifnot(approxEq(c(llon0,llon1,llon2,llon3),lon));

