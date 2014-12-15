library(pgeo)

filename <- system.file("extdata","validation.csv",package="pgeo")

d <- read.csv(filename,comment.char="#")

approxEq <- function(x1,x2,prec=1e-5) {
  all(abs(x1-x2)/rowMeans(cbind(abs(x1),abs(x2)))<prec);
}

########################### modis IJ to CH1903 conversion ########################


deviation <- numeric(nrow(d))

for(px in c(250,500,1000)) {
  cat("### Testing modisIJtoCH for pixel size",px,"m ...\n");
  for(i in seq_along(rownames(d))) {
    I<-d[[sprintf("I%d",px)]][i]
    J<-d[[sprintf("J%d",px)]][i]
    tmp <- modisIJtoCH(I=I,J=J,pixel=px)
#     stopifnot(approxEq(c(d$N[i],d$E[i]),c(tmp$N,tmp$E)))
    deviation[i]<-sqrt((d$N[i]-tmp$N)^2+(d$E[i]-tmp$E)^2)
  }
  cat("    Deviation in obtained CH1903 coordinates (in meters):\n",
    "    - mean =",mean(deviation),"\n",
    "    - max =",max(deviation),"\n")
  stopifnot(deviation<20) 
}



deviation <- numeric(nrow(d))

for(px in c(250,500,1000)) {
  cat("### Testing CHtomodisIJ for pixel size",px,"m ...\n");
  for(i in seq_along(rownames(d))) {
    I<-d[[sprintf("I%d",px)]][i]
    J<-d[[sprintf("J%d",px)]][i]
    tmp <- CHtomodisIJ(N=d$N[i],E=d$E[i],pixel=px)
    deviation[i]<-px * sqrt((I-tmp$I)^2+(J-tmp$J)^2)
  }
  cat("    Deviation in obtained modis (I/J) * pixel size (approx meters):\n",
    "    - mean =",mean(deviation),"\n",
    "    - max =",max(deviation),"\n")
  stopifnot(deviation<20) 
}
 
 