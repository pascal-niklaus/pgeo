
modisIJtoCH <- function(I,J=NULL,grid=250) {
  if(length(dim(I))==2 && is.null(J)) {
    if(all(c("I","J") %in% names(I))) {
      J<-I$J;
      I<-I$I;
    } else {
      J<-I[,2];
      I<-I[,1];
    }
  }

  if(grid==250) {  
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
  } else if(grid==500) {
    I<-I-730;
    J<-J-1200;
    data.frame(
        N = ( 200000 + 580.299014353816
          +I0 * -463.150029821094
          +J0 * -0.659952850893845
          +I0^2 * 0.000336272263047911
          +J0^2 * 0.0180913677275943
          +I0^3 * -4.35231222686723e-07
          +I0 * J0 * -0.00338475381066359
          +I0^2 * J0 * 4.36035907632673e-07
          +I0 * J0^2 * -1.40316143017419e-06 ),
        E = ( 600000 -8386.72290726289
            +I * -44.096768394133
            +J * 464.666339994653
            +I^2 * 0.00148349149573048
            +J^2 * 2.43973683358157e-05
            +J^3 * -4.71894109241974e-07
            +I*J * -0.000127349259576026
            +I^2*J * 1.20951872572593e-06
            +I*J^2 * 1.77355098239527e-07 ));
  } else {
    stop("Unknown MODIS grid: ",grid);
  }
}
  
CHtomodisIJ <- function(N,E=NULL,grid=250) {
  RSS <- function(x) {      
    sum((modisIJtoCH(x[1],x[2],grid=grid)-c(N0,E0))^2);
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
                       optim(c(1463,2437),RSS,method="BFGS")$par; 
                     }
        ))) 
  names(r) <- c("I","J");
  r;
}


