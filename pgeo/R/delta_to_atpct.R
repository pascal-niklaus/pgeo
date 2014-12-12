# Karlen, I., Olsson, I.U., Kallburg, P. and Kilici, S., 1964. 
# Absolute determination of the activity of two 14C dating standards. 
# Arkiv Geofysik, 4:465-471.

delta.to.atpct <- function(delta, R=NULL, isotope="N15", fraction=FALSE) 
{
  R_ref <- list("C13"=0.011237,"N15"=0.003676466,"O18"=0.002005,"H2"=0.00015576,
                "C14"=1.176e-12);
  if(is.null(R)) {
    atom<-gsub("^.*?([A-Z]+).*?$","\\1",isotope)
    mass<-gsub("^.*?([0-9]+).*?$","\\1",isotope)
    iso <- if(atom=="D") "H2" else paste(atom,mass,sep="");
    R<-R_ref[[iso]];
    if(is.null(R))
      stop("No reference R known for isotope ",isotope,"!");    
  }  
  
  ifelse(fraction,1,100)/(1+(1/((delta/1e3+1)*R)));
}
