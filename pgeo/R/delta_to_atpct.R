#' Convert isotope delta notation to atom percent
#'
#' Often, one needs to convert delta-permil data from isotope-ratio
#' mass spectrometry to atom-percent, i.e. the percentage (or fraction)
#' an isotopes makes up of a given element. This is crucial for mass balance
#' calculations, since delta-values are not proportional to the extent of
#' labelling.  
#'
#' @param delta A vector of delta-values, in permilles 
#' @param R The R value of the reference material on which the delta values are based (defaults to NULL)
#' @param isotope Instead of passing the R value as argument, one can also indicated the
#'     isotope using a character string. This works for C-13 (PD-belemnite), N-15 (Air), O-18 (VSMOW) and H-2 (VSMOW). 
#'     The specific formatting does not matter, e.g. `C13', `C-13', `13C' or `13-C' will all work.
#'     `D' is equivalent to `H-2'.
#' @param fraction logical (defaults to FALSE). 
#'         If TRUE, then the isotopic content is returned as a fraction, i.e. 1.0 for 100\%.
#'         If FALSE, a value of 100 indicates 100\%. 
#' @return atom percentages (or fractions)
#' @examples
#' delta.to.atpct(-28,isotope="C13")
#' ## [1] 1.080435
#' delta.to.atpct(c(23,47),isotope="N-15",fraction=TRUE)
#' ## [1] 0.003746932 0.003834500
#' delta.to.atpct(-280,isotope="D")
#' ## [1] 0.01121346
#' delta.to.atpct(-280,isotope="H-2")
#' ## [1] 0.01121346
#' delta.to.atpct(-280,isotope="2H")
#' ## [1] 0.01121346
#' @author Pascal Niklaus \email{pascal.niklaus@@ieu.uzh.ch}
#' @export    
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
