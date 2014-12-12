\name{delta.to.atpct}
\alias{delta.to.atpct}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Convert delta-permil data into atom-percent of a given isotope}

\description{
Often, one needs to convert delta-permil data from isotope-ratio
mass spectrometry into atom-percent, i.e. the percentage (or fraction)
an isotopes makes up of a given element. This is crucial for mass balance
calculations, since delta-values are not proportional to the extent of
labelling.  
}
\usage{
delta.to.atpct(delta, R=null, isotope="N15", fraction=FALSE) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{delta}{A vector of delta-values, in permilles (i.e. 0.001 = 1 permil)}
  \item{R}{The R value of the reference material on which the delta values are based}
  \item{isotope}{Instead of passing the R value as argument, one can also indicated the
    isotope character string. This works for C-13 (PD-belemnite), N-15 (Air), O-18 (VSMOW) and H-2 (VSMOW). 
    The specific formatting does not matter, e.g. "C13", "C-13", "13C" or "13-C" will all work.
    "D" works as equivalent to "H-2".    
    }
  \item{fraction}{If TRUE, then the isotopic content is returned as a fraction, i.e. 1.0 for 100%.
       If FALSE, a value of 100 indicates 100%.    
    }
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A vector of atom-percent (or fraction) values.
}
\references{
}
\author{
Pascal A. Niklaus
}
\note{
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
require(pascal.geo)
delta.to.atpct(0,isotope="C-13")
# [1] 1.111
delta.to.atpct(0,isotope="C-13",fraction=TRUE)
# [1] 0.01111
delta.to.atpct(0,isotope="N-15")
# [1] 0.3663
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}
\keyword{misc}
