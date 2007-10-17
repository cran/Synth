\name{fn.V}
\alias{fn.V}
\title{Loss Function for nested optimization of W and V weights}
\description{This is the loss function for the nested optimization of W and V weights used for constructing synthtic 
             control groups according to the methods outlined in Abadie and Gardeazabal (2003) and Abadie, Diamond, Hainmueller (2007)
             (see references). This function is called by \code{\link{synth}} automatically, and should not be
             called manually by a normal user.}

\usage{fn.V(variables.v = stop("variables.v missing"), 
X0.scaled = stop("X0.scaled missing"), X1.scaled = stop("X0.scaled missing"), 
Z0 = stop("Z0 missing"), Z1 = stop("Z1 missing"), margin.ipop = 0.0005, sigf.ipop = 5, bound.ipop = 10, ...)

}
\arguments{
  \item{variables.v}{NA}
  \item{X0.scaled}{NA}
  \item{X1.scaled}{NA}
  \item{Z0}{NA}
  \item{Z1}{NA}
  \item{margin.ipop}{NA}
  \item{sigf.ipop}{NA}
  \item{bound.ipop}{NA}
  \item{\dots}{NA}

}
\details{NA}
\value{A list object containing the optimization results.
}
\references{Abadie, A. and Gardeazabal, J. (2003) Economic Costs of Conflict: A Case Study of the Basque Country 
            \emph{American Economic Review} 93 (1) 113--132 
             \url{http://ksghome.harvard.edu/~.aabadie.academic.ksg/ecc.pdf}
           
           Abadie, A., Diamond, A., Hainmueller, J. (2007) Synthetic Control Methods
            for Comparative Case Studies: Estimating the Effect of California's Tobacco Control Program
            \emph{NBER Technical Working Paper no 335}
            \url{http://www.people.fas.harvard.edu/~jhainm/}

            }
   
\author{Alexis Diamond \email{adiamond@fas.harvard.edu} Jens Hainmueller \email{jhainm@fas.harvard.edu}}\note{NA}
\seealso{ \code{gaps.plot},\code{synth.plot},\code{dataprep},\code{synth.tab} }
\examples{
##---- NA -----
}
\keyword{methods}

