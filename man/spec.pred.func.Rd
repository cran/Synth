\name{spec.pred.func}
\alias{spec.pred.func}
\title{Special Predictor Function for Dataprep}
\description{ This function is called by \code{\link{dataprep}} to handle special predictors
 in the process of setting up the dataset to be loaded into \code{\link{synth}}.
  It should not be called manually by the normal user.
}
\usage{spec.pred.func(list.object = NULL,
          tr.numb = treatment.identifier,
          co.numb = controls.identifier,
          unit.var = unit.variable,
          time.var = time.variable,
          foo.object = foo,
          X0.inner = X0,
          X1.inner = X1)
}
\arguments{
  \item{list.object}{NA}
  \item{tr.numb}{NA}
  \item{co.numb}{NA}
  \item{unit.var}{NA}
  \item{time.var}{NA}
  \item{foo.object}{NA}
  \item{X0.inner}{NA}
  \item{X1.inner}{NA}
}
\details{NA
}
\value{NA
}
\references{Abadie, A. and Gardeazabal, J. (2003) Economic Costs of Conflict: A Case Study of the Basque Country 
            \emph{American Economic Review} 93 (1) 113--132 
             \url{http://ksghome.harvard.edu/~.aabadie.academic.ksg/ecc.pdf}
           
            Abadie, A., Diamond, A., Hainmueller, J. (2007) Synthetic Control Methods
            for Comparative Case Studies: Estimating the Effect of California's Tobacco Control Program
            \emph{NBER Technical Working Paper no 335}
            \url{http://www.people.fas.harvard.edu/~jhainm/}

            }
   
\author{Alexis Diamond \email{adiamond@fas.harvard.edu} Jens Hainmueller \email{jhainm@fas.harvard.edu}}\note{}
\seealso{ \code{gaps.plot},\code{synth.plot},\code{dataprep},\code{synth.tab} }
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
}
\keyword{methods}
