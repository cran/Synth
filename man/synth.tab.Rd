\name{synth.tab}
\alias{synth.tab}
\title{Creates Tables that Summarize Results of Synthetic Control Group Method}
\description{
  This function is called after \code{\link{dataprep}} and \code{\link{synth}} in order to create tables summarizing the results of the 
  run of the synthetic control method. The result tables can be latexed directly.}
\usage{
synth.tab(synth.res    = NA,
          dataprep.res = NA,
          round.digit  = 3)
}

\arguments{
  \item{synth.res}{The list resulting from the call to \code{\link{synth}}.}
  \item{dataprep.res}{The list resulting from the call to \code{\link{dataprep}}.}
  \item{round.digit}{Integer for rounding in tables.}
}
\details{NA}
\value{
  \item{tab.v}{The matrix that contains the table of V-weights and respective variable names.}
  \item{tab.w}{The matrix that contains the table of W-weights and respective unit numbers and possibly names.}
  \item{tab.loss}{The matrix that contains the table of W-loss and V-loss}
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
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--    or do  help(data=index)  for the standard data sets.

## The function is currently defined as
}
\keyword{methods}
