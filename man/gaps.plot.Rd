\name{gaps.plot}
\alias{gaps.plot}
\title{Plots Gap in Outcome Trajectories between the Treated its Synthetic Control Unit}
\description{ This function plots the gaps in the trajectories of the outcome variable for the treated unit and the 
  synthetic control group constructed by \code{\link{synth}} and \code{\link{dataprep}}. The user can specifify whether
  the whole time period or only the pre-treatment period should be plotted.
}
\usage{gaps.plot(synth.res = NA,
       dataprep.res = NA,
       Ylab = c("Title"),
       Xlab = c("Time"),
       Main = c("Gaps: Treated - Synthetic"),
       tr.intake = NA,
       Ylim = NA,
       Z.plot = FALSE)
}
\arguments{
  \item{synth.res}{Output list created by \code{\link{synth}}.}
  \item{dataprep.res}{Output list created by \code{\link{dataprep}}.}
  \item{tr.intake}{Optional scalar to indicate the time of treatment intake with a vertical line.}
  \item{Ylab}{Optional label for Y axis.}
  \item{Xlab}{Optional label for X axis.}
  \item{Ylim}{Optional Ylim.}
  \item{Main}{Optional main title.}
  \item{Z.plot}{Flag. If true, only pretreatment period is plotted.}
}
\details{ The trajectory of the outcome for the synthetic control group is calculated as:
  dataprep.res$Y0plot \%*\% synth.res$solution.w. You can use this calculation to construct custom made plots.
}
\value{The plot of trajectories.}
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
}
\keyword{methods}

