\name{gaps.plot}
\alias{gaps.plot}
\title{Plots Gap in Outcome Trajectories between the Treated its Synthetic Control Unit}
\description{ This function plots the gaps in the trajectories of the outcome variable for the treated unit and the 
  synthetic control group constructed by \code{\link{synth}} and \code{\link{dataprep}}. The user can specify whether
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
\references{

Abadie, A., Diamond, A., Hainmueller, J. (2014). Comparative Politics and the Synthetic Control Method. \emph{American Journal of Political Science} Forthcoming 2014.

Synthetic : An R Package for Synthetic Control Methods in Comparative Case Studies. \emph{Journal of Statistical Software} 42 (13) 1--17.
       
Abadie, A., Diamond, A., Hainmueller, J. (2011). Synth: An R Package for Synthetic Control Methods in Comparative Case Studies. \emph{Journal of Statistical Software} 42 (13) 1--17.

Abadie A, Diamond A, Hainmueller J (2010). Synthetic Control Methods for Comparative Case Studies: Estimating the Effect of California's Tobacco Control Program. \emph{Journal of the American Statistical Association} 105 (490) 493--505.
     
Abadie, A. and Gardeazabal, J. (2003) Economic Costs of Conflict: A Case Study of the Basque Country \emph{American Economic Review} 93 (1) 113--132.
  
            }
   
\author{Jens Hainmueller and Alexis Diamond}
\seealso{\code{\link{dataprep}}, \code{\link{synth}}, \code{\link{path.plot}}, \code{\link{synth.tab}}
}

