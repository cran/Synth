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
\references{

Abadie, A., Diamond, A., Hainmueller, J. (2014). Comparative Politics and the Synthetic Control Method. \emph{American Journal of Political Science} Forthcoming 2014.

Synthetic : An R Package for Synthetic Control Methods in Comparative Case Studies. \emph{Journal of Statistical Software} 42 (13) 1--17.
       
Abadie, A., Diamond, A., Hainmueller, J. (2011). Synth: An R Package for Synthetic Control Methods in Comparative Case Studies. \emph{Journal of Statistical Software} 42 (13) 1--17.

Abadie A, Diamond A, Hainmueller J (2010). Synthetic Control Methods for Comparative Case Studies: Estimating the Effect of California's Tobacco Control Program. \emph{Journal of the American Statistical Association} 105 (490) 493--505.
     
Abadie, A. and Gardeazabal, J. (2003) Economic Costs of Conflict: A Case Study of the Basque Country \emph{American Economic Review} 93 (1) 113--132.
  
            }
   
\author{Jens Hainmueller and Alexis Dimaond}
\seealso{\code{\link{synth}}, \code{\link{dataprep}}, \code{\link{gaps.plot}}, \code{\link{path.plot}}
}

