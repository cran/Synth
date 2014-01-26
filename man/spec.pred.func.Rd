\name{spec.pred.func}
\alias{spec.pred.func}
\title{Special Predictor Function for Dataprep}
\description{ This function is called by \code{\link{dataprep}} to handle special predictors in the process of setting up the dataset to be loaded into \code{\link{synth}}. It should not be called manually by the normal user.
}
\usage{spec.pred.func(list.object = NULL,
          tr.numb = NULL,
          co.numb = NULL,
          unit.var = NULL,
          time.var = NULL,
          foo.object = NULL,
          X0.inner = NULL,
          X1.inner = NULL)
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
\references{

Abadie, A., Diamond, A., Hainmueller, J. (2014). Comparative Politics and the Synthetic Control Method. \emph{American Journal of Political Science} Forthcoming 2014.

Synthetic : An R Package for Synthetic Control Methods in Comparative Case Studies. \emph{Journal of Statistical Software} 42 (13) 1--17.
       
Abadie, A., Diamond, A., Hainmueller, J. (2011). Synth: An R Package for Synthetic Control Methods in Comparative Case Studies. \emph{Journal of Statistical Software} 42 (13) 1--17.

Abadie A, Diamond A, Hainmueller J (2010). Synthetic Control Methods for Comparative Case Studies: Estimating the Effect of California's Tobacco Control Program. \emph{Journal of the American Statistical Association} 105 (490) 493--505.
     
Abadie, A. and Gardeazabal, J. (2003) Economic Costs of Conflict: A Case Study of the Basque Country \emph{American Economic Review} 93 (1) 113--132.
  
  }
   
\author{Jens Hainmueller and Alexis Diamond}
\seealso{ \code{\link{synth}}, \code{\link{dataprep}}, \code{\link{gaps.plot}}, \code{\link{path.plot}}, \code{\link{synth.tab}} }

