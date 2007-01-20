\name{synth}
\alias{synth}
\title{ Constructs Synthetic Control Unit }
\description{
  This function constructs Synthetic Control Groups according to the methods outlined in 
  Abadie and Gardeazabal (2003) and Abadie, Diamond, Hainmueller (2007).

  The main input is the object list that results from calling \code{\link{dataprep}}. 
  Accordingly, a usual sequence of commands to implement the synthetic control method is to first call
   \code{\link{dataprep}} to prepare the data to be loaded into  \code{\link{synth}}. Then 
 \code{\link{synth}} is called to run the synthetic control algorithm. Finally, results are 
 obtained using \code{\link{synth.tab}}, \code{\link{path.plot}}, or \code{\link{gaps.plot}}.
  An example of this sequence is provided in the documentation to \code{\link{dataprep}}.

Notice that by first running \code{\link{dataprep}} the user creates a data.prep.obj, which is
an object list that contains all essential data information to run \code{synth}. This procedure 
is strongly recommended. Alternativley, the user may provide his own preprocessed data matrices 
and load them into synth via the X0, X1, Z1, and Z0 options. In this case, no data.prep.obj 
should be specifcied.

As proposed in Abadie and Gardeazabal (2003) and Abadie, Diamond, Hainmueller (2007),
the \code{\link{synth}} function routinley searches for the set of weights that generate the 
best fitting convex combination of the control units such that the path of outcomes 
for the treated unit prior to treatment is as close as possible to 
the path for the synthetic control unit for that same 
period. In other words, V is chosen among all positive 
definite diagonal matrices such that mean squared prediction error 
of the outcome variable is minimized for the pre-intervention 
period. 

Instead of using this data-driven procedures to search for the best fitting
synthtic control group, the
user may supply his own V weights, based on his subjective assessment of the predictive power of the variables in 
$X_{1}$ and $X_{0}$. In this case, the vector of V weights for each variable should be
supplied via the custom.V option in \code{\link{synth}} and the optimization over the V matrices is bypassed.

The output from \code{\link{synth}} is a list containing the weights on predictors (solution.V) 
and weights on control units (solution.W) that define contributions 
to the synthetic control unit. 

}
\usage{
synth(data.prep.obj = NA, X1 = stop("X0 missing"), X0 = stop("X1 missing"), 
Z0 = stop("Z0 missing"), Z1 = stop("Z1 missing"), 
custom.v = FALSE, Margin.ipop = 5e-04, Sigf.ipop = 4, Bound.ipop = 10, ...)
}
\arguments{
  \item{data.prep.obj}{the object that comes from running \code{\link{dataprep}}. This object contains all
  information about X0, X1, Z1, and Z0. Therefore, if data.prep.obj is supplied, none of X0, X1, Z1, and Z0 
  should be manually specified!}
  \item{X1}{vector of treated predictor data, length = number of predictors and
    special predictors.}
  \item{X0}{matrix of controls' predictor data. 
		    nrows = number of predictors & special predictors
		    ncols = number of control units. }
  \item{Z0}{matrix of controls' outcome data.
		    nrows = number of pre-treatment years
		    ncols = number of control units. }
  \item{Z1}{vector of treated predictor data.
    length = number of pre-treatment years .}
  \item{custom.v}{vector of weights for predictors supplied by the
    user. uses \code{synth} to bypass optimization for solution.V. }
   \item{Margin.ipop}{Settings for Quadratic Programming Solver ipop(): Margin for contraint violation tolerance. See ?ipop for details}
  \item{Sigf.ipop}{Settings for Quadratic Programming Solver ipop(): Precision (no of significant figures). See ?ipop for details.}
  \item{Bound.ipop}{Settings for Quadratic Programming Solver ipop(): Clipping bound for the variables. See ?ipop for details.}
\item{\dots}{Additional arguments to be passed to \code{optim} to adjust optimization}
}
\details{ Please consult the papers in the reference section for detailed information on 
the exact algorythm used to construct synthetic control groups. }
\value{
  \item{solution.V }{  vector of predictor weights.}
  \item{solution.W }{  vector of weights across the controls.}
  \item{loss.v }{  Loss.v.}
  \item{loss.w }{  Loss.w.}
  \item{custom.V }{  if this was specified in the call to synth, this
    outputs the weight vector specified.}
  \item{rgV.optim}{Results from optim() minimization. Could be used for diagnostics.}

}

\references{Abadie, A. and Gardeazabal, J. (2003) Economic Costs of Conflict: A Case Study of the Basque Country 
            \emph{American Economic Review} 93 (1) 113--132 
             \url{http://ksghome.harvard.edu/~.aabadie.academic.ksg/ecc.pdf}
           
            Abadie, A., Diamond, A., Hainmueller, J. (2007) Synthetic Control Methods
            for Comparative Case Studies: Estimating the Effect of California's Tobacco Control Program
            \emph{NBER Technical Working Paper no 335}
            \url{http://www.people.fas.harvard.edu/~jhainm/}
            }
   
\author{Alexis Diamond \email{adiamond@fas.harvard.edu} 
        Jens Hainmueller \email{jhainm@fas.harvard.edu}}
\note{}
\seealso{ \code{gaps.plot},\code{synth.plot},\code{dataprep},\code{synth.tab} }
\examples{##
}
\keyword{methods}


