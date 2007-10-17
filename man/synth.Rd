\name{synth}
\alias{synth}
\title{ Constructs synthetic control units for comparative case studies}
\description{
  The function \code{\link{synth}} implements the synthetic control method for causal inference in comparative case studies as developed in Abadie and Gardeazabal (2003) and Abadie, Diamond, Hainmueller (2007). \code{\link{synth}} estimates the effect of an intervention of interest by comparing
the evolution of an aggregate outcome for a unit affected by the intervention to the evolution of the same aggregate outcome for a synthetic control group.

    \code{\link{synth}} constructs this synthetic control group by searching for a weighted combination of control units chosen to approximate the unit affected by the intervention in terms of the outcome predictors. The evolution of the outcome for the resulting synthetic control group is an estimate of the counterfactual of what would have been observed for the affected unit in the absence of the intervention. 

\code{\link{synth}} can also be used to conduct a variety of placebo and permutation tests that produce
    informative inference regardless of the number of available comparison units and the number of available time-periods. See Abadie and Gardeazabal (2003) and Abadie,
    Diamond, and Hainmueller (2007) for details.

\code{\link{synth}} requires the user to supply four matrices as its main arguments. These matrices are named X0, X1, Z1, and Z0 accordingly. X1 and X0 contain the predictor values for the treated unit and the control units respectively. Z1 and Z0 contain the outcome variable for the pre-intervention period for the treated unit and the control unit respectively. The pre-intervention period refers to the time period prior to the intervention, over which the mean squared prediction error (MSPE) should be minimized. The MSPE refers to the squared deviations between the outcome for the treated unit and the
synthetic control unit summed over all pre-intervention periods specified in Z1 and Z0.

Creating the matrices X1, X0, Z1, and Z0 from a (panel) dataset can be tedious. Therefore the \code{Synth} library offers a preparatory function called \code{\link{dataprep}}, that allows the user to easily create all inputs required for \code{\link{synth}}. By first calling \code{\link{dataprep}} the user creates a single list object called \code{data.prep.obj} that contains all essential data information to run \code{synth}. 

Accordingly, a usual sequence of commands to implement the synthetic control method is to first call
   \code{\link{dataprep}} to prepare the data to be loaded into  \code{\link{synth}}. Then 
 \code{\link{synth}} is called to construct the synthetic control group. Finally, results are 
 easily summarized using the functions \code{\link{synth.tab}}, \code{\link{path.plot}}, or \code{\link{gaps.plot}}.

 An example of this sequence is provided in the documentation to \code{\link{dataprep}}. This procedure is strongly recommended. Alternatively, the user may provide his own preprocessed data matrices and load them into \code{\link{synth}} via the X0, X1, Z1, and Z0 arguments. In this case, no data.prep.obj should be specified.

As proposed in Abadie and Gardeazabal (2003) and Abadie, Diamond, Hainmueller (2007), the \code{\link{synth}} function routinely searches for the set of weights that generate the  best fitting convex combination of the control units. In other words, the predictor weight matrix V is chosen among all positive definite diagonal matrices such that MSPE is minimized for the pre-intervention period. 

Instead of using this data-driven procedures to search for the best fitting synthetic control group, the
user may supply his own V weights, based on his subjective assessment of the predictive power of the variables in X1 and X0. In this case, the vector of V weights for each variable should be
supplied via the custom.V option in \code{\link{synth}} and the optimization over the V matrices is bypassed.

The output from \code{\link{synth}} is a list object that contains the weights on predictors (solution.V) 
and weights on control units (solution.W) that define contributions 
to the synthetic control unit. 

}
\usage{
synth(data.prep.obj = NULL, X1 = NULL, X0 = NULL, 
Z0 = NULL, Z1 = NULL, 
custom.v = FALSE, Margin.ipop = 0.0005, Sigf.ipop = 5, Bound.ipop = 10,genoud = FALSE, ...)
}
\arguments{
  \item{data.prep.obj}{the object that comes from running \code{\link{dataprep}}. This object contains all
  information about X0, X1, Z1, and Z0. Therefore, if data.prep.obj is supplied, none of X0, X1, Z1, and Z0 
  should be manually specified!}
  \item{X1}{matrix of treated predictor data, nrows = number of predictors ncols = ones.}
  \item{X0}{matrix of controls' predictor data. 
		    nrows = number of predictors.
		    ncols = number of control units (>=2). }
  \item{Z1}{matrix of treated outcome data for the pre-treatment periods over which MSPE is to be minimized.
    nrows = number of pre-treatment periods. ncols = 1.}
  \item{Z0}{matrix of controls' outcome data for the pre-treatment periods over which MSPE is to be minimized. nrows = number of pre-treatment periods.
		    ncols = number of control units.}
  \item{custom.v}{vector of weights for predictors supplied by the
    user. uses \code{synth} to bypass optimization for solution.V. }
   \item{Margin.ipop}{Settings for Quadratic Programming Solver ipop(): Margin for constraint violation tolerance. See ?ipop for details}
  \item{Sigf.ipop}{Settings for Quadratic Programming Solver ipop(): Precision (no of significant figures). See ?ipop for details.}
  \item{Bound.ipop}{Settings for Quadratic Programming Solver ipop(): Clipping bound for the variables. See ?ipop for details.}
\item{genoud}{Logical flag. If true, synth embark on a two step optimization. In the first step, genoud(), a optimization function that combines evolutionary algorithm methods with a derivative-based (quasi-Newton) method to solve difficult optimization problems, is used to obtain a solution. See \code{genoud} for details. In the second step, the genoud results are passed to optim() for a local optimization within the neighboorhood of the genoud solution. This two step optimization procedure will require much more computing time, but may yield lower loss in cases where the search space is highly irregular.}
\item{\dots}{Additional arguments to be passed to \code{optim} and or \code{genoud} to adjust optimization}
}
\details{ Please also consult the papers in the reference section for detailed information on 
the algorithm used to construct synthetic control groups.}
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
\note{NA}
\seealso{ \code{gaps.plot},\code{synth.plot},\code{dataprep},\code{synth.tab} }
\examples{

## While synth() can be used to construct synthetic control groups
## direcly, by providing a X1,X0,Z1, and Z0 matrix. We strongly
## recommend to first run dataprep() to setup the data.
## Two extensive examples illustrating the whole sequence of
## of commands with:
## 1. dataprep() for matrix-extraction
## 2. synth() for the construction of the synthetic control group
## 3. synth.tab(), gaps.plot(), and path.plot() to summarize the results
## are provided in the examples for the dataprep() function
## please take a close look there.

## Here we provide a stand-alone example, for synth()
## that does not require the user to run dataprep first.
## This example is a generic version of the original estimation
## of economic impact of terrorism in the Basque country
## presented in Abadie and Gardeazabal (2003).
## See the paper for details and the example in dataprep() for the exact replication.
## Here we just use a subset of the predictors.
## Notice the various matrix extractions below;
## these are all taken care of by running dataprep() first.

# load data
data(basque)

# Construct matrix of predictors
predictors    = c(
                  "school.illit","school.prim","school.med","school.high",
                  "invest","gdpcap","popdens"
                  )

# for the treated unit (Basque country)
# (predictors are averaged for the 1964:1969 period)
X1 <-
 as.matrix(apply(
                 basque[
                        basque$regionname == "Basque Country (Pais Vasco)" &
                        is.element(basque$year,1964:1969),
                        predictors
                       ]
            ,2,mean,na.rm=TRUE))

# and the control units (other Spanish regions)
controls <- c(2:16,18)
X0 <- basque[
             is.element(basque$regionno,controls) &
             is.element(basque$year,1964:1969),
             c(predictors,"regionno")
             ]
X0 <- split(X0, X0[,dim(X0)[2]])
X0 <- sapply(X0, apply, 2, mean, na.rm = TRUE)
X0 <- as.matrix(X0[-dim(X0)[1],])

# get matrix of pre-intervention values of the outcome variable (GDP)
# over which mean squared prediction error should be minimized
# treated unit
Z1 <-
  matrix(basque$gdpcap[
                       is.element(basque$year,1960:1969) &
                       basque$regionname == "Basque Country (Pais Vasco)"
                      ])

Z0 <-
  matrix(basque$gdpcap[
                       is.element(basque$year,1960:1969) &
                       is.element(basque$regionno,controls)
                      ],ncol=length(controls))
                      
rownames(Z0) <- rownames(Z1) <- 1960:1969
colnames(Z0) <- colnames(X0) <- controls

# now construct a synthetic basque country
synth.out <- synth(X1=X1,X0=X0,Z1=Z1,Z0=Z0,method="BFGS")

# examine the weights associated with each of the control units
# with region_numbers
round(synth.out$solution.w,3)
# or regionnames
data.frame(round(synth.out$solution.w,3),row.names = unique(basque$regionname)[c(-1,-17)])

# compare the predictor values for the treated unit and its synthetic counterpart
tab <- cbind(X1,X0\%*\%synth.out$solution.w)
colnames(tab) <- c("Basque Country","Synthetic Basque country")
tab

# plot the pre-terrorism trajectory for the treated unit and its synthetic counterpart
# over with the mean squared prediction error was minimized
matplot(cbind(1960:1969,1960:1969),
        cbind(Z1,Z0\%*\%as.matrix(synth.out$solution.w)),type="l",xlab="year",ylab="GDPcap")
legend("topleft",legend=c("Basque","Synthetic Basque"),col=c("black","red"),lty=c(1,2))

# plot the whole pre and post terrorism period
Y1 <-
  matrix(basque$gdpcap[
                       is.element(basque$year,1955:1997) &
                       basque$regionname == "Basque Country (Pais Vasco)"
                      ])

Y0 <-
  matrix(basque$gdpcap[
                       is.element(basque$year,1955:1997) &
                       is.element(basque$regionno,controls)
                      ],ncol=length(controls))
matplot(cbind(1955:1997,1955:1997),
        cbind(Y1,Y0\%*\%as.matrix(synth.out$solution.w)),type="l",xlab="year",ylab="GDPcap",ylim=c(0,11.5))
legend("topleft",legend=c("Basque","Synthetic Basque"),col=c("black","red"),lty=c(1,2))

## to run placebo studies, simply re-run synth
## and change the treated unit ot time of intervention (see references for details)


}
\keyword{methods}


