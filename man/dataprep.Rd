\name{dataprep}
\alias{dataprep}
\title{Extracts Matrices from Panel Dataset to be Loaded into synth()}
\description{
             Function that takes a standard panel dataset and produces 
	     data objects necessary for running \code{\link{synth}}
	     and other "Synth" package functions to construct
           synthetic control groups according to the methods outlined in 
           Abadie and Gardeazabal (2003) and Abadie, Diamond, Hainmueller (2007) (see references and example).

	     User supplies a dataframe ("foo"), chooses predictors,
	     special predictors (explained below), the operators that
	     act upon these predictors, the dependent variable,
	     identifies the columns associated with unit numbers,
	     time periods (and unit names, when available),
	     as well as the treated unit, the control units,
	     the time-period over which to select the predictors,
	     the time-period over which to optimize,
	     and the time-period over which outcome data should be plotted.
	     
	     The output of \code{dataprep} contains  list of matrixes to 
	     be loaded into \code{\link{synth}}.
  
}
\usage{dataprep(foo = NULL, predictors = NULL,
          predictors.op = "mean", special.predictors = NA,
          dependent = NULL, unit.variable = NULL,
          time.variable = NULL, treatment.identifier = NULL,
          controls.identifier = NULL, time.predictors.prior = NULL,
          time.optimize.ssr = NULL, y.plot = time.optimize.ssr,
          unit.names.variable = NA)
}

\arguments{
  \item{foo}{The dataframe with the panel data.}

  \item{predictors}{A vector of column numbers or column-name character
    strings that identifies the predictors' columns.}
  \item{predictors.op}{A character string identifying the method (operator) to be used
    on the predictors.  Default is "mean". rm.na = T is hardwired into
    the code. See *Details*.}
  \item{special.predictors}{A list object identifying additional predictors and
    their associated pre-treatment years and operators (analagous to
    ``predictors.op'' above).  See *Details*.}
  \item{dependent}{A scalar identifying the column number or column-name character string
    that corresponds to the dependent (outcome) variable.}
  \item{unit.variable}{A scalar identifying the column number or column-name character string
    associated unit numbers.}
  \item{time.variable}{A scalar identifying column number or column-name character string
    associated with period (time) data.} 
  \item{treatment.identifier}{A scalar identifying the ``unit.variable'' number or a character string
    giving the ``unit.name ''of the treated unit.}
  \item{controls.identifier }{ A scalar identifying the ``unit.variable'' numbers or a vector of
    character strings giving the ``unit.name''s of control units.}
  \item{time.predictors.prior }{A vector identifying the pretreatment years for predictors.}
  \item{time.optimize.ssr}{A vector identifying the years of the dependent variable over which the loss function 
should be minimized (i.e. the years over which the sum of squared residuals between treated and the syntehtic conrol unit are minimzed.}
  \item{y.plot}{A vector identifying the years over which results are to be plotted
    with \code{\link{gaps.plot}} and \code{\link{path.plot}}.}
  \item{unit.names.variable}{A scalar or column-name character string identifying the column with the names of the units.
  }
}
\details{ The predictors.op argument is a character string that provides a
  function (eg., "mean", "median", etc.) identifying the name of the
  operator to be applied to the predictors over the given time period. 

  The special.predictors argument is a list object that contains one or
  more lists of length = 3.  The components of each of these lists are:

  (a) scalar column number associated with that predictor
  (b) vector of time-period number(s) desired (eg., 1998:2003)
  (c) character-string identifying the name of the operation 
  to be applied (ie., "mean", "median", etc.)

  eg.,
  special.predictors <- list(listc(x1, 1990:2000, "mean"),
                             listc(x2, 1980:2000, "median")
                             )
}
\value{ 
  \item{X0}{matrix of controls' predictor data. nrows = number of predictors & special predictors.
    ncols = number of control units
  }

  \item{X1}{vector of treated predictor data.
    length = number of predictors & special predictors
  }

  \item{Z0}{matrix of controls' outcome data.
    nrows = number of pre-treatment years
    ncols = number of control units
  }

  \item{Z1}{vector of treated predictor data.
    length = number of pre-treatment years
  }

  \item{Y0plot}{matrix of outcomes for control units
  }

  \item{Y1plot}{vector of outcomes for treated unit
  }

  \item{names.and.numbers}{dataframe with two columns showing all unit numbers and
    corresponding unit names.
  }
  \item{tag}{a list of all arguments in initial function call.}
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

## Below we provide two examples of how to use synth

## First example using artifcial data:

## First, load the dataframe synth.data
## this dataframe has
## 8 units: 
## 1 treated, 6 control (2,7,13,17,29,32,38), treated is no 7
## 3 predictors (X1, X2, X3)
## 21 time periods (1980 - 2000)
## a unit.names.variable column ("names"
## and an outcome variable column (Y)
## all columns have column names

data(synth.data)

## run dataprep using character strings
## imagine treatment took place in 1991.
## as predictors, we want the average values of
## X1, X2, X3 over 1984 to 1989.
## and we want to track our synthetic control unit
## to track the treated unit's 1980-1991 outcomes
## as well as possible.

## we want to set it up to plot outcomes from 1984-1996

 dataprep.out <-
                  dataprep(
                           foo = synth.data,
                           predictors = c("X1", "X2", "X3"),
                           predictors.op = "mean",
                           dependent = "Y",
                           unit.variable = "unit.num",
                           time.variable = "year",
                           special.predictors = list(
                                                    list("Y", 1991, "mean"),
                                                    list("Y", 1985, "mean"),
                                                    list("Y", 1980, "mean")
                                                    ),
                           treatment.identifier = 7,
                           controls.identifier = c(29, 2, 13, 17, 32, 38),
                           time.predictors.prior = c(1984:1989),
                           time.optimize.ssr = c(1984:1990),
                           unit.names.variable = "name",
                           y.plot = 1984:1996
                           )


## then we run the synth command to identify optimal
## weights-- the optimal way to aggregate information about all the
## control units in such a way as to create the best possiblesynthetic
## control unit for the treated.
synth.out <- synth(dataprep.out,method = "BFGS")

## then we want to summarize the results, both numerically and graphically
## to get summary tables for all information 
## (V and W weights plus balance btw. treated and synthtic control) use the 
## synth.tab() command
synth.tables <- synth.tab(dataprep.res = dataprep.out,synth.res = synth.out)
print(synth.tables)

## to get summary plots for outcome trajectories 
## of the treated and the synthtic control unit use the 
## path.plot() and the gaps.plot() commands

## plot in levels (treated and synthetic)
path.plot(dataprep.res = dataprep.out,synth.res = synth.out)

## plot the gaps (treated - synthetic)
gaps.plot(dataprep.res = dataprep.out,synth.res = synth.out)

## Second example: The economic impact of terrorism in the
## Basque country using data from Abadie and Gardeazabal (2003)
## see the paper for details

data(basque)

# dataprep: prepare data for synth

dataprep.out <-
                  dataprep(
                            foo = basque
                           ,predictors    = c(
                                               "school.illit","school.prim","school.med","school.high","school.post.high" 
                                               ,"invest"
                                               )
                           ,predictors.op = c("mean")
                           ,dependent     = c("gdpcap")
                           ,unit.variable = c("regionno")
                           ,time.variable = c("year")
                           ,special.predictors = list(
                                                      list("gdpcap",           1960:1969,       c("mean")),                            
                                                      list("sec.agriculture",         seq(1961,1969,2),c("mean")),
                                                      list("sec.energy",        seq(1961,1969,2),c("mean")),
                                                      list("sec.industry",          seq(1961,1969,2),c("mean")),
                                                      list("sec.construction",       seq(1961,1969,2),c("mean")),                                                      
                                                      list("sec.services.venta",   seq(1961,1969,2),c("mean")),
                                                      list("sec.services.nonventa",seq(1961,1969,2),c("mean")), 
                                                      list("popdens",             1969,            c("mean"))
                                                      )
                           ,treatment.identifier  = 17
                           ,controls.identifier   = c(2:16,18)
                           ,time.predictors.prior = c(1964:1969)
                           ,time.optimize.ssr     = c(1960:1969)
                           ,unit.names.variable   = c("regionname")
                           ,y.plot                = c(1955:1997) 
                           )

# 1. combine highest and second highest schooling category and eliminate highest category
dataprep.out$X1["school.high",] <- dataprep.out$X1["school.high",] + dataprep.out$X1["school.post.high",]
dataprep.out$X1                 <- as.matrix(dataprep.out$X1[-which(rownames(dataprep.out$X1)=="school.post.high"),])
dataprep.out$X0["school.high",] <- dataprep.out$X0["school.high",] + dataprep.out$X0["school.post.high",]
dataprep.out$X0                 <- dataprep.out$X0[-which(rownames(dataprep.out$X0)=="school.post.high"),]

# 2. make total and compute shares for the schooling catgeories
lowest  <- which(rownames(dataprep.out$X0)=="school.illit")
highest <- which(rownames(dataprep.out$X0)=="school.high")

dataprep.out$X1[lowest:highest,] <- (100 * dataprep.out$X1[lowest:highest,]) / sum(dataprep.out$X1[lowest:highest,])
dataprep.out$X0[lowest:highest,] <-  100 * scale(
                                                 dataprep.out$X0[lowest:highest,],
                                                 center=FALSE,
                                                 scale=colSums(dataprep.out$X0[lowest:highest,])
                                                 )
    

# run synth
synth.out <- synth(
                   data.prep.obj = dataprep.out,                                 
                   method = "BFGS"
                   )

# Get result tables
synth.tables <- synth.tab(
                          dataprep.res = dataprep.out,
                          synth.res = synth.out
                          ) 

# results tables:
print(synth.tables)

# plot results:
# path
path.plot(synth.res = synth.out,
          dataprep.res = dataprep.out,
          Ylab = c("real per-capita GDP (1986 USD, thousand)"),
          Xlab = c("year"), 
          Ylim = c(0,13), 
          Legend = c("Basque country","synthetic Basque country"),
          ) 

## gaps
gaps.plot(synth.res = synth.out,
          dataprep.res = dataprep.out, 
          Ylab = c("gap in real per-capita GDP (1986 USD, thousand)"),
          Xlab = c("year"), 
          Ylim = c(-1.5,1.5), 
          ) 

}
\keyword{methods}
