\name{dataprep}
\alias{dataprep}
\title{Constructs a list of matrices from panel dataset to be loaded into synth()}
\description{
The \code{\link{synth}} function takes a standard panel dataset and produces a list of data objects necessary for running \code{\link{synth}} and other Synth package functions to construct synthetic control groups according to the methods outlined in Abadie and Gardeazabal (2003) and Abadie, Diamond, Hainmueller (2010, 2011, 2014) (see references and example). 

User supplies a dataframe ("foo"), chooses predictors, special predictors (explained below), the operators that act upon these predictors, the dependent variable, identifies the columns associated with unit numbers, time periods (and unit names, when available), as well as the treated unit, the control units, the time-period over which to select the predictors, the time-period over which to optimize, and the time-period over which outcome data should be plotted.

The output of \code{\link{dataprep}} contains a list of matrices. This list object can be directly loaded into \code{\link{synth}}. 
}
\usage{dataprep(foo = NULL, predictors = NULL,
          predictors.op = "mean", special.predictors = NULL,
          dependent = NULL, unit.variable = NULL,
          time.variable = NULL, treatment.identifier = NULL,
          controls.identifier = NULL, time.predictors.prior = NULL,
          time.optimize.ssr = NULL, time.plot = time.optimize.ssr,
          unit.names.variable = NA)
}

\arguments{
  \item{foo}{The dataframe with the panel data.}

  \item{predictors}{A vector of column numbers or column-name character
    strings that identifies the predictors' columns. All predictors have to be numeric.}
  \item{predictors.op}{A character string identifying the method (operator) to be used
    on the predictors.  Default is "mean". rm.na = T is hardwired into
    the code. See *Details*.}
  \item{special.predictors}{A list object identifying additional numeric predictors and their associated pre-treatment years and operators (analogous to
    ``predictors.op'' above).  See *Details*.}
  \item{dependent}{A scalar identifying the column number or column-name character string that corresponds to the numeric dependent (outcome) variable.}
  \item{unit.variable}{A scalar identifying the column number or column-name character string associated unit numbers. The unit.varibale has to be numeric.}
  \item{time.variable}{A scalar identifying column number or column-name character string associated with period (time) data. The time variable has to be numeric.} 
  \item{treatment.identifier}{A scalar identifying the ``unit.variable'' number or a character string
    giving the ``unit.name ''of the treated unit. If a character is supplied, a unit.names.variable also has to be supplied to identify the treated unit.}
  \item{controls.identifier }{ A scalar identifying the ``unit.variable'' numbers or a vector of character strings giving the ``unit.name''s of control units. If a character is supplied, a unit.names.variable also has to be supplied to identify the control units unit.}
  \item{time.predictors.prior }{A numeric vector identifying the pretreatment periods over which the values for the outcome predictors should be averaged.}
  \item{time.optimize.ssr}{A numeric vector identifying the periods of the dependent variable over which the loss function 
should be minimized (i.e. the periods over which mean squared prediction error (MSPE) , that is the sum of squared residuals between treated and the synthetic control unit, are minimized.}
  \item{time.plot}{A vector identifying the periods over which results are to be plotted
    with \code{\link{gaps.plot}} and \code{\link{path.plot}}.}
  \item{unit.names.variable}{A scalar or column-name character string identifying the column with the names of the units. This variable has to be of mode character. 
  }
}
\details{ The predictors.op argument is a character string that provides a
  function (eg., "mean", "median", etc.) identifying the name of the
  operator to be applied to the predictors over the given time period. 

  The special.predictors argument is a list object that contains one or
  more lists of length = 3.  The required components of each of these lists are:

  (a) scalar column number associated with that predictor
  (b) vector of time-period number(s) desired (eg., 1998:2003)
  (c) character-string identifying the name of the operation 
  to be applied (ie., "mean", "median", etc.)

  eg.,
  special.predictors <- list(listc(x1, 1990:2000, "mean"),
                             listc(x2, 1980:1983, "median"),
                             listc(x3, 1980, "mean")   
                             )
  
  indicates that predictor x1, should be used with its values averaged over periods 1990:2000; 
predicator x2 should be used with its median values over periods 1980:1983; x3 should be used with the values from period 1980 only.

}
\value{ 

  \item{X1}{matrix of treated predictor data.
    nrows = number of predictors and (possibly) special predictors. ncols = one.
  }

  \item{X0}{matrix of controls' predictor data. nrows = number of predictors and (possibly) special predictors. ncols = number of control units.
  }


  \item{Z1}{matrix of treated outcome data for the pre-treatment periods over which MSPE is to be minimized. nrows = number of pre-treatment periods. ncols = one.
  }

  \item{Z0}{matrix of controls' outcome data for the pre-treatment periods over which MSPE is to be minimized.
    nrows = number of pre-treatment periods.
    ncols = number of control units.
  }

  \item{Y1plot}{matrix of outcome data for treated unit to be used for results plotting. nrows = number of periods. ncols = one.
  }

  \item{Y0plot}{matrix of outcome data for control units to be used for results plotting. nrows = number of periods. ncols = number of control units. 
  }


  \item{names.and.numbers}{dataframe with two columns showing all unit numbers and
    corresponding unit names.
  }
  \item{tag}{a list of all arguments in initial function call.}
}
\references{

Abadie, A., Diamond, A., Hainmueller, J. (2014). Comparative Politics and the Synthetic Control Method. \emph{American Journal of Political Science} Forthcoming 2014.

Synthetic : An R Package for Synthetic Control Methods in Comparative Case Studies. \emph{Journal of Statistical Software} 42 (13) 1--17.
       
Abadie, A., Diamond, A., Hainmueller, J. (2011). Synth: An R Package for Synthetic Control Methods in Comparative Case Studies. \emph{Journal of Statistical Software} 42 (13) 1--17.

Abadie A, Diamond A, Hainmueller J (2010). Synthetic Control Methods for Comparative Case Studies: Estimating the Effect of California's Tobacco Control Program. \emph{Journal of the American Statistical Association} 105 (490) 493--505.
     
Abadie, A. and Gardeazabal, J. (2003) Economic Costs of Conflict: A Case Study of the Basque Country \emph{American Economic Review} 93 (1) 113--132.
  
}
   
\author{Jens Hainmueller and Alexis Diamond}
\seealso{ \code{\link{synth}}, \code{\link{gaps.plot}}, \code{\link{path.plot}}, \code{\link{synth.tab}} 
}
\examples{
\donttest{
## The usual sequence of commands is:
## 1. dataprep() for matrix-extraction
## 2. synth() for the construction of the synthetic control group
## 3. synth.tab(), gaps.plot(), and path.plot() to summarize the results
## Below we provide two examples.

## First Example: Toy panel dataset

# load data
data(synth.data)

# create matrices from panel data that provide inputs for synth()
dataprep.out<-
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
   time.plot = 1984:1996
   )

## run the synth command to identify the weights
## that create the best possible synthetic 
## control unit for the treated.
synth.out <- synth(dataprep.out)

## there are two ways to summarize the results
## we can either access the output from synth.out directly
round(synth.out$solution.w,2)
# contains the unit weights or
synth.out$solution.v 
## contains the predictor weights. 

## the output from synth opt 
## can be flexibly combined with 
## the output from dataprep to 
## compute other quantities of interest
## for example, the period by period 
## discrepancies between the 
## treated unit and its synthetic control unit
## can be computed by typing
gaps<- dataprep.out$Y1plot-(
        dataprep.out$Y0plot\%*\%synth.out$solution.w
        ) ; gaps

## also there are three convenience functions to summarize results.
## to get summary tables for all information 
## (V and W weights plus balance btw. 
## treated and synthetic control) use the 
## synth.tab() command
synth.tables <- synth.tab(
      dataprep.res = dataprep.out,
      synth.res = synth.out)
print(synth.tables)

## to get summary plots for outcome trajectories 
## of the treated and the synthetic control unit use the 
## path.plot() and the gaps.plot() commands

## plot in levels (treated and synthetic)
path.plot(dataprep.res = dataprep.out,synth.res = synth.out)

## plot the gaps (treated - synthetic)
gaps.plot(dataprep.res = dataprep.out,synth.res = synth.out)

}
\donttest{
## Second example: The economic impact of terrorism in the
## Basque country using data from Abadie and Gardeazabal (2003)
## see JSS paper in the references details

data(basque)

# dataprep: prepare data for synth
dataprep.out <-
  dataprep(
  foo = basque
  ,predictors= c("school.illit",
                 "school.prim",
                 "school.med",
                 "school.high",
                 "school.post.high"
                 ,"invest"
                 )
   ,predictors.op = c("mean")
   ,dependent     = c("gdpcap")
   ,unit.variable = c("regionno")
   ,time.variable = c("year")
   ,special.predictors = list(
    list("gdpcap",1960:1969,c("mean")),                            
    list("sec.agriculture",seq(1961,1969,2),c("mean")),
    list("sec.energy",seq(1961,1969,2),c("mean")),
    list("sec.industry",seq(1961,1969,2),c("mean")),
    list("sec.construction",seq(1961,1969,2),c("mean")),
    list("sec.services.venta",seq(1961,1969,2),c("mean")),
    list("sec.services.nonventa",seq(1961,1969,2),c("mean")),
    list("popdens",1969,c("mean")))
    ,treatment.identifier  = 17
    ,controls.identifier   = c(2:16,18)
    ,time.predictors.prior = c(1964:1969)
    ,time.optimize.ssr     = c(1960:1969)
    ,unit.names.variable   = c("regionname")
    ,time.plot            = c(1955:1997) 
    )

# 1. combine highest and second highest 
# schooling category and eliminate highest category
dataprep.out$X1["school.high",] <- 
 dataprep.out$X1["school.high",] + 
 dataprep.out$X1["school.post.high",]
dataprep.out$X1                 <- 
 as.matrix(dataprep.out$X1[
  -which(rownames(dataprep.out$X1)=="school.post.high"),])
dataprep.out$X0["school.high",] <- 
 dataprep.out$X0["school.high",] + 
 dataprep.out$X0["school.post.high",]
dataprep.out$X0                 <- 
dataprep.out$X0[
 -which(rownames(dataprep.out$X0)=="school.post.high"),]

# 2. make total and compute shares for the schooling catgeories
lowest  <- which(rownames(dataprep.out$X0)=="school.illit")
highest <- which(rownames(dataprep.out$X0)=="school.high")

dataprep.out$X1[lowest:highest,] <- 
 (100 * dataprep.out$X1[lowest:highest,]) /
 sum(dataprep.out$X1[lowest:highest,])
dataprep.out$X0[lowest:highest,] <-  
 100 * scale(dataprep.out$X0[lowest:highest,],
             center=FALSE,
             scale=colSums(dataprep.out$X0[lowest:highest,])
                                                 )
    
# run synth
synth.out <- synth(data.prep.obj = dataprep.out)

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
          
## To create the placebo studies simply reassign
## the intervention to other units or times (see references for details)
}
}

