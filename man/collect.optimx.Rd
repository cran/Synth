\name{collect.optimx}
\alias{collect.optimx}
\title{
Collect results from optimx optimization methods
}
\description{
An internal function that collects the results from the different optimization methods run by optimx. It stores the parameter and function values and extracts the results for the best performing method (minimum or maximum).
}
\usage{
collect.optimx(res, opt = "min")
}
\arguments{
  \item{res}{
Output from a call to optimx().
}
  \item{opt}{
Either "min" or "max" to extract results for he methods that obtained the minimum or maximum function value 
across the methods.
}
}

\value{
  \item{out.list}{Dataframe with results from the different methods.}
  \item{par}{Parameter values from method that attained minimum/maximum across the methods.}
  \item{value}{Function value from method that attained minimum/maximum across the methods.}
}

\author{
Jens Hainmueller
}

\seealso{
Also see \code{\link[optimx]{optimx}}.
}

