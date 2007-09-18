.First.lib <-
function (libname, pkgname)
{
   # figure out year automatically (probably could be done more elegantly)
   date <- date()
   x <- regexpr("[0-9]{4}", date)
   this.year <- substr(date, x[1], x[1] + attr(x, "match.length") - 1)
   
   # echo output to screen
   cat("##\n## Synth Package: Implements Synthetic Control Methods.\n")
   cat("## Copyright (C) 2005-", this.year,
      " Alberto Abadie, Alexis J. Diamond, and Jens Hainmueller\n", sep="")
   cat("\n")
   cat("## See http://www.people.fas.harvard.edu/~jhainm/software.htm for additional information.\n\n")
   require(kernlab,quietly=TRUE)
#library.dynam(pkgname, pkgname, lib.loc=libname)
}

