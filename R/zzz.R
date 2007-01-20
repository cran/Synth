.onAttach <- function(...) {
 
   # figure out year automatically (probably could be done more elegantly)
   date <- date()
   x <- regexpr("[0-9]{4}", date)
   this.year <- substr(date, x[1], x[1] + attr(x, "match.length") - 1)
   
   # echo output to screen
   cat("##\n## Synth Package )\n")
   cat("## Copyright (C) 2005-", this.year,
      " Alberto Abadie, Alexis J. Diamond, and Jens Hainmueller\n", sep="")
   cat("\n")
   cat("## See Abadie and Gardeazabal (2003) and Abadie, Diamond, and Hainmueller (2007)\n\n")
   require(kernlab, quietly=TRUE)
}

.onUnload <- function(libpath) {
    library.dynam.unload("Synth", libpath)
}

