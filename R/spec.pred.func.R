spec.pred.func <- function(
                           list.object = NULL,
                           tr.numb = treatment.identifier,
                           co.numb = controls.identifier,
                           unit.var = unit.variable,
                           time.var = time.variable,
                           foo.object = foo,
                           X0.inner = X0,
                           X1.inner = X1
                           )
  {

    special.units.tr <-
      which(is.element(foo.object[,unit.var], tr.numb) == TRUE)

    special.units.co <-
      which(is.element(foo.object[,unit.var], co.numb) == TRUE)

    special.times <-
      which(is.element(foo.object[,time.var], list.object[[2]]) == TRUE)

    if(mode(list.object[[1]]) == "character")
      {
        list.object[[1]] <- which(names(foo.object) == list.object[[1]])
      }

    name.predictor <- names(foo.object)[list.object[[1]]]

    X1.special <- as.matrix(foo.object[
                                       intersect(
                                                 special.units.tr,
                                                 special.times
                                                ),
                                       list.object[[1]]
                                       ]
                            )

    row.names(X1.special) <-
      foo.object[
                 intersect(special.units.tr, special.times),
                 time.var
                 ]


    if(sum(is.na(X1.special)) > 0)
      {
        cat(
            "\n\n######################################################",
             "\nYou have missing data in treated special predictor vars!",
             "\nWe ignore (na.rm = TRUE) missingness when we perform operations",
             "\nYou may want to reexamine your data!\n"
             )
      }

    
    if(is.na(list.object[[3]]) == TRUE)
      {
        X1.inner <- rbind(X1.inner, X1.special)
      } else {
        X1.special <- apply(X1.special,
                            2,
                            paste(list.object[[3]]),
                            na.rm = TRUE
                            )
        
        X1.inner <- rbind(X1.inner, X1.special)
      }

    X0.special <-
      as.matrix(foo.object[intersect(
                                     special.units.co,
                                     special.times
                                     ),
                           c(list.object[[1]])
                           ]
                )

    X0.special <-
      matrix(X0.special[,1], byrow = FALSE, ncol = length(co.numb))

    # Define row and column names

    row.names(X0.special) <-
      foo.object[intersect(special.units.tr, special.times), time.var]

    colnames(X0.special) <- co.numb

    if(sum(is.na(X0.special)) > 0)
      {
        cat(
            "\n\n######################################################",
             "\nYou have missing data in control special predictor vars!",
             "\nWe ignore (na.rm = TRUE) missingness when we perform operations",
             "\nYou may want to reexamine your data!\n"
             )
      }

    # Continue with object creation           
    if(is.na(list.object[[3]]) == TRUE)

      {
        XO.inner <- rbind(X0.inner, X0.special)
      } else {
        X0.special <- apply(
                            X0.special,
                            2,
                            paste(list.object[[3]]),
                            na.rm = TRUE
                            )

        X0.special <- t(as.matrix(X0.special))
        X0.inner <- rbind(X0.inner,
                          X0.special
                          )
      }

    if(length(list.object[[2]])>1)
      {
      row.names(X1.inner)[length(row.names(X1.inner))] <-
      paste("special.", name.predictor, ".",
            paste(list.object[[2]][1],".",
                  list.object[[2]][length(list.object[[2]])]
                                  , sep = ""), sep = "")
      
      } else {    

      row.names(X1.inner)[length(row.names(X1.inner))] <-
      paste("special.", name.predictor, ".",
            paste(list.object[[2]]
            , sep = ""), sep = "")
      }
      
   if(length(list.object[[2]])>1)
      {
      row.names(X0.inner)[length(row.names(X0.inner))] <-
      paste("special.", name.predictor, ".",
            paste(list.object[[2]][1],".",
            list.object[[2]][length(list.object[[2]])]
            , sep = ""), sep = "")
      
      } else {

      row.names(X0.inner)[length(row.names(X0.inner))] <-
      paste("special.", name.predictor, ".",
            paste(list.object[[2]], sep = ""), sep = "")

      }
    
    # Prepare output
    special.output <- list(X0.inner = X0.inner,
                           X1.inner = X1.inner
                           )

    return(invisible(special.output))

  }
