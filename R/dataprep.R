dataprep<-

  function(
                        foo = NULL,
                        predictors = NULL,
                        predictors.op = "mean",
                        special.predictors = NA,
                        dependent = NULL,
                        unit.variable = NULL,
                        time.variable = NULL,
                        treatment.identifier = NULL,
                        controls.identifier = NULL,
                        time.predictors.prior = NULL,
                        time.optimize.ssr = NULL,
                        y.plot = time.optimize.ssr,
                        unit.names.variable = NA
                        )
  
  {

    ##############################################################
    # identify the unit.variable and time.variable
    # if given by a character instead of a column number
    if(mode(unit.variable) == "character")
      {
       if(sum(which(names(foo) == unit.variable)) <= 0)
          {
           stop("\n Unit.variable not found in data.\n")
           }
       unit.variable <- which(names(foo) == unit.variable)
      }

    if(mode(time.variable) == "character")
      {
       if(sum(which(names(foo) == time.variable)) <= 0)
          {
           stop("\n Time.variable not found in data.\n")
           }    
       time.variable <- which(names(foo) == time.variable)
      }

    # identify the predictors if given by characters, not numbers
    if(mode(predictors) == "character")
      {
       if(length(which(is.element(names(foo), predictors))) != length(predictors))
          {
           stop("\n At least one predictor not found in data. Check predcitors vector.\n")
           }
       predictors <- which(is.element(names(foo), predictors))    
      }

    for(i in predictors)
      {
        if(mode(foo[,i]) == "character")
          {
            stop(
                "\n One of your predictors is a character! Make it numeric.\n"
                )
          }
        foo[,i] <- as.numeric(as.character(foo[,i]))
      }
        

    # identify special predictors in the same way
    for(i in 1:length(special.predictors))
      {
        if(sum(is.element(names(foo), special.predictors[[i]][1])) > 0)
          {
            special.predictors[[i]][1] <-
              which(names(foo) == special.predictors[[i]][1])
          } else { 
                  stop(
                       "\n","Your special predictor ", special.predictors[[i]][1]," is not found in the data",".\n"
                       ) 
          }
        }

    # if the unit.names.variable is given as a character, convt to colnumber
    if(mode(unit.names.variable) == "character")
      { 
        if(sum(which(names(foo) == unit.names.variable)) <= 0)
          {
            stop(
                "\n Your unit.names.variable is not found in data.\n"
                )
          }
        unit.names.variable <- which(names(foo) == unit.names.variable)
       }

    ########################################################################
    # sort first by unit, then by time variable

    foo[,time.variable] <- as.numeric(as.character(foo[,time.variable]))
    foo[,unit.variable] <- as.numeric(as.character(foo[,unit.variable]))
    foo <- foo[order(foo[,unit.variable], foo[,time.variable]),]

#    if(unique(table(foo[,unit.variable], foo[,time.variable])) != 1)
#      {
#        stop("Your panel is unbalanced (units, times). Balance units & times.")
#      }
      
      
    ########################################################################
    # If we have unit names,
    # construct a dataframe with unit names and corresponding numbers

    if(is.na(unit.names.variable) == TRUE) {
      names.and.numbers <-
        as.data.frame(
                      cbind(
                            rep(NA, length(unique(
                                                  foo[,unit.variable]
                                                  )
                                           )
                                ),
                            unique(
                                   as.numeric(foo[,unit.variable])
                                   )
                            )
                      )

      

      names(names.and.numbers) <- c(
                                    "unit.names",
                                    "unit.numbers"
                                    )
    } else {
      unit.names <-
        unique(
               as.character(foo[,unit.names.variable])
               )

      unit.numbers <- unique(
                             as.numeric(foo[,unit.variable])
                             )

      names.and.numbers <- as.data.frame(
                                         cbind(
                                               unit.names,
                                               unit.numbers
                                               )
                                         )

      names.and.numbers[,2] <- as.numeric(as.character(names.and.numbers[,2]))

      names(names.and.numbers) <- c(
                                    "unit.names",
                                    "unit.numbers"
                                    )

    }

    
    ################################################################
    # Identify important rows and cols (treated, control units, etc.)

    if(mode(treatment.identifier) == "character")
      {
        if(is.na(unit.names.variable) == TRUE)
          {stop
           ("\n Supply unit-names vect if tmt.identifier is a character! \n")}
        treatment.identifier <-
          which(names.and.numbers[,1] == treatment.identifier)
        treatment.identifier <- names.and.numbers[treatment.identifier,2]
      }

    if(mode(controls.identifier) == "character")
      {
        if(is.na(unit.names.variable) == TRUE)
          {stop
           ("\n Supply unit-names vector if controls.id is a character! \n")}
        controls.identifier <-
          is.element(names.and.numbers[,1], controls.identifier)
        controls.identifier <- names.and.numbers[controls.identifier,2]
      }


    # eliminate rows in names.and.numbers that we don't use
     all.units <- c(treatment.identifier, controls.identifier)

     names.and.numbers <- 
              names.and.numbers[is.element(names.and.numbers[,2], all.units),]
        
 
   # Get rows
    treatment.rows <- which(foo[,unit.variable] == treatment.identifier)
    
    if(length(treatment.rows) == 0)
      {
      stop("\nTreated unit not in the dataset!\n")
      }      
    
    control.rows <-
      which(is.element(foo[,unit.variable], controls.identifier) == TRUE)

    if(length(control.rows) == 0)
      {
      stop("\nControl units not not in the dataset!\n")
      }

    if(length(intersect(treatment.rows, control.rows))>0)
      {
      stop("\nTreated unit is among the controls!\n")
      }      


     if(unique(
                  table(
                        foo[
                            c(control.rows,treatment.rows)
                            ,unit.variable],
                        foo[
                            c(control.rows,treatment.rows)
                            ,time.variable])
                            ) != 1)
      {
        stop("Your panel is unbalanced (units, times). Balance units & times.")
      }
   
    time.predictors.prior.rows <-
      which(is.element(foo[,time.variable], time.predictors.prior) == TRUE)

    time.optimize.ssr.rows <-
      which(is.element(foo[,time.variable], time.optimize.ssr ) == TRUE)

    ############################################################
    # Predictors Pretreatment

    X1 <-
      as.data.frame(foo[
                        intersect(
                                  treatment.rows,
                                  time.predictors.prior.rows
                                  ),
                        predictors
                        ]
                    )

    if(sum(is.na(X1)) > 0)
      {
        cat(
            "\n\n#####################################################",
            "\nYou have missing data in pretmt TREATED predictors!",
            "\nWe ignore (na.rm = TRUE) all missing values for predictors.op.",
            "\nYou may want to reexamine your data!\n\n"
            )
      }

    X1 <- apply(X1, 2, paste(predictors.op), na.rm = TRUE)
    X1 <- as.matrix(X1)

    if(sum(is.na(X1)) > 0) {stop("\nX1 has NAs. Look at data.\n")}

    

    colnames(X1) <- treatment.identifier

    X0 <- as.data.frame(foo[intersect(control.rows,
                                      time.predictors.prior.rows
                                      ),
                            c(predictors, unit.variable)
                            ]
                        )

    if(sum(is.na(X0)) > 0)
      {
        cat(
            "\n\n#####################################################",
            "\nYou have missing data in pretmt CONTROL predictors!",
            "\nWe ignore (na.rm = TRUE) all missing values for predictors.op.",
            "\nYou may want to reexamine your data!\n\n"
            )
      }

    X0 <- split(X0, X0[,dim(X0)[2]])

    X0 <- sapply(X0, apply, 2, mean, na.rm = TRUE, simplify = TRUE)
    X0 <- as.matrix(X0[-dim(X0)[1],])

    if(sum(is.na(X0)) > 0) {stop("\nX1 has NAs. Look at data.\n")}

    ################################################################
    # Dependent Variable Pretreatment

    if(mode(dependent) == "character")
      {dependent <- which(is.element(names(foo), dependent))}
    foo[,dependent] <- as.numeric(as.character(foo[,dependent]))

    Z1 <- foo[
              intersect(
                        treatment.rows,
                        time.optimize.ssr.rows
                        ),
              dependent
              ]

    Z1 <- as.matrix(Z1)

    if(sum(is.na(Z1)) > 0)

      {
        stop(
            "\n\n#####################################################",
            "\nYou have missing data in TREATED outcome vars!\n\n"
             )            
      }

    rownames(Z1) <- time.optimize.ssr
    colnames(Z1) <- treatment.identifier

    Z0 <-
      as.matrix(foo[
                    intersect(
                              control.rows,
                              time.optimize.ssr.rows
                              ),
                    c(dependent)
                    ]
                )

    if(sum(is.na(Z1)) > 0)
      {
        stop(
            "\n\n#####################################################",
            "\nYou have missing data in CONTROL outcome vars!\n\n)"
            )
      }

    Z0 <- matrix(
                 Z0,
                 byrow = FALSE,
                 nrow = length(time.optimize.ssr),
                 ncol = length(controls.identifier),
                 )

    colnames(Z0)<- controls.identifier
    rownames(Z0)<- time.optimize.ssr

    ##########################################################
    # Dependent Variable Plot

        y.plot.rows <-
          which(is.element(foo[,time.variable], y.plot ) == TRUE)

        Y1plot <- foo[intersect(treatment.rows, y.plot.rows), dependent]
        Y1plot <- as.matrix(Y1plot)

        if(sum(is.na(Y1plot) > 0))
           {
             cat( 
                 "\n\n######################################################",
                 "\nYou have missing data in treated plotted outcome vars!\n\n")           }

        rownames(Y1plot) <- y.plot
        colnames(Y1plot) <- treatment.identifier

        Y0plot <- as.matrix(foo[
                                intersect(control.rows, y.plot.rows),
                                c(dependent)
                                ]
                            )

        if(sum(is.na(Y0plot) > 0))
           {
             cat(
                 "\n\n######################################################",
                 "\nYou have missing data in control plotted outcome vars\n\n")
           }

        Y0plot  <- matrix(
                          Y0plot,
                          byrow = FALSE,
                          nrow = length(y.plot),
                          ncol = length(controls.identifier),
                          )

        colnames(Y0plot) <- unique(foo[control.rows, unit.variable])
        
     

    ########################################################################
    # Special Predictors#

    if(mode(special.predictors) != "logical" & length(special.predictors) > 0)
      {
        for(i in 1:length(special.predictors))
          {
            spf <- spec.pred.func(list.object = special.predictors[[i]],
                                  tr.numb = treatment.identifier,
                                  co.numb = controls.identifier,
                                  unit.var = unit.variable,
                                  time.var = time.variable,
                                  foo.object = foo,
                                  X0.inner = X0,
                                  X1.inner = X1
                                  )

            if(sum(is.na(spf$X1.inner) > 0))
              {
                stop("\n\n#################################################\n",
                     "\nMissingness in *Treated* special predictor", i, "\n\n")
              }

            if(sum(is.na(spf$X0.inner) > 0))
              {
                stop("\n\n#################################################\n",
                     "\nMissingness in *Control* special predictor", i, "\n\n")
              }

            X0 <- spf[[1]]
            X1 <- spf[[2]]

          }

      }

  #######################
       
  tag <- list(
              foo = as.character(foo),
              predictors = predictors,
              predictors.op = predictors.op,
              special.predictors = special.predictors,
              dependent = dependent,
              unit.variable = unit.variable,
              time.variable = time.variable,
              treatment.identifier = treatment.identifier,
              controls.identifier = controls.identifier,
              time.predictors.prior = time.predictors.prior,
              time.optimize.ssr = time.optimize.ssr,
              y.plot = y.plot,
              unit.names.variable = unit.names.variable
              )

######################################

  output <- list(
                 X0 = X0,
                 X1 = X1,
                 Z0 = Z0,
                 Z1 = Z1,
                 Y0plot = Y0plot,
                 Y1plot = Y1plot,
                 names.and.numbers = names.and.numbers,
                 tag = tag
                 )

  return(invisible(output))

}
