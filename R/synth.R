"synth" <-
  function(           data.prep.obj = NA,
                      X1 = stop("X0 missing"),
                      X0 = stop("X1 missing"),
                      Z0 = stop("Z0 missing"),
                      Z1 = stop("Z1 missing"),
                      custom.v = FALSE,
                      Margin.ipop = 0.0005,
                      Sigf.ipop = 4,
                      Bound.ipop = 10,... 
                      )
  { 
    # Retrieve dataprep objects
    if(sum(is.na(data.prep.obj))==0)
      {
        cat("\nX1, X0, Z1, Z0 all come directly from dataprep object.\n\n")
        X1 <- data.prep.obj$X1
        Z1 <- data.prep.obj$Z1
        X0 <- data.prep.obj$X0
        Z0 <- data.prep.obj$Z0
      } else {
        cat("X1,X0,Z1,Z0 were individually input (not dataprep object.)\n\n")}
    
    # Normalize X 
    nvarsV <- dim(X0)[1]
    big.dataframe <- cbind(X0, X1)
    divisor <- sqrt(apply(big.dataframe, 1, var))
    scaled.matrix <-
      t(t(big.dataframe) %*% ( 1/(divisor) *
                              diag(rep(dim(big.dataframe)[1], 1)) ))

    X0.scaled <- scaled.matrix[,c(1:(dim(X0)[2]))]
    X1.scaled <- scaled.matrix[,dim(scaled.matrix)[2]]
    
    # check if custom v weights are supplied
    # if not start optimization
    if(custom.v == FALSE) 
      {
      
      # two attemps for best V are made: 
      # equal weights and regression based starting values 
     cat("\n****************",
         "\n searching for synthetic control unit  \n","\n"
        )
      
      # first set of starting values: equal weights  
      SV1 <- rep(1/nvarsV,nvarsV)
  
      rgV.optim.1 <- optim(SV1, fn.V, 
                           margin.ipop = Margin.ipop,
                           sigf.ipop = Sigf.ipop,
                           bound.ipop = Bound.ipop,
                           X0.scaled = X0.scaled,
                           X1.scaled = X1.scaled,
                           Z0 = Z0,
                           Z1 = Z1,...
                           )
        
      # second set of starting values: regression method 
      # will sometimes not work because of collinearitys in X
      # so it's wrapped in a try command
      Xall <- cbind(X1.scaled,X0.scaled)
      Xall <- cbind(rep(1,ncol(Xall)),t(Xall))
      Zall <- cbind(Z1,Z0)
      Beta <- try(solve(t(Xall)%*%Xall)%*%t(Xall)%*%t(Zall),silent=TRUE)
      
      # if inverses did not work, we
      # stick with first results    
      if(inherits(Beta,"try-error")) 
       {
        rgV.optim <- rgV.optim.1
       } else {
      # otherwise we run a second optimization with regression starting values
        Beta <- Beta[-1,]
        V    <- Beta%*%t(Beta)
        SV2  <- diag(V)
        SV2 <- SV2 / sum(SV2)
  
      rgV.optim.2 <- optim(SV2, fn.V, 
                           margin.ipop = Margin.ipop,
                           sigf.ipop = Sigf.ipop,
                           bound.ipop = Bound.ipop,
                           X0.scaled = X0.scaled,
                           X1.scaled = X1.scaled,
                           Z0 = Z0,
                           Z1 = Z1,...
                           )
          
      # and keep the better optim results    
      if(rgV.optim.1$value < rgV.optim.2$value) 
       { rgV.optim <- rgV.optim.1 } else {
         rgV.optim <- rgV.optim.2 }
     }
     
      # final V weights from optimization
      solution.v   <- abs(rgV.optim$par)/sum(abs(rgV.optim$par))
     
     } else {  # if user choose to supply v manually:
 
     cat("\n****************",
         "\n v weights supplied manually: computing synthtic control unit \n","\n\n"
        )
 
     rgV.optim  <- NULL
     solution.v <- abs(custom.v)/sum(custom.v)} 

    if(length(solution.v) != nvarsV) {stop("WARNING: CUSTOM V WRONG LENGTH")}
        
    # now recover solution.w  
    V <- diag(solution.v)
    H <- t(X0.scaled) %*% V %*% (X0.scaled)
    a <- X1.scaled
    c <- -2*c(t(a) %*% V %*% (X0.scaled) )
    A <- t(rep(1, length(c)))
    b <- 1
    l <- rep(0, length(c))
    u <- rep(1, length(c))
    r <- 0

    res <- ipop(c = c, H = H, A = A, b = b, l = l, u = u, r = r,
                margin = Margin.ipop, maxiter = 1000, sigf = Sigf.ipop, bound = Bound.ipop)

    solution.w <- as.matrix(primal(res))

    loss.w <- t(X1.scaled - X0.scaled %*% solution.w) %*%
      V %*% (X1.scaled - X0.scaled %*% solution.w)

    loss.v <-
      t(Z1 - Z0 %*% as.matrix(solution.w)) %*%
        (Z1 - Z0 %*% as.matrix(solution.w))
          
 
    # produce viewable output
    cat("\n****************",
        "\n****************",
        "\n****************",
        "\n\nLOSS (V):", loss.v,
        "\n\nLOSS (W):", loss.w,
        "\n\nsolution.v:\n", round(as.numeric(solution.v), 10),
        "\n\nsolution.w:\n", round(as.numeric(solution.w), 10),
        "\n\n"
        )
        
    optimize.out <- list(
                         solution.v = solution.v,
                         solution.w = solution.w,
                         loss.v = loss.v,
                         loss.w = loss.w,
                         custom.v = custom.v,
                         rgV.optim = rgV.optim
                         )

    return(invisible(optimize.out))

  }
