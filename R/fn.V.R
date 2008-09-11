# Function fn.V: JH 9/11/08
"fn.V" <-

  function(
           variables.v = stop("variables.v missing"),
           X0.scaled = stop("X0.scaled missing"),
           X1.scaled = stop("X0.scaled missing"),
           Z0 = stop("Z0 missing"),
           Z1 = stop("Z1 missing"),
           margin.ipop = 0.0005,
           sigf.ipop = 5,
           bound.ipop = 10,...
           )

  {
    # rescale par
    V <- diag( abs(variables.v)/sum(abs(variables.v)) )
    
    # set up QP problem
    H <- t(X0.scaled) %*% V %*% (X0.scaled)
    a <- X1.scaled
    c <- -1*c(t(a) %*% V %*% (X0.scaled) )
    A <- t(rep(1, length(c)))
    b <- 1
    l <- rep(0, length(c))
    u <- rep(1, length(c))
    r <- 0
    
    # run QP and obtain w weights
    res <- ipop(c = c, H = H, A = A, b = b, l = l, u = u, r = r, bound = bound.ipop,
                 margin = margin.ipop, maxiter = 1000, sigf = sigf.ipop)

    solution.w <- as.matrix(primal(res))
    
    # compute losses
    loss.w <- t(X1.scaled - X0.scaled %*% solution.w) %*%
      (V) %*% (X1.scaled - X0.scaled %*% solution.w)

    loss.v <- t(Z1 - Z0 %*% solution.w) %*%
      ( Z1 - Z0 %*% solution.w )

    return(invisible(loss.v))
  }
