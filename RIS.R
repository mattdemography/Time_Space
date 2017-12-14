#RIS estimator - adaoted from Calabrese and Elkink (2014) Online Appendix

bsar <- function(formula, data, adj.matrix, method = "RIS", weights, subset,
    na.action, start = NULL, etastart, mustart, offset, control = list(...),
    model = TRUE, x = FALSE, y = TRUE, contrasts = NULL, debug = FALSE, ...)
{
    ## Name all parameters
    call <- match.call()
    
    ## If ’data’ parameter not given, take calling environment
    if (missing(data))
        data <- environment(formula)
    mf <- match.call(expand.dots = FALSE)
    
    m <- match(c("formula", "data", "subset", "weights", "na.action",
        "etastart", "mustart", "offset"), names(mf), 0L)
    mf <- mf[c(1L, m)]
    mf$drop.unused.levels <- TRUE
    mf[[1L]] <- as.name("model.frame")
    mf <- eval(mf, parent.frame())
    mt <- attr(mf, "terms")
    
    Y <- model.response(mf, "any")
    if (length(dim(Y)) == 1L) {
        nm <- rownames(Y)
        dim(Y) <- NULL
        if (!is.null(nm))
            names(Y) <- nm
    }
    
    X <- if (!is.empty.model(mt))
        model.matrix(mt, mf, contrasts)
    else matrix(, NROW(Y), 0L)
    
    W <- adj.matrix
    weights <- as.vector(model.weights(mf))
    if (!is.null(weights) && !is.numeric(weights))
        stop("’weights’ must be a numeric vector")
    if (!is.null(weights) && any(weights < 0))
        stop("negative weights not allowed")
    offset <- as.vector(model.offset(mf))
    if (!is.null(offset)) {
        if (length(offset) != NROW(Y))
            stop(gettextf(
                "number of offsets is %d should equal %d (number of obs.)",
                length(offset), NROW(Y)), domain = NA)
    }
    mustart <- model.extract(mf, "mustart")
    
etastart <- model.extract(mf, "etastart")
    
    if (method == "RIS")
        fit <- ris_estimator(Y, X, W, control, debug = debug)
    else
        stop(sprintf("Method ’%s’ not yet implemented", method))

    fit 
}

ris_estimator <- function(y, X, W, control = list(...), debug = FALSE)
{
    con <- list(
    R = 1000,
    store.density = FALSE,
    optim.max.iterations = 1000,
    optim.reltol = .0025
    )
    
    con[names(control)] <- control
    with(con, {
         n <- length(y)
         k <- dim(X)[2]
         Z <- diag(as.vector(1 - 2 * y))
         In <- diag(n)

  ## Random draw from importance density function, given
  ## an upper bound, using antithetical sampling
  random.draw <- function(upper.bound)
  {
    q <- runif(R/2)
    q <- c(q, 1-q)
    
    qnorm(q * pnorm(upper.bound))
  }
  
  iter <- 0
  
  densities <- NULL
  
  ll <- function(par)
  {
    iter <<- iter + 1
    
    rho <-  -1 + 2 * pnorm(par[k+1])
    
    beta <- par[1:k]
    
    Ai <- -rho * W
    diag(Ai) <- 1
    A<-solve(Ai)
    
    omega <- Z %*% A %*% t(A) %*% t(Z)
    V <- -Z %*% A %*% X %*% beta
    B <- solve(chol(solve(omega)))
    
    Eta0 <- Eta <- matrix(NA, nrow = n, ncol = R)
    
    Eta0[n,] <- rep(1/B[n,n] * V[n], R)
    Eta[n,] <- random.draw(Eta0[n,])
    
    for (i in (n-1):1)

        
        {
        Eta0[i,] <- 1/B[i,i] * (V[i] - t(B[i,(i+1):n]) %*% Eta[(i+1):n,])
        if (i > 1)
            Eta[i,] <- random.draw(Eta0[i,])
         }

      pnEta0 <- pnorm(Eta0)
      a <- 1 / mean(pnEta0)
      ll <- log(mean(apply(pnEta0 * a, 2, prod))) - log(1/R) - n * log(a)
      
      ll 
   }
   
    par <- optim(c(rep(0,k), 0), ll, control=list(fnscale = -1,
                reltol = optim.reltol, maxit = optim.max.iterations))
                
    list(beta = par$par[1:k],
         rho = -1 + 2 * pnorm(par$par[k+1]),
         beta.se = rep(NA, k),
         rho.se = NA,
         densities = densities)
         })    # end of with con
         
}

