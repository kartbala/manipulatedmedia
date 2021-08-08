# pests.r
#
# A PESTS source for R  (and maybe S-plus)
#
# PESTS = Poisson Estimators for State-space Time Series
# VERSION 1.1.6
#
# Requires R version >= 2.9.0
#
#
# Patrick Brandt
# pbrandt@utdallas.edu
# School of Economic, Political, and Policy Sciences
# The University of Texas, Dallas
# 800 W. Campbell Rd., GR 31
# Richardson, Texas 75080
#
# This file contains no warranty or guarantee of performance.
# Use at your own risk for non-commercial use
#
# Contents based on
#
# Brandt et al. 2000. "Dynamic Modelling for Persistent Event
# Count Time Series." American Journal of Political Science. 44(4):
# 823-843.
#
# Brandt and Williams. 2001. "A Linear Poisson Autoregressive
# Model: The PAR(p)"  Political Analysis 9(2).
#
# IF YOU USE THESE FUNCTIONS, CITE THE ABOVE REFERENCES.
#
# These functions are translated with modifications from the original
# GAUSS code.
# Copyright 1998-2005, Patrick T. Brandt and John T. Williams.
#
# R version of the code Copyright 2001-2006, Patrick T. Brandt.
#
# Revision history
#
# 20061120
#
# 20061025 Added descriptions of the functions for the PAR(p)
#          multiplier functions in this file
#
# 20050920 Revised the Pewma() function to estimate models WITHOUT any
#          covariates or a model with ONLY an intercept.
#
# 20041021 Added options to parp() and Pewma() for user supplied
#          starting values (thanks to Dan Wood for requesting this).
#
# 20040419 Revised the function calls for PEWMA and PAR(p) to use the
#          formula interface.
#          Implemented a class space for the models.
#          Implemented a new output format for the results.
#          Include a default test of omega=1 for the PEWMA and rho_i =
#          0 for PAR(p).
#          Added print functions for the class of the models.
# 20090907 Corrected some of the logic for how user input initial
#          starting values for the pewma() function are handled.
# 20161104 Add an offset term for the PAR(p) model
#---------------------------------------------------------------------#


####################################################################
# EWMA function for use in the PEWMA model
# Computes a simple EWMA
# Inputs:
# x = vector
# omega = weight parameter in [0.1]

ewma<-function(x,omega)
  { ind<-omega^(rev(seq(1:length(x))))
    wm<-weighted.mean(x,ind)
    wm
  }

####################################################################
# Purpose: Implements a Wald Test using the covariance matrix and
# a restriction matrix for general OLS / MLS regressions
#
# Format: wald(coef,restrict,covar,t);
#
# INPUTS: coef = Kx1, vector, coefficent vector
#     restrict = RxK, matrix, restriction matrix
#     covar = KxK, matrix, covariance matrix (from estimation)
#          that is symmetric and PD
#     t = scalar, sample size for df calculation
#
# OUTPUTS: wald_val = Wald statistic under the null
#          wp = p-value for X^2 with R degrees of freedom

wald<-function(coef,restrict,covar,t)
 { # Trap non-conformable restrictions */
   if(ncol(restrict)!=nrow(coef))
     {
     stop("WALD TEST ERROR: Restriction and coefficient matrices are non-conformable")
   }
   # Compute Wald statistic and p-value
   waldval<-t(coef)%*%t(restrict)%*%solve(restrict%*%covar%*%t(restrict))%*%restrict%*%coef
   # Small sample correction
   waldval <- ((t-nrow(coef)+nrow(restrict))/t)*waldval
   wp<-1-pchisq(waldval,nrow(restrict));

   list(Wald.stat=waldval,Wald.p=wp);
 }

####################################################################
# PEWMA code
# There are 4 functions:
# 1) DGP function
# 2) PEWMA Kalman filter
# 3) PEWMA model log-likelihood function
# 4) PEWMA wrapper that estimates a PEWMA regression model using
#    optim()
####################################################################

####################################################################
# pewmadgp -- PEWMA data generation process
# n = number of observations in generated series
# a0 = initial value of a0, shape parameter for gamma prior
# b0 = initial value of b0, scale parameter for gamma prior
# w = omega, EWMA weight parameter
# x = matrix of regressors, NOT including an intercept, unless you
#     want a model with a growth term.
# d = delta, vector of regression coefficients for x.
#

pewmadgp<-function(n,a0,b0,w,x,d=c(0))
  { y<-matrix(0,nrow=n); mu<-matrix(0,nrow=n);
    d<-matrix(d,ncol=1)
    r<-matrix(0, nrow=n, ncol=1);
    for(t in 1:n)
    {   if(t==1)
          { r[t]<-digamma(a0) - digamma(w*a0)
            m<-(a0*w*exp(r[t]))/(b0*w*exp(-x[t,]%*%d))
            y[t]<-rpois(1,m)
            # update to compute posterior
            a<-a0*w + y[t]
            b<-b0*w + exp(x[t,]%*%d + r[t]);
            mu[t]<-y[t]
          }
        else
          { r[t]<-digamma(a) - digamma(w*a)
            aa<-a*w
            bb <- b*w*exp(-x[t,]%*%d - r[t])
            m<-(exp(x[t,]%*%d + r[t])*(ewma(y[1:t-1],w))/(ewma((exp(x[1:t-1,]%*%d + r[1:t-1])),w)))
            y[t]<-rpois(1,m)
            rm(m)
            a<-aa + y[t]
            b<-w*b + exp(x[t,]%*%d + r[t]);
            mu[t]<-exp(x[t,]%*%d + r[t])*a/b;
          }
      }
    y<-ts(y,start=1)
    y
  }

####################################################################
# pewmafilter -- Extended Kalman filter for PEWMA model.  This is used
# to compute the filter and the parameters for the log-likelihood
# function, pewmallf().
# THIS FUNCTION IS NOT CALLED BY THE USER -- IT IS USED IN THE MAIN
# Pewma() ESTIMATION FUNCTION

pewmafilter<-function(y,x,w,d)
{  # Initialize some matrices we need to store the filter
   a<-matrix(0,nrow=length(y)); aa<-a; aaa<-a;
   b<-a; bb<-a; bbb<-a;
   mu<-a; r<-a;
   # Loop over the filter calculations
  for(t in 1:length(y))
    { if(t==1)
        { aaa[t]<-y[t]; bbb[t]<-exp(x[t,]%*%d + r[t,1]);
          mu[t]<-mean(y);
        }
      else
        { a[t]<-aaa[t-1];  b[t,1]<-bbb[t-1];
          r[t]<-digamma(a[t]) - digamma(w*a[t]);
          aa[t]<-w*a[t,1];
          bb[t]<-w*b[t,1]*exp(-x[t,]%*%d)*exp(-r[t]);
          aaa[t]<-w*a[t,1] + y[t];
          bbb[t]<-w*b[t,1] + exp(x[t,]%*%d + r[t]);
          mu[t]<-exp(x[t,]%*%d + r[t])*aaa[t]/bbb[t];
        }
    }
   # Clean up and store the filter components in a ts object
   dta<-matrix(cbind(a,aa,aaa,b,bb,bbb,mu,r),ncol=8)
   dta<-ts(dta, start=1, names=c("a","aa","aaa","b","bb","bbb","mu","r"))
   dta
 }

####################################################################
# pewmallf -- PEWMA log-likelihood function.
# Returns the value of the LLF for the given data and parameters.

pewmallf<-function(p,y=y,x=matrix(0,nrow=length(y)))
  { # initialize the parameters
    if(length(p)==1)
      { w<-p[1];  d<-as.matrix(0) }
    else
      { w<-p[1]; d<-as.matrix(p[2:length(p)]) }
    y<-as.ts(y); x<-as.ts(x);
    # Call the filter
    pf<-pewmafilter(y,x,w,d);
    # Extract vectors of parameters from filter output
    pf<-pf[2:nrow(pf),];
    a<-pf[,"a"]; b<-pf[,"b"]; r<-pf[,"r"];
    x<-x[2:nrow(x),];    y<-y[2:nrow(y)];
    # Compute log-likelihood function
    lp<-(lgamma((w*a) + y) - lgamma(w*a)
         - lgamma(y+1) + ((w*a) * log(w*b*exp(-x%*%d - r)))
         - ((w*a) + y)*(log(1 + (w*b*exp(-x%*%d - r)))));
    lp<-sum(lp)
    lp
  }

####################################################################
# Pewma -- Fits a Poisson Exponentially Weighted Moving Average Model
#          by maximum likelihood.
#
# Syntax: Pewma(formula, omega.init=0.6, init.param=NULL, ...)
#
# Description:
#
# This function uses the standard formula interface:
# y ~ x1 + x2 + ...
#
# y = Event count time series you wish to fit.  (can be a matrix or a
# ts object)
# x = regressors not including an intercept, unless you
# want a growth term.
#
# init.param = starting values for the optimizer.  Use a glm() call to
# compute these or leave NULL and they will be computed for you.
#
# THIS IS IMPORTANT!  IF YOU DO *NOT* THINK THERE
# IS EXPONENTIAL GROWTH IN THE COUNTS USE A FORMULA OF THE FORM:
#
# y ~ -1 + x1 + x2 ....
#
# If you think the counts are a random walk without drift you want the
# above specification, which estimates a model that has a latent
# random walk with zero drift. (IF YOU DO NOT KNOW WHAT THIS IS
# SAYING, USE THE ABOVE SPECIFICATION WITH THE "MINUS 1" TERM IN THE
# FORMULA!)
#
# If there is random drift, you want an intercept or a model formula like
# this:
#
# y ~ 1 + x1 + x2 ....
#
# This model will have drift in the latent mean.
#
# Models without covariates can now be fit using the following formula
# specifications:
#
# y ~ -1  : Just the PEWMA process -- estimates only omega.
# y ~ 1   : PEWMA process with drift -- estimates omega and a trend
#           term as the intercept
#
# Other optional arguments:
#
# omega.init = 0.6 -- starting value for omega.  Can be changed, but
#                     0 < omega <= 1
# init.param = NULL --- Function uses a Poisson regression for
#                       starting values.  This can be replaced with a
#                       list, c(....) for user supplied starting
#                       values.  Must be conformable.



# Note that the R version uses the constrained optimizer for the
# parameter w.  This is because 0 < w <= 1.  The parameter can go
# slightly over the bound of 1 because this is where the Pewma reduces
# to the Poisson model.  If the bound bites for d, just alter the
# value of 1000 to something else.

Pewma<-function(formula, omega.init=0.6, init.param=NULL, ...)
  { # Set up the model

    model <- model.frame(formula)

    # Setup of x and y for the model depends on the number of
    # covariates and the inclusion of the intercept

    # Get y and x
    y <- model.extract(model, "response")
    x <- as.matrix(model.matrix(formula))
    xnames <- dimnames(x)[[2]]
    k <- ncol(x)

    # Get starting values if needed
    if(is.null(init.param)==TRUE & k>0)
       {
         init.param <- glm(formula, family=poisson())$coefficients
       }

    # Now build the correct bounds for the optimizer

    bound <- 1000
    if(k>0)
      { lower.bound <- c(0,rep(-bound,k))
        upper.bound <- c(1.1,rep(bound,k))
      } else {
        lower.bound <- c(0)
        upper.bound <- c(1.1)
      }

    # Fit the model
    if(k>0)
      {
        fitted.param<-optim(c(omega.init,init.param), pewmallf, method = "BFGS",
#                            lower=lower.bound, upper=upper.bound,
                            hessian=TRUE,
                            control=list(trace=2, REPORT=5, fnscale=-(length(y))),
                            y=as.matrix(y),x=as.matrix(x))
     } else {
        fitted.param<-optim(omega.init, pewmallf, method = "L-BFGS-B",
                            lower=lower.bound, upper=upper.bound,
                            hessian=TRUE,
                            control=list(trace=2, REPORT=5, fnscale=-(length(y))),
                            y=as.matrix(y))
      }

    # Retrieve the parameters and the llf value
    param<-fitted.param$par
    llf <- fitted.param$value

    # Estimate the standard errors and compute the z scores
    if(k>0)
    { covar <- -solve(fitted.param$hessian)
      se<-sqrt(diag(covar))
      z<-param/se
    } else {
      covar <- -1/(fitted.param$hessian)
      se<-sqrt((covar))
      z<-param/se
    }
    coefs <- t(matrix(rbind(param,se,z),nrow=3))
    colnames(coefs) <- c("Parameters","Std. Errors","Z-score")

    if(k>0)
      {
        rownames(coefs) <- c("Omega", colnames(x))
      } else {
        rownames(coefs) <- c("Omega")
      }

    cat(" ","\n")
    cat("--------------------------------------","\n")
    cat("PEWMA regression output","\n")
    cat("--------------------------------------","\n")
    printCoefmat(coefs)
    cat("--------------------------------------","\n")
    cat("Log-likelihood value  : ", llf, "\n")
    aic <- -2*llf+2*k
    cat("AIC                   : ", aic, "\n")
    dof <- length(y)-k
    cat("Degrees of Freedom    : ", dof, "\n")
    cat("--------------------------------------","\n")
    cat("Test for reduction to Poisson\n")
    cat("--------------------------------------","\n")
    wald.test <- ((param[1]-1)/se[1])^2
    wald.p <- 1-pchisq(wald.test,1)
    cat("Wald test for omega=1 : ", wald.test, "\n")
    cat("Wald p-value          : ", wald.p, "\n")
    cat("--------------------------------------","\n\n")

    fit <- list(coefs=coefs,
                optim.param=fitted.param,
                param=param,
                covar=covar,
                std.err=se,
                z=z,
                llf=llf,
                aic=aic,
                dof=dof,
                wald.test=wald.test,
                wald.p=wald.p,
                k=k,
                y=y,
                x=x,
                call=match.call(),
                obj.type=c("PEWMA regression output"))
    class(fit) <- c("pests.output")
    return(fit)
  }

####################################################################
# PAR(p) code
# There are 4 functions:
# 1) DGP function
# 2) PAR(p) Kalman filter
# 3) PAR(p) model log-likelihood function
# 4) PAR(p) wrapper that estimates a PAR(p) regression model using
# optim()
####################################################################

####################################################################
# parpdgp -- PAR(p) data generation process
# n = number of observations in generated series
# r = AR(p) coefficients
# x = matrix of regressors, including an intercept.
# d = delta, vector of regression coefficients for x.
#

parpdgp<-function(n,r,x=matrix(0,nrow=n),d=as.matrix(0))
  { # Initialize the parameters
    y<-matrix(0,nrow=n)
    r<-as.matrix(r)
    p<-nrow(r)
    xd<-exp(as.matrix(as.matrix(x)%*%as.vector(d)))
    # Loop over DGP
    for(t in 1:n)
      { if(t<(p+1))
          { y[t]<-rpois(1,xd[t]) }
        else
          { ylag<-t(y[(t-p):(t-1)])%*%rev(r)
            y[t]<-rpois(1,(ylag + (1-sum(r))*xd[t]))
          }
      }
    return(ts(y,start=1))
  }

####################################################################
# parpfilter -- Extended Kalman filter for PAR(p) model.  This is used
# to compute the filter and the parameters for the log-likelihood
# function, parpllf().

parpfilter<-function(y,x,r,d,o)
  { # Initialize some parameters
    y<-as.matrix(y)
    r<-as.matrix(r)
    p<-nrow(r)
    n <- length(y)
    xd<-exp(as.matrix(as.matrix(x)%*%as.vector(d)) + o)
    m<-matrix(0,nrow=n)
    s<-m
    # Filter loop
    for(t in 1:n)
      { if(t<(p+3))
          { m[t]<-mean(y[1:(p+3)]); s[t]<-var(y[1:(p+3)]);
          }
        else
          { ylag<-t(y[(t-p):(t-1)])%*%rev(r)
            m[t]<-ylag + (1-sum(r))*xd[t]
            s[t]<-(sum(r^2)*var(y[1:t-1]) + (1-sum(r^2))*var(xd[1:t-1]))
          }
      }
    return(ts(matrix(cbind(m,s,xd),ncol=3),start=1,names=c("m","s2","xd")))

  }

####################################################################
# parpllf -- PAR(p) log-likelihood function.
# Returns the value of the LLF for the given data and parameters.

parpllf<-function(p,y=y,x=matrix(0,nrow=length(y)),o)
  { # initialize the parameters
    k<-ncol(x)
    no.param<-length(p)
    r<-as.matrix(p[1:(no.param-k)])
    d<-as.matrix(p[(no.param-k+1):length(p)])
    y<-as.matrix(y); x<-as.matrix(x);
    # Call the filter
    pf<-parpfilter(y,x,r,d,o);
    # Extract vectors of parameters from filter output
    pf<-pf[(no.param-k+1):nrow(pf),];
    m<-pf[,"m"]; s2<-pf[,"s2"];
    x<-x[(no.param-k+1):nrow(x),];    y<-y[(no.param-k+1):nrow(y)];
    # Compute log-likelihood function
    lp<-(lgamma((s2*m) + y) - lgamma(s2*m) - lgamma(y+1)
         + (s2*m*log(s2)) - ((s2*m + y)*log(1+s2)))
    lp<-sum(lp)
    lp
  }

####################################################################
# Parp -- this is the main function for calling the PAR(p) model
# estimation.

# Inputs
# y = Event count time series you wish to fit.  (can be a matrix or a
# ts object)
# x = matrix of regressors,including an intercept
# formula = R formula format,, typically y ~ x1 + x2 + ...
# o = offset
#
# parp.init = REQUIRED starting the AR(p) values for the optimizer.  This is
#             the p AR parameters
# init.param = NULL.  This uses the Poisson regression values for
#             starting values unless the user supplies values in the form
#             c(d_1,d_2,...,d_k), where d_1 is the constant.
#

Parp<-function(formula, p=1, parp.init=rep(0.1,p), init.param=NULL, ...)
  {

    model <- model.frame(formula)
    y <- model.extract(model, "response")
    o <- model.extract(model, "offset")   # If an offset is provided
    if(is.null(o)==TRUE) {o=0} else {o=o} # If one is not provided
    x <- model.matrix(formula)
    k <- ncol(x)
    xnames <- dimnames(x)[[2]]
    colnames(x) <- xnames

    # Do some work to get a vector of starting values.
    if(is.null(init.param))
       {
         init.param <- glm(formula, family=poisson())$coefficients
       }

    # Do a simplex run to get some good starting values
    start.param <- optim(c(parp.init,init.param), parpllf,
                         method="Nelder-Mead", hessian=FALSE,
                         control=list(maxit=800, reltol=0.0001,
                         trace=0, fnscale=-(length(y))),
                         y=y,x=x,o=o)

    # Estimate the PAR(p) model
    fitted.param<-optim(start.param$par, parpllf, method = "BFGS", hessian=TRUE,
                        control=list(maxit=1000,
                          trace=0, fnscale=-(length(y))),
                        y=y,x=x,o=o)

    # Retreive the parameters and llf values
    param<-fitted.param$par
    llf <- fitted.param$value

    # Estimate the standard errors and compute z scores
    covar <- -solve(fitted.param$hessian)
    se<-sqrt(diag(covar))
    z<-param/se

    k <- ncol(x)
    k.plus.p <- length(se)
    num.p <- k.plus.p - k
    fitted.par.filter <- parpfilter(y,x,param[1:num.p],
                                    param[(num.p+1):k.plus.p],o)

    residuals <- y - fitted.par.filter[,"m"]
    # Print the results
    coefs <- t(matrix(rbind(param,se,z),nrow=3));
    rownames(coefs) <- c(rep("rho",num.p),colnames(x))
    colnames(coefs) <- c("Parameters","Std. Errors","Z-score")
##     cat(" ","\n")
##     cat("PAR(p) regression output","\n")
##     cat("--------------------------------------------","\n")
##     printCoefmat(coefs)
##     cat("--------------------------------------------","\n")
##     cat("Log-likelihood value  : ", llf, "\n")
    aic <- (-2*llf)+(2*(length(param)-1))
##    cat("AIC                   : ", aic, "\n")
    dof <- length(y) - k.plus.p
##    cat("Degrees of Freedom    : ", dof, "\n")
##    cat("--------------------------------------------","\n")
    R <- diag(1, nrow=p, ncol=k.plus.p)
    wald.test <- wald(matrix(param, ncol=1), R, covar, length(y))
    wald.p <- wald.test$Wald.p
    wald.test <- wald.test$Wald.stat
##     cat("Test for reduction to Poisson regression\n")
##     cat("--------------------------------------------","\n")
##     cat("Wald test for rho_i=0 : ", wald.test, "\n")
##     cat("Wald p-value          : ", wald.p, "\n")
##     cat("--------------------------------------------","\n\n")
    fit <- list(coefs=coefs,
                optim.param=fitted.param,
                param=param,
                covar=covar,
                std.err=se,
                z=z,
                residuals=residuals,
                llf=llf,
                aic=aic,
                dof=dof,
                wald.test=wald.test,
                wald.p=wald.p,
                k=k,
                p=num.p,
                y=y,
                x=x,
                o=o,
                fit=fitted.par.filter, # These were computed, but not returned
                residuals=residuals,   # These were computed, but not returned
                call=match.call(),
                obj.type=c("PAR(p) regression output"))
    class(fit) <- c("pests.output")
    return(fit)
  }


# Print function for the PEWMA and PAR(p) output.

"print.pests.output" <- function(x)
  { cat(" ","\n")
    cat(x$obj.type, "\n")
    cat("\nCall: ", deparse(x$call), "\n\n")
    cat("--------------------------------------------","\n")
    printCoefmat(x$coefs)
    cat("--------------------------------------------","\n")
    cat("Log-likelihood value  : ", x$llf, "\n")
    aic <- (-2*x$llf)+(2*(length(x$param)-1))
    cat("AIC                   : ", x$aic, "\n")
    cat("Degrees of Freedom    : ", x$dof, "\n")
    cat("--------------------------------------------","\n")
    cat("Test for reduction to Poisson\n")
    cat("--------------------------------------------","\n")
    cat("Wald test statistic   : ", x$wald.test, "\n")
    cat("Wald p-value          : ", x$wald.p, "\n")
    cat("--------------------------------------------","\n")

  }

# Holder function for the summary method.
"summary.pests.output" <- print.pests.output

####################################################################
# ACF function standardized for count data...
# Feed in your counts and you can generate some ACFs!

# Computes an ACF for counts by standardizing them.  This is a valid
# method for searching for autocorrelation in counts.  See Cameron and
# Trivedi (Regression Analysis of Count Data, CUP, 1998: 228).

counts.acf <- function(y, lag.max=(length(y)/4),...)
  { z <- y-mean(y)
    ct.acf <- acf(z, lag.max=lag.max,plot=T,...)
    return(ct.acf)
  }


# Box-Ljung Q-text for count data.  Tests for serial correlation ag
# "lag" value.
counts.Q <- function(y, lag=1, ...)
  { z <- y-mean(y)
    test <- Box.test(z,lag=lag,type=c("Ljung-Box"))
    return(test)
  }


# Multiplers functions
# These work, but at present are undocumented.


# parp.multipliers() Computes impact multipliers for the PAR(p) model
# output.
#
# Inputs: parp.obj : output of a Parp() model call.  See Parp() above
#                    for details.
#         xvec : vector of values at which to compute the
#                multipliers.  This should be conformable with the number of
#                exogenous variable coefficients in the model (including the
#                constant).  TYPICALLY, this will be a vector of zeros
#                and ones, where the 1's are for the one unit change in a variable,
#                and the zeros leave a variable unchanged.  Details are documents in
#                the Brandt and Williams (2001) paper in the section on
#                interpretation of the PAR(p) model.  Note that code
#                below exactly matches equations 10 and 11

parp.multipliers <- function(parp.obj, xvec=matrix(rep(1,parp.obj$k),1,parp.obj$k))
  { p <- parp.obj$p
    k <- parp.obj$k
    ar.coef <- parp.obj$param[1:p]
    x.coef <- matrix(parp.obj$param[(p+1):(p+k)], ncol=1)
    o <- mean(parp.obj$o)
    sum.ar.coef <- sum(ar.coef)
    impact.multiplier <- ((1-sum.ar.coef)*exp(xvec%*%x.coef + o)*x.coef[,1])
    longrun.multiplier <- impact.multiplier/(1-sum.ar.coef)

    # Format
    impacts <- rbind(impact.multiplier, longrun.multiplier)
    colnames(impacts) <- colnames(parp.obj$x)
    rownames(impacts) <- c("Short Run", "Long Run")
    output <- list(impacts=impacts, call=parp.obj$call, changes=xvec)
    class(output) <- c("pestsmultipliers")
    return(output)
  }

# print.pestsmultipliers: printing function for parp.multipliers

"print.pestsmultipliers" <- function(x)
  { print(x$impacts)
  }

########################################################################
# montecarlo.parp.multipliers()
# Draws a parametric Monte Carlo sample from the PAR(p) multipliers.
# Inputs: first two are the same as in parp.multipliers.
#         n : number of samples to draw.

# Output: array of impact and long run multiplers.  Inner array
# indices are n x k where n is the number of samples and k is the
# number of variables over which the impacts are computed (i.e.,
# number of exogenous variables).  Outermost array indices separate on
# the short versus total impact.  The first array is the impact, the
# second the long run.

montecarlo.parp.multipliers <- function(parp.obj, xvec=matrix(rep(1,parp.obj$k),1,parp.obj$k), n)
  { p <- parp.obj$p
    k <- parp.obj$k
    o <- max(parp.obj$o)

    chol.covar <- t(chol(-solve(parp.obj$optim.param$hessian)))
    coef <- parp.obj$param
    mean.y <- mean(parp.obj$y)
    multipliers <- array(0, dim=c(n,k,2))

    for (i in 1:n)
      { coef.i <- coef + chol.covar%*%matrix(rnorm(length(coef)),nrow=(p+k))
        ar.coef <- coef.i[1:p]
        x.coef <- coef.i[(p+1):(p+k)]
        sum.ar.coef <- sum(ar.coef)
        # impact
        multipliers[i,,1] <- (1-sum.ar.coef)*exp(xvec%*%x.coef + o)*x.coef
        # long run
        multipliers[i,,2] <- multipliers[i,,1]/(1-sum.ar.coef)
      }
    return(multipliers)
  }

#######################################################################
# Same as the montecarlo.parp.multipliers.  Just computes them for a
# Poisson model estimated by glm().  Typically used for comparison
# with the above.

glm.poisson.multipliers <- function(psn.glm,xvec,n)
  { chol.covar <- chol(vcov(psn.glm))
    coef <- coefficients(psn.glm)
    xvec <- matrix(xvec,nrow=1)
    o <- max(psn.glm$offset)
    impact <- matrix(0, nrow=n, ncol=length(coef))

    for (i in 1:n)
      { coef.i <- coef + chol.covar%*%matrix(rnorm(length(coef)),nrow=(length(coef)))
        impact[i,] <- t(rep(exp(xvec%*%coef.i + o),length(coef.i))*coef.i)
      }
    return(impact)
  }


#######################################################################
# parp.impulse: Function to compute impulse responses for shocks in X
# variables for PAR(p) model.  Traces these out over a horizon of
# 1...n

# Inputs: parp.obj : output of a call to Parp() for a model
#         x.impulses : size of the shocks in the X variables.  Should
#                      probably only use this to shock one at a time!
#                      This will typically be a vector of 0-1 values
#                      where the 1's enter for the variable whose
#                       shock you want to trace.
# Output: impulse response vector.  Plot this over time to trace the
# response.

parp.impulse <- function(parp.obj, x.impulses, n)
  { p <- parp.obj$p
    k <- parp.obj$k
    o <- mean(parp.obj$o)
    ar.coef <- parp.obj$param[1:p]
    x.coef <- parp.obj$param[(p+1):(p+k)]
    mean.y <- mean(parp.obj$y)
    sum.ar.coef <- sum(ar.coef)
    impulse <- matrix(0, nrow=(n+p))
    for (i in (1+p):n)
      {
        impulse[i] <- (impulse[(i-1)])%*%ar.coef[1] +
              (1-sum.ar.coef)*exp(x.impulses[i,]%*%x.coef + o)
        if (p>1)
          { for (j in 2:p)
              { impulse[i] <- impulse[i] + impulse[i-j] %*% ar.coef[j]
              }
          }
      }
    return(impulse[2:n])
  }

# Function to do a Monte Carlo estimate of the trend components
# INPUTS:
# parp.obj = fitted PAR(p) model
# trend = specification of the trend variable
# n = number of MC replications

parp.trends <- function(parp.obj, trend, n)
{
  p <- parp.obj$p
  k <- parp.obj$k
  o <- parp.obj$o
  capT <- length(parp.obj$y)
  chol.covar <- t(chol(-solve(parp.obj$optim.param$hessian)))
  coef <- parp.obj$param
  est <- matrix(0, capT, n)

  for (i in 1:n)
  { # Sample coefficients
    coef.i <- coef + chol.covar%*%matrix(rnorm(length(coef)),nrow=(p+k))
    ar.coef <- coef.i[1:p]
    x.coef <- coef.i[(p+1):(p+k)]
    # Predict step
    trend.pred <- parpfilter(parp.obj$y, parp.obj$x,
                             ar.coef, x.coef, o)
    # Save result
    est[,i] <- trend.pred[,3]
    }
  return(est)
}

# Same as above for Poisson GLM
glm.trends <- function(psn.glm, n)
{
  return(simulate(psn.glm, n))
}
