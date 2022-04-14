#' Multivariate Adaptive Regression Splines (MARS)
#' @description Fit Friedman's Multivariate Adaptive Regression Splines (MARS) model.
#'
#' @usage mars(formula, data, control = NULL)
#' @param formula an R formula
#' @param data a data frame containing data for use in the model
#' @param control an optional object of class 'mars.control'
#'
#' @details The model we receive at the end from the mars algorithm is a many basis functions in a linear combination, found by least squares
#' We input a formula, data frame & control object. It uses step-wise algorithms to fit.
#'
#' @return an object of mars which has the model
#' @export
#'
#'
#' @examples
#' mm<-mars(y~.,dat=mars::marstestdata)
#' @import stats
#' @author Tauseef Kashtwari, Promit Chowdhury, Ibraheem Azad
#' @references Jerome H. Friedman. Multivariate Adaptive Regression Splines (with discussion).Annals of Statistics 19/1, 1991. \url{https://statistics.stanford.edu/research/multivariate-adaptive-regression-splines.}
#' @seealso [mars.control] for constructing control objects
#' @seealso [plot.mars] for plotting results
#' @seealso [predict.mars] for predictions
#' @seealso [summary.mars] for summarizing mars objects
#' @seealso [print.mars] for printing mars objects
mars <- function(formula,data,control=NULL){
  cc <- match.call()
  mf <- model.frame(formula,data)
  y <- model.response(mf)
  mt <- attr(mf,"terms")
  x <- model.matrix(mt,mf)[,-1,drop=FALSE]

  if(is.null(control)) {
    control <- mars.control()
  }

  fwd <- fwd_stepwise(y,x,control)
  bwd <- bwd_stepwise(fwd,control)
  mdl <- lm(y~.-1, data = data.frame(y = y, bwd$B))
  out <- c(list(call = cc, formula = formula, y = y, B = bwd$B, Bfuncs = bwd$Bfuncs, x_names=colnames(x)), mdl)
  class(out) <- c("mars", class(mdl))
  out
  return(out)
}


#' Constructor for 'mars.control' objects
#'
#' @param Mmax Maximum number of basis functions. Should be an even integer. Default value is is 2.
#' @param d The coefficient in the penalty term of the generalized cross validation measure. Default is 3.
#' @param trace Should we print status information about the fitting? Default is 'FALSE'.
#'
#' @return a 'mars.control. object
#' @export
#'
#' @examples mc <- mars.control(Mmax = 10)
mars.control = function(Mmax = 2, d=3,trace=FALSE) {
  control <- new_mars.control(Mmax, d, trace)
  validate_mars.control(control)
  return (control)
}


#----------------------Constructor---------------------#
new_mars.control = function(Mmax, d, trace) {
  structure(list(Mmax = Mmax, d = d, trace = trace), class = 'mars.control')
}


#----------------------Validator-----------------------#
validate_mars.control = function(control) {
  if(control$Mmax < 2 || control$Mmax %% 2 == 1) {
    warning("Input Mmax must be an even integer >= 2; setting to 2")
    control$Mmax <- 2
  }
}


#---------------------fwd_stepwise--------------------#
fwd_stepwise <- function(y,x,control){
  #initialize variables
  Mmax <- control$Mmax
  N <- length(y)
  n <- ncol(x)
  B <- init_B(N,Mmax)
  Bfuncs <- vector(mode="list", length=Mmax+1)

  for(i in 1:(Mmax/2)) {
    M <- 2*i - 1
    lof_best <- Inf
    for(m in 1:M) {
      dd <- setdiff(c(1:n), Bfuncs[[m]][,"v"])
      for(v in dd){
        tt <- split_points(x[,v],B[,m])
        for(t in tt) {
          Bnew <- data.frame(B[,1:M],
                             Btem1=B[,m]* h(+1,x[,v],t), #-------------swtich order of hinge function---------------
                             Btem2=B[,m]* h(-1,x[,v],t)) #-------------swtich order of hinge function---------------
          gdat <- data.frame(y=y,Bnew)
          lof <- LOF(y~.-1,gdat,control)
          if(lof < lof_best) {
            lof_best <- lof;
            split_best <- c(m=m,v=v,t=t)
          }
        }
      }

    }
    m <- split_best["m"]; v <- split_best["v"]; t <- split_best["t"];

    #conver tto numeric
    'switch this around h function -MISTAKE'
    B[,M+1] <- as.numeric(cbind(B[,m]*h(-1, x[, v], t)))
    B[,M+2] <- as.numeric(cbind(B[,m]*h(+1, x[, v], t)))


    Bfuncs[[M+1]] <- rbind(Bfuncs[[m]], c(s=-1,v,t))
    Bfuncs[[M+2]] <- rbind(Bfuncs[[m]], c(s=+1,v,t))
  }
  colnames(B) <- paste0("B",(0:(ncol(B)-1)))
  return(list(y=y,B=B, Bfuncs=Bfuncs))
}


#---------------------bwd_stepwise--------------------#
bwd_stepwise <- function(bwd_in, control) {
  y <- bwd_in$y
  B <- bwd_in$B
  Mmax <- ncol(bwd_in$B) - 1
  Jstar <- 2:(Mmax+1)
  Kstar <- Jstar
  dat <- data.frame(y = y, B)
  LOFstar <- LOF(y~., dat, control)
  for(M in (Mmax+1):2) {
    b <- Inf
    L <- Kstar
    for(m in L) {
      K <- setdiff(L,m)
      dat <- data.frame(y = bwd_in$y, bwd_in$B[,K])
      lof <- LOF(y~., dat, control)
      if(lof < b) {
        b <- lof
        Kstar <- K
      }
      if(lof < LOFstar) {
        LOFstar <- lof
        Jstar <- K
      }
    }
  }
  Jstar <- c(1,Jstar)
  return(list(y = bwd_in$y, B = bwd_in$B[,Jstar], Bfuncs = bwd_in$Bfuncs[Jstar]))
}


#-----------------------init_B-----------------------#
init_B <- function(N,Mmax) {
  B <- data.frame(matrix(NA,nrow=N,ncol=(Mmax+1)))
  B[,1] <- 1
  names(B) <- c("B0",paste0("B",1:Mmax))
  return(B)
}


#-----------------------LOF_fwd----------------------#
LOF <- function(formula, data, control) {
  ff <- lm(formula, data)
  RSS <- sum(residuals(ff) ^ 2)
  MM <- length(ff$coefficients) - 1
  N <- nrow(data)
  C_M <- sum(hatvalues(ff))
  return (RSS  * N / (N - (C_M + control$d * MM)) ^ 2)
}


#-----------------------Hinge------------------------#
h <- function(s, x, t) {
  return(pmax(0, s * (x - t)))
}

#--------------------split_points--------------------#
split_points <- function(xv,Bm) {
  out <- sort(unique(xv[Bm>0]))
  return(out[-length(out)])
}


#-----------------------LOF_bwd----------------------#
lof <- function(form,data) {
  ff <- lm(form,data)
  return(sum(residuals(ff)^2))
}
