#####################################################################################
#
#     UNCONDITIONAL NORMAL SCORES X FROM Y
#
#     Author: Dr. Víctor G. Tercero Gómez
#     Affiliation: Tecnologico de Monterrey
#
#     Date: February 6, 2018
#     Versión: 1.0
#
#     DESCRIPTION
#
#     Get Normal Scores of X relative to Y. If Y = NULL NS is relative to X.
#
#     X: is a numerical vector.
#     Y: is a numerical vector. Y = NULL if it is not defined.
#
#     COMMENTS
#
#     If ties, average ranks are used.
#     If Y = NULL, normal scores are set relative to X.
#     Instead of Van Der Waerden Normal Scores where p = r/(n+1), p = (r-0.5)/n,
#     where r stands for rank and p for the input evaluated in the
#     inverse of a Standard Normal Distribution.
#
#     EXAMPLE
#
#     Y = c(10,20,30,40,50,60,70,80,90,100)
#     X = c(30, 35, 45)
#     > NS1(X = X, Y = Y)
#     [1] -0.6045853 -0.4727891 -0.2298841
#
#     References
#     Conover, W. J., Tercero-Gómez, V. G., & Cordero-Franco, A. E. (2017).
#     The sequential normal scores transformation. Sequential Analysis, 36(3), 397-414.

NS1 <- function(X, Y = NULL){
  if(is.null(Y) == TRUE){
    n = length(X) #all observations in the batch.
    r = rank(X)
    p = (r-0.5)/n
    z = qnorm(p)
    return(z)
  }
  if(is.null(Y) == FALSE){
    n = length(Y) + 1 #numbers of observations used in individual ranking
    nX = length(X) #number of normal scores needed
    r = rep(NA,nX)
    z = rep(NA,nX)
    for(i in 1:nX){
      r[i] = sum(Y < X[i]) + (sum(Y == X[i]) + 2)/2
    }
    p = (r-0.5)/n
    z = qnorm(p)
    return(z)
  }
}
