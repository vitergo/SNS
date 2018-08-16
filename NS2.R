#####################################################################################
#
#     CONDITIONAL NORMAL SCORES X FROM Y
#
#     Author: Dr. Víctor G. Tercero Gómez
#     Affiliation: Tecnologico de Monterrey
#
#     Date: February 19, 2018
#     Versión: 1.0
#
#     DESCRIPTION
#
#     Get Conditional Normal Score of X relative to Y incorporating given quantile.
#     If Y = NULL NS is relative to X.
#
#     X: is a numerical vector.
#     Y: is a numerical vector. Y = NULL if it is not defined.
#     theta: is a constant
#     Ftheta: is a constant between (0,1)
#
#     COMMENTS
#
#     If ties, average ranks are used.
#     If Y = NULL, normal scores are set relative to X.
#
#     EXAMPLE
#
#     Y = c(10,20,30,40,50,60,70,80,90,100)
#     X = c(30, 35, 45)
#     theta = 40
#     Ftheta = 0.5
#     > NS2(X = X, Y = Y, theta = theta, Ftheta = Ftheta)
#     [1] -0.52440051 -0.38532047  0.08964235
#
#     References
#     Conover, W. J., Tercero-Gómez, V. G., & Cordero-Franco, A. E. (2017).
#     The sequential normal scores transformation. Sequential Analysis, 36(3), 397-414.

NS2 <- function(X, Y = NULL, theta, Ftheta){
  #CASE 1: No previous data
  if(is.null(Y) == TRUE){
    Nminus = sum(X <= theta) #number <= than theta in batch.
    Nplus = sum(X > theta) #number > than theta in batch.
    nX = length(X) #number of normal scores needed
    r = rep(NA,nX) #initialize r
    p = rep(NA,nX) #initialize p
    z = rep(NA,nX) #initialize z
    for(i in 1:nX){
      if (X[i] <= theta){
        r[i] = sum(X < X[i] & X <= theta) + (sum(X == X[i] & X <= theta) + 1)/2
        p[i] = Ftheta * (r[i] - 0.5) / Nminus
      }
      if (X[i] > theta){
        r[i] = sum(X < X[i] & X > theta) + (sum(X == X[i] & X > theta) + 1)/2
        p[i] = Ftheta + (1 - Ftheta) * (r[i] - 0.5) / Nplus
      }
    }
    z = qnorm(p)
    return(z)
  }
  #CASE 2: Previous data does exist
  if(is.null(Y) == FALSE){
    Nminus = sum(Y <= theta) #numbers of <= theta used in individual ranking
    Nplus = sum(Y > theta) #number > theta used in individual ranking.
    nX = length(X) #number of normal scores needed
    r = rep(NA,nX) #initialize r
    p = rep(NA,nX) #initialize p
    z = rep(NA,nX) #initialize z
    for(i in 1:nX){
      if (X[i] <= theta){
        r[i] = sum(Y < X[i] & Y <= theta) + (sum(Y == X[i] & Y <= theta) + 2)/2
        p[i] = Ftheta * (r[i] - 0.5) / (Nminus + 1)
      }
      if (X[i] > theta){
        r[i] = sum(Y < X[i] & Y > theta) + (sum(Y == X[i] & Y > theta) + 2)/2
        p[i] = Ftheta + (1 - Ftheta) * (r[i] - 0.5) / (Nplus + 1)
      }
    }
    z = qnorm(p)
    return(z)
  }
}
