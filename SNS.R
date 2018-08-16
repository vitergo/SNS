#####################################################################################
#
#     SEQUENTIAL NORMAL SCORES
#
#     Author: Dr. Víctor G. Tercero-Gómez
#     Affiliation: Tecnologico de Monterrey
#
#     Date: February 19, 2018
#     Versión: 1.0
#
#     DESCRIPTION
#
#     Transform a vector X into SNS using initial observations Y if available
#     SNS follow the order of X.
#     Procedure follows Conover et al. (2017)
#
#     Function requires NS, NS1 and NS2 to work.
#
#     X: is a numerical vector.
#     Y: is a numerical vector. Y = NULL if undefined.
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
#     > NS(X = X, Y = Y, theta = theta, Ftheta = Ftheta)
#     [1] -0.52440051 -0.38532047  0.08964235
#
#     References
#     Conover, W. J., Tercero-Gómez, V. G., & Cordero-Franco, A. E. (2017).
#     The sequential normal scores transformation. Sequential Analysis, 36(3), 397-414.


SNS <- function(X, Y = NULL, theta = NULL, Ftheta = NULL){
  nX = length(X) #number of observations to be transformed into SNS
  z = rep(NA,nX) #initialize z
  nY = length(Y) #number of initial observations not transformed into SNS
  
  #Check for initial observations
  if(is.null(Y) == FALSE){
    Y = c(Y,X)
  }
  
  #Transform vector X into SNS
  for (i in 1:nX){
    z[i] = NS(X = X[i], Y = Y[1:(nY + i)], theta = theta, Ftheta = Ftheta)
  }
  return(z)
}
