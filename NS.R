#####################################################################################
#
#     NORMAL SCORES X FROM Y
#
#     Author: Dr. Víctor G. Tercero-Gómez
#     Affiliation: Tecnologico de Monterrey
#
#     Date: February 19, 2018
#     Versión: 1.0
#
#     DESCRIPTION
#
#     Get conditional and unconditional normal score of X relative to Y.
#     If Y = NULL NS is relative to X.
#
#     Function requires NS1 and NS2 to work.
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
#     > NS(X = X, Y = Y, theta = theta, Ftheta = Ftheta)
#     [1] -0.52440051 -0.38532047  0.08964235
#
#     References
#     Conover, W. J., Tercero-Gómez, V. G., & Cordero-Franco, A. E. (2017).
#     The sequential normal scores transformation. Sequential Analysis, 36(3), 397-414.

NS <- function(X, Y = NULL, theta = NULL, Ftheta = NULL){
  #Lets check for errors
  if(is.null(theta) != is.null(Ftheta)){ #in case one is NULL and not the other
    print("ERROR, theta or Ftheta missing")
    return()
  }
  
  #Caculations start here
  n = length(X) #normal scores needed
  z = rep(NA,n) #initialize z
  if(is.null(theta) == TRUE | is.null(Ftheta) == TRUE){
    z = NS1(X = X, Y = Y)
    return(z)
  }else{
    z = NS2(X = X, Y = Y, theta = theta, Ftheta = Ftheta)
    return(z)
  }
}
