#' optimal number of factors function
#'
#' @param x a matrix x.
#' @param kmax the maximum number of factors
#'
#' @importFrom stats prcomp
#' @return a list with the optimal factors.
#' @export
#'
#' @examples
#' data("optimal_x")
#' optimal_factor <- optimal_number_factors(x=optimal_x,kmax=8)
optimal_number_factors <-function(x,kmax){

  X=as.matrix(x)




  T=nrow(X)
  N=ncol(X)
  K=kmax

  if (N<T) {
    xx=(t(X)%*%X)/(T*N)
  } else {
    xx=(X%*%t(X))/(T*N)
  }


  eig <- eigen(xx)
  a=eig$values;
  d=rev(sort(a));


  #xx=(t(X)%*%as.matrix(X))/(N*T)

  m=min(N,T);


  eing_values <- d


  ER <-  matrix(0,K-1,1)

  for (k in 1:(K-1)) {
    ER[k,1]= eing_values[k]/eing_values[(k+1)]
  }
  ER[is.na(ER)]=0
  n_fac=max(ER)

  number_factors=which(n_fac==ER)
  sx=prcomp(X,scale=TRUE,center=TRUE,retx=TRUE)
  sx_opt=as.matrix(sx$x[,1:number_factors])
  results=list(number_factors,sx_opt)
  return(results)
}
