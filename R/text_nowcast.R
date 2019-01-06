#' text nowcast
#'
#' @param x the input matrix x. It should have 1 observation more that y.
#' @param y the response variable
#' @param intercept TRUE for include intercept in the forecast equation.
#'
#' @importFrom stats lm
#'
#' @return the nowcast h=0 for the variable y.
#' @export
#'
#' @examples
#' set.seed(1)
#' data("stock_data")
#' data("news_data")
#' y=as.matrix(stock_data[,2])
#' w=as.matrix(stock_data[,3])
#' data("news_data")
#' data("optimal_factors")
#' pc=optimal_factors
#' z=cbind(w,pc)
#' t=length(y)
#' ncsts=text_nowcast(z,y[1:(t-1)],TRUE)

text_nowcast <- function(x,y,intercept) {
  y=as.vector(y)
  x=as.matrix(x)

  t=length(y)
  yy=y[1:t]
  xx=x[1:t,]


  if(intercept==TRUE){
    eq1=lm(yy~xx)
    betas=eq1$coef
    betas[is.na(betas)]=0
    nx2=x[(t+1),]
    ncsts1=t(c(1,nx2))%*%betas
  } else {
    eq1=lm(yy~xx+0)
    betas=eq1$coef
    betas[is.na(betas)]=0
    nx2=x[(t+1),]
    ncsts1=t(nx2)%*%betas
  }
  return(ncsts1)
}

