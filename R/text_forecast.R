#' Text Forecast function
#'
#' @param x the input matrix x.
#' @param y the response variable
#' @param h the forecast horizon
#' @param intercept TRUE for include intercept in the forecast equation.
#'
#' @importFrom stats lm
#' @return The h step ahead forecast
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
#' fcsts=text_forecast(z,y,1,TRUE)

text_forecast <- function(x,y,h,intercept) {
  y=as.vector(y)
  x=as.matrix(x)

  t=length(y)
  yy=y[(h+1):t]
  xx=x[1:(t-h),]


  if(intercept==TRUE){
    eq1=lm(yy~xx)
    betas=eq1$coef
    betas[is.na(betas)]=0
    nx2=x[t,]
    fcsts1=t(c(1,nx2))%*%betas
  } else {
    eq1=lm(yy~xx+0)
    betas=eq1$coef
    betas[is.na(betas)]=0
    nx2=x[t,]
    fcsts1=t(nx2)%*%betas
  }
  return(fcsts1)
}
