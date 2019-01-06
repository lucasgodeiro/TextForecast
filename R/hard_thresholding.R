#' hard thresholding
#'
#' @param x the input matrix x.
#' @param w the optional input matrix w, that cannot be selected.
#' @param y the response variable.
#' @param p_value the threshold p-value.
#' @param newx matrix  that selection will applied. Useful for time series, when we need the observation at time t.
#'
#' @importFrom stats lm
#'
#' @return the variables less than p-value.
#' @export
#' @examples
#' data("stock_data")
#' data("optimal_factors")
#' y=as.matrix(stock_data[,2])
#' y=as.vector(y)
#' w=as.matrix(stock_data[,3])
#' pc=as.matrix(optimal_factors)
#' t=length(y)
#' news_factor <- hard_thresholding(w=w[1:(t-1),],x=pc[1:(t-1),],y=y[2:t],p_value = 0.01,newx = pc)

#'
hard_thresholding <- function(x,w,y,p_value,newx) {
  if(missing(w)){
    y=as.vector(y)
    x=as.matrix(x)
    newx=as.matrix(newx)


    eq=summary(lm(y~x))
    I=eq$coef[2:nrow(eq$coef),4]<p_value
    if (sum(I)==0){
      I=rank(eq$coef[2:nrow(eq$coef),4])<2
    }
    sc=newx[,which(I)]
    sc=as.matrix(sc)



  } else {
    y=as.vector(y)
    x=as.matrix(x)
    w=as.matrix(w)
    newx=as.matrix(newx)

    nw=ncol(w)
    nx=ncol(x)
    z=cbind(w,x)
    nz=ncol(z)




    eq=summary(lm(y~z))
    I=eq$coef[(nw+2):nrow(eq$coef),4]< p_value
    if (sum(I)==0){
      I=rank(eq$coef[(nw+2):nrow(eq$coef),4])<2
    }
    sc=newx[,which(I)]
    sc=as.matrix(sc)



  }





  return(sc)
}
