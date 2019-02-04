#' tv dictionary function
#'
#' @param x A matrix of variables to be selected by shrinkrage methods.
#' @param w Optional Argument. A matrix of variables to be selected by shrinkrage methods.
#' @param y the response variable.
#' @param alpha the alpha required in glmnet.
#' @param lambda the lambda required in glmnet.
#' @param newx Matrix  that selection will applied. Useful for time series, when we need the observation at time t.
#' @param family the glmnet family.
#'
#' @return X_star: a list with the coefficients and a matrix with the most predictive terms.
#'
#' @import glmnet
#' @importFrom stats coef
#' @export
#'
#' @examples
#'
#' set.seed(1)
#' data("stock_data")
#' data("news_data")
#' y=as.matrix(stock_data[1:200,2])
#' w=as.matrix(stock_data[1:200,3])
#' data("news_data")
#' X=news_data[1:200,2:ncol(news_data)]
#' x=as.matrix(X)
#' grid_alphas=seq(by=0.5,to=1,from=0.5)
#' cont_folds=TRUE
#' t=length(y)
#' optimal_alphas=optimal_alphas(x[1:(t-1),],w[1:(t-1),],
#' y[2:t],grid_alphas,TRUE,"gaussian")
#' x_star=tv_dictionary(x=x[1:(t-1),],w=w[1:(t-1),],y=y[2:t],
#' alpha=optimal_alphas[1],lambda=optimal_alphas[2],newx=x,family="gaussian")
#'


tv_dictionary <-function(x,w,y, alpha, lambda,newx,family) {
  if(missing(family)){
    family="gaussian"
  }


  if(missing(w)) {
    y=as.vector(y)
    x=as.matrix(x)
    newx=as.matrix(newx)

    eq=glmnet(x,y, alpha=alpha,lambda=lambda,family=family)
    co=as.matrix(eq$beta)
    II2=co!=0
    if (sum(II2)==0){
      cv=cv.glmnet(x,y)
      eq=glmnet(x,y, alpha=0.5, lambda=cv$lambda.1se,family=family)
      co=as.matrix(eq$beta)
      II2=co!=0
    }
    if (sum(II2)==0){
      eq=glmnet(x,y, alpha=0, lambda=0,family=family)
      co=as.matrix(eq$beta)
      II2=co!=0
    }

  } else {
    y=as.vector(y)
    x=as.matrix(x)
    w=as.matrix(w)
    newx=as.matrix(newx)


    nw=ncol(w)
    nx=ncol(x)
    pw=rep(0,nw)
    px=rep(1,nx)
    pf=c(pw,px)
    z=cbind(w,x)
    nz=ncol(z)


    eq=glmnet(z,y, alpha=alpha,lambda=lambda,penalty.factor = pf,family=family)
    co=as.matrix(eq$beta[(nw+1):nz])
    II2=co!=0
    if (sum(II2)==0){
      cv=cv.glmnet(x,y)
      eq=glmnet(z,y, alpha=0.5, lambda=cv$lambda.1se,family=family)
      co=as.matrix(eq$beta[(nw+1):nz])
      II2=co!=0
    }
    if (sum(II2)==0){
      eq=glmnet(z,y, alpha=0, lambda=0,family=family)
      co=as.matrix(eq$beta[(nw+1):nz])
      II2=co!=0
    }




  }
  coef_est=coef(eq)
  sx=as.matrix(subset(newx,select=II2))
  results=list(sx,coef_est)
  return(results)
}
