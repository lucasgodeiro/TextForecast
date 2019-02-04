###This functions computes the optimal alphas.

#### @param x A matrix of variables to be selected by shrinkrage methods.

#### @ param w A matrix or vector of variables that cannot be selected(no shrinkrage).

#### @param grid_alphas A grid of alphas between 0 and 1.

#### @param cont_folds Set TRUE for contiguos folds used in time depedent data.

##### @return lambdas_opt a vector with the optimal alpha and lambda.

####@example ########################################################
#set.seed(1)
#y=rnorm(200)
#w=replicate(2,rnorm(200))
#x=replicate(100,rnorm(200))
#grid_alphas=seq(by=0.05,to=0.95,from=0.05)
##cont_folds=TRUE
#cont_folds=FALSE
#optimal_alphas=opt_alpha(x=x,y=y,grid_alphas=grid_alphas,cont_folds = TRUE)


#' Title optimal alphas function
#'
#' @param x A matrix of variables to be selected by shrinkrage methods.
#' @param w A matrix or vector of variables that cannot be selected(no shrinkrage).
#' @param y response variable.
#' @param grid_alphas a grid of alphas between 0 and 1.
#' @param cont_folds Set TRUE for contiguous folds used in time depedent data.
#' @param family The glmnet family.
#'
#' @return lambdas_opt a vector with the optimal alpha and lambda.
#'
#' @import glmnet
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
#' grid_alphas=seq(by=0.25,to=1,from=0.5)
#' cont_folds=TRUE
#' t=length(y)
#' optimal_alphas=optimal_alphas(x[1:(t-1),],
#' w[1:(t-1),],y[2:t],grid_alphas,TRUE,"gaussian")
#'
optimal_alphas <- function(x,w,y, grid_alphas,cont_folds,family) {
if(missing(family)){
  family="gaussian"
}

  if(missing(w)){
    y=as.vector(y)
    x=as.matrix(x)
    alphas=grid_alphas

    nalp=nrow(as.matrix(alphas))
    lambdas.min=array(0,dim=c(1,nalp))
    mse.min=matrix(0,nrow=1,ncol=nalp)
    alphas.min=matrix(0,nrow=1,ncol=1)
    lambdas=matrix(0,nrow=1,ncol=1)


    if(cont_folds==TRUE) {
      foldid=sort(sample(1:10,size=length(y),replace=TRUE)  )
    }else{
      foldid=sample(1:10,size=length(y),replace=TRUE)
    }
    for (i in 1:nalp)
    {
      cv=cv.glmnet(x,y,foldid=foldid,alpha=alphas[i],family=family)
      lmcv=cv$lambda.min==cv$lambda
      lambdas.min[,i]=cv$lambda.min
      mse.min[,i]=cv$cvm[lmcv]
      print(i)
    }
    II=which(mse.min[,1:nalp]==min(mse.min[,1:nalp]))
    alphas.min=alphas[II]
    lambdas=lambdas.min[,II]



  }else {

    y=as.vector(y)
    x=as.matrix(x)
    w=as.matrix(w)
    alphas=grid_alphas

    nalp=nrow(as.matrix(alphas))
    lambdas.min=array(0,dim=c(1,nalp))
    mse.min=matrix(0,nrow=1,ncol=nalp)
    alphas.min=matrix(0,nrow=1,ncol=1)
    lambdas=matrix(0,nrow=1,ncol=1)

    if(cont_folds==TRUE) {
      foldid=sort(sample(1:10,size=length(y),replace=TRUE)  )
    }else{

      foldid=sample(1:10,size=length(y),replace=TRUE)
    }

    nw=ncol(w)
    nx=ncol(x)
    pw=rep(0,nw)
    px=rep(1,nx)
    pf=c(pw,px)
    z=cbind(w,x)
    for (i in 1:nalp)
    {
      cv=cv.glmnet(z,y,foldid=foldid,alpha=alphas[i],penalty.factor=pf,family=family)
      lmcv=cv$lambda.min==cv$lambda
      lambdas.min[,i]=cv$lambda.min
      mse.min[,i]=cv$cvm[lmcv]
      print(i)
    }
    II=which(mse.min[,1:nalp]==min(mse.min[,1:nalp]))
    alphas.min=alphas[II]
    lambdas=lambdas.min[,II]




  }

  lambdas_opt=c(alphas.min,lambdas)
  return(lambdas_opt)
}

