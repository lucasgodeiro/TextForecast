#' TV sentiment index using all positive and negative coefficients.
#'
#' @param x A matrix of variables to be selected by shrinkrage methods.
#' @param w Optional Argument. A matrix of variables to be selected by shrinkrage methods.
#' @param y the response variable.
#' @param alpha the alpha required in glmnet.
#' @param lambda the lambda required in glmnet.
#' @param newx Matrix  that selection will be applied. Useful for time series, when we need the observation at time t.
#' @param family the glmnet family.
#' @param scaled Set TRUE for scale and FALSE for no scale.
#' @param k_mov_avg The moving average order.
#' @param type_mov_avg The type of moving average. See \link[pracma]{movavg}.
#'
#' @import glmnet
#' @importFrom pracma movavg
#'
#' @return A list with the net, postive and negative sentiment index. The net time-varying sentiment index. The index is based on the word/term counting and is computed using: tv_index=(pos-neg)/(pos+neg). The postive sentiment index is computed using: tv_index_pos=pos/(pos+neg) and the negative tv_index_neg=neg/(pos+neg).
#' @export
#'
#' @examples
#' suppressWarnings(RNGversion("3.5.0"))
#' set.seed(1)
#' data("stock_data")
#' data("news_data")
#' y=as.matrix(stock_data[,2])
#' w=as.matrix(stock_data[,3])
#' data("news_data")
#' X=news_data[,2:ncol(news_data)]
#' x=as.matrix(X)
#' grid_alphas=0.05
#' cont_folds=TRUE
#' t=length(y)
#' optimal_alphas=optimal_alphas(x=x[1:(t-1),],
#'                               y=y[2:t],grid_alphas=grid_alphas,cont_folds=TRUE,family="gaussian")
#' tv_idx=tv_sentiment_index_all_coefs(x=x[1:(t-1),],y=y[2:t],alpha = optimal_alphas[1],
#'                                  lambda = optimal_alphas[2],newx=x,
#'                                  scaled = TRUE,k_mov_avg = 4,type_mov_avg = "s")

tv_sentiment_index_all_coefs <- function(x,w,y, alpha, lambda,newx,family,scaled,k_mov_avg,type_mov_avg){

  estimate <- NULL
  term <- NULL

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
      eq=glmnet(x,y, alpha=0.25, lambda=cv$lambda.1se,family=family)
      co=as.matrix(eq$beta)
      II2=co!=0
    }

    if (sum(II2)==0){
      eq=glmnet(x,y, alpha=0, lambda=0,family=family)
      co=as.matrix(eq$beta)
      II2=co!=0
    }

    II_pos = co > 0
    II_neg = co < 0

    if(sum(II_pos) == 0 | sum(II_neg) == 0 ){
      stop("There is no positive or negative coefficients. Try another alphas and lambdas.")
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
    alphas=alpha
    lambdas=lambda

    if (sum(II2)==0){
      cv=cv.glmnet(z,y)
      eq=glmnet(z,y, alpha=0.25, lambda=cv$lambda.1se,family=family)
      co=as.matrix(eq$beta[(nw+1):nz])
      II2=co!=0
    }

    if (sum(II2)==0){
      eq=glmnet(z,y, alpha=0, lambda=0,family=family)
      co=as.matrix(eq$beta[(nw+1):nz])
      II2=co!=0
    }

    II_pos = co > 0
    II_neg = co < 0

    if( sum(II_pos) == 0 | sum(II_neg) == 0 ){
      stop("There is no positive or negative coefficients. Try another alphas and lambdas.")
    }



  }


  new_pos=as.matrix(newx[,which(II_pos)])
  new_neg=as.matrix(newx[,which(II_neg)])
  sx_pos_sum = apply(new_pos,1,FUN = sum)
  sx_neg_sum = apply(new_neg,1,FUN = sum)
  wrd_tot=sx_pos_sum+sx_neg_sum
  stmt_tv=(sx_pos_sum-sx_neg_sum)/(1+wrd_tot)
  stmt_tv_pos=sx_pos_sum/(1+wrd_tot)
  stmt_tv_neg=sx_neg_sum/(1+wrd_tot)


  if(missing(k_mov_avg) & missing(type_mov_avg)){
    si_tv = stmt_tv
    si_tv_pos = stmt_tv_pos
    si_tv_neg = stmt_tv_neg
  } else {
    si_tv = movavg(stmt_tv,k_mov_avg,type_mov_avg)
    si_tv_pos = movavg(stmt_tv_pos,k_mov_avg,type_mov_avg)
    si_tv_neg = movavg(stmt_tv_neg,k_mov_avg,type_mov_avg)

    }

  if(scaled==TRUE) {
    si_tv1 = scale(si_tv)
    si_tv_pos1 = scale(si_tv_pos)
    si_tv_neg1 = scale(si_tv_neg)
    }


  if(scaled==FALSE) {
    si_tv1 = si_tv
    si_tv_pos1 = si_tv_pos
    si_tv_neg1 = si_tv_neg
  }

  tv_sent_index=list(si_tv1,si_tv_pos1,si_tv_neg1)

  return(tv_sent_index)

}
