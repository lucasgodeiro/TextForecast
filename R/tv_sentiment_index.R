#' tv sentiment index function
#'
#' @param x A matrix of variables to be selected by shrinkrage methods.
#' @param w Optional Argument. A matrix of variables to be selected by shrinkrage methods.
#' @param y the response variable.
#' @param alpha the alpha required in glmnet.
#' @param lambda the lambda required in glmnet.
#' @param newx Matrix  that selection will be applied. Useful for time series, when we need the observation at time t.
#' @param family the glmnet family.
#' @param k the highest positive and negative coefficients to be used.
#'
#' @import glmnet
#' @import ggplot2
#' @import tidytext
#' @import forcats
#' @importFrom dplyr tibble
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr ungroup
#' @importFrom dplyr top_n
#'
#'
#' @return The time-varying sentiment index. The index is based on the word/term counting and is computed using: tv_index=(pos-neg)/(pos+neg).
#' @export
#'
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
#' optimal_alphas=optimal_alphas(x[1:(t-1),],w[1:(t-1),],
#' y[2:t],grid_alphas,TRUE,"gaussian")
#' tv_index <- tv_sentiment_index(x[1:(t-1),],w[1:(t-1),],y[2:t],
#' optimal_alphas[[1]],optimal_alphas[[2]],x,"gaussian",2)
#'
tv_sentiment_index <- function(x,w,y, alpha, lambda,newx,family,k){

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
      eq=glmnet(x,y, alpha=0.5, lambda=cv$lambda.1se,family=family)
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

    if(sum(II_pos) < k | sum(II_neg) < k ){
      stop("The k chosen is greater than the coefficients number. Try another k.")
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
      eq=glmnet(z,y, alpha=0.5, lambda=cv$lambda.1se,family=family)
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

    if(sum(II_pos) < k | sum(II_neg) < k ){
      stop("The k chosen is greater than the coefficients number. Try another k.")
    }


  }







  names_co=colnames(as.data.frame(x))
  co_df=dplyr::tibble(term=names_co, estimate=as.vector(co))

  coefs <- co_df


  print(
    coefs %>%
      group_by(estimate > 0) %>%
      top_n(k, abs(estimate)) %>%
      ungroup() %>%
      ggplot(aes(fct_reorder(term, estimate), estimate, fill = estimate > 0)) +
      geom_col(alpha = 0.8, show.legend = FALSE) +
      coord_flip() +
      labs(
        x = NULL,
        title = "Most positive and negative coefficients ",
        subtitle = "This chart depicts the highest and lowest k coefficients"
      )
  )

  coefs1 <- coefs %>%
    group_by(estimate > 0) %>%
    top_n(k, abs(estimate)) %>%
    ungroup()

  pos_trm <- coefs1 %>%
    dplyr::filter(estimate > 0)


  neg_trm <- coefs1 %>%
    dplyr::filter(estimate < 0)

  names_sx=colnames(as.data.frame(newx))


  II_pos_trm <- vector(length=k)
  II_neg_trm <- vector(length=k)

  for ( i in 1:k) {
    II_pos_trm[i]=which(pos_trm$term[i]==names_sx)
    II_neg_trm[i]=which(neg_trm$term[i]==names_sx)
  }


  sx_pos=newx[,II_pos_trm]
  sx_neg=newx[,II_neg_trm]

  sx_pos_sum=apply(sx_pos,1,FUN = sum)
  sx_neg_sum=apply(sx_neg,1,FUN = sum)


  tv_sent_index=(sx_pos_sum-sx_neg_sum)/(sx_pos_sum+sx_neg_sum)
  tv_sent_index[is.na(tv_sent_index)]=0
  return(tv_sent_index)

}
