#' Top Terms Function
#'
#' @param x the input matrix of terms to be selected.
#' @param w optional argument. the input matrix of structured data to not be selected.
#' @param y the response variable
#' @param alpha the glmnet alpha
#' @param lambda the glmnet lambda
#' @param k the k top terms
#' @param wordcloud set TRUE to plot the wordcloud
#' @param max.words the maximum number of words in the wordcloud
#' @param scale the wordcloud size.
#' @param rot.per wordcloud proportion 90 degree terms
#' @param family glmnet family
#'
#' @import wordcloud
#' @import glmnet
#' @import ggplot2
#' @import RColorBrewer
#' @importFrom RColorBrewer brewer.pal
#'
#' @return the top k terms and the corresponding wordcloud.
#' @export
#'
#' @examples
#' \donttest{
#' set.seed(1)
#' data("stock_data")
#' data("news_data")
#' y=as.matrix(stock_data[,2])
#' w=as.matrix(stock_data[,3])
#' data("news_data")
#' X=news_data[,2:ncol(news_data)]
#' x=as.matrix(X)
#' grid_alphas=seq(by=0.05,to=0.95,from=0.05)
#' cont_folds=TRUE
#' t=length(y)
#' optimal_alphas=optimal_alphas(x[1:(t-1),],w[1:(t-1),],
#' y[2:t],grid_alphas,TRUE,"gaussian")
#' top_trms<- top_terms(x[1:(t-1),],w[1:(t-1),],y[2:t],
#' optimal_alphas[[1]], optimal_alphas[[2]],10,TRUE,
#' 10,c(2,0.3),.15,"gaussian")
#' }
#'
top_terms <-function(x,w,y, alpha,lambda,k,wordcloud,max.words,scale,rot.per,family){
  if(missing(family)){
    family="gaussian"
  }


  if(missing(w)) {
    trms <- tv_dictionary(x=x,y=y,alpha=alpha,lambda=lambda,newx=x,family=family)
    betas=trms[[2]]
    betas=as.matrix(betas[2:nrow(betas)])
  } else {
    y=as.vector(y)
    x=as.matrix(x)
    w=as.matrix(w)
    nw=ncol(as.matrix(w))
    nx=ncol(as.matrix(x))
    z=cbind(w,x)
    nz=ncol(z)

    trms <- tv_dictionary(x=x,y=y,w=w,alpha=alpha,lambda=lambda,newx=x,family = family)

    betas=trms[[2]]
    betas=as.matrix(betas[(nw+2):nrow(betas)])

  }

  II_rank=as.matrix(rank(-abs(betas),ties.method = "random"))
  betas_names=colnames(as.data.frame(x))

  betas_abs=abs(betas)

  II=betas!=0
  III=sum(II)

  if(III<k){
    k=III
  }

  II_rank_k=II_rank[1:k]

  top_coefs <- vector(length=k)


  for (p in 1:k){
    gg = which(II_rank==p)
    top_coefs[p]=betas_names[gg]
  }

  if(wordcloud==TRUE) {



    freqs=vector(length = k)

    for(p in 1:k){
      gg = which(II_rank==p)
      freqs[p]=betas_abs[gg]
    }
    pal2 <- RColorBrewer::brewer.pal(8,"Dark2")
    wordcloud(top_coefs,freqs, colors = pal2,random.order=FALSE,
              max.words=max.words,rot.per = rot.per ,scale=scale )
  }


  return(top_coefs)
}
