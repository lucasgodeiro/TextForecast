#' tf-idf function
#'
#' @param x a input matrix x of terms counting.
#'
#' @return a list with the terms tf-idf and the terms tf-idf in descending order.
#' @importFrom dplyr arrange
#' @importFrom dplyr desc
#' @importFrom dplyr tibble
#' @export
#'
#' @examples
#' data("news_data")
#' X=as.matrix(news_data[,2:ncol(news_data)])
#' tf_idf_terms = tf_idf(X)
  tf_idf<- function(x) {
  xx=as.matrix(x)
  ndoc=nrow(x)

  II = xx>0
  II_sum = apply(II,2,FUN=sum)
  nct=II_sum
  idf=log(ndoc/nct)



  tf=apply(xx,2,FUN=sum)

  xx_tfidf_sum=tf*idf
  xx_tfidf_sum[is.nan(xx_tfidf_sum)]=0
  names_xx=colnames(x)

  xx_df=dplyr::tibble(terms=names_xx,tf_idf=as.numeric(xx_tfidf_sum))
  xx_df_srt <- dplyr::arrange(xx_df,desc(tf_idf))
  results=list(xx_df,xx_df_srt)
  return(results)
}
