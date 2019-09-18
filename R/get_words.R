#' get_words function
#'
#' @param corpus_dates A vector of characters indicating the subfolders where are located the texts.
#' @param ntrms maximum numbers of words  that will be filtered by tf-idf. We rank the word by tf-idf in a decreasing order. Then, we select the words with the ntrms highest tf-idf.
#' @param st set 0 to stem the words and 1 otherwise.
#' @param path_name the folders path where the subfolders with the dates are located.
#' @param language The texts language.
#'
#' @return a list containing  a sparse matrix with the all words couting and another with a td-idf filtered words counting according to the ntrms.
#' @import tm
#' @import pdftools
#' @importFrom dplyr tbl_df
#' @importFrom plyr rbind.fill
#' @importFrom stats aggregate
#' @importFrom Matrix Matrix
#'
#' @export
#'
#' @examples
#' \donttest{
#' st_year=2017
#' end_year=2018
#' path_name=system.file("news",package="TextForecast")
#' qt=paste0(sort(rep(seq(from=st_year,to=end_year,by=1),12)),
#' c("m1","m2","m3","m4","m5","m6","m7","m8","m9","m10","m11","m12"))
#' z_wrd=get_words(corpus_dates=qt[1:23],path_name=path_name,ntrms=500,st=0)
#' }
#' path_name=system.file("news",package="TextForecast")
#' days=c("2019-31-01","2019-31-01")
#' z_wrd=get_words(corpus_dates=days,path_name=path_name,ntrms=500,st=0)
#'

get_words <- function(corpus_dates,ntrms,st,path_name,language) {
stop_words <- NULL


if(missing(language)){
  language = "english"
  }
  qtr <- corpus_dates
  options(stringAsFactors = FALSE)
  pathname=path_name


  if(st==0) {
    cleancorpus <- function(corpus) {
      corpus.tmp <-  tm::tm_map(corpus,removePunctuation)
      corpus.tmp1 <- tm::tm_map(corpus.tmp,content_transformer(tolower))
      corpus.tmp2 <- tm::tm_map(corpus.tmp1,stripWhitespace)
      corpus.tmp3 <- tm::tm_map(corpus.tmp2,removeNumbers)
      corpus.tmp4 <- tm::tm_map(corpus.tmp3,removeWords,c(tidytext::stop_words$word))
      corpus.tmp5 <- tm::tm_map(corpus.tmp4,removeWords,stopwords(language))
      corpus.tmp6 <- tm::tm_map(corpus.tmp5, stemDocument, language = language)
      return(corpus.tmp6)
    }
  } else {
    cleancorpus <- function(corpus) {
      corpus.tmp <-  tm::tm_map(corpus,removePunctuation)
      corpus.tmp1 <- tm::tm_map(corpus.tmp,content_transformer(tolower))
      corpus.tmp2 <- tm::tm_map(corpus.tmp1,stripWhitespace)
      corpus.tmp3 <- tm::tm_map(corpus.tmp2,removeNumbers)
      corpus.tmp4 <- tm::tm_map(corpus.tmp3,removeWords,c(stop_words$word))
      corpus.tmp5 <- tm_map(corpus.tmp4,removeWords,stopwords("english"))
      return(corpus.tmp5)
    }
  }

  Rpdf <- tm::readPDF(control = list(text = "-layout"))


  generateTDM <- function(cand,path) {
    s.dir <- sprintf("%s/%s",path,cand)
    s.cor <- tm::Corpus(DirSource(directory=s.dir,encoding = "UTF-8"), readerControl=list(reader = Rpdf) )
    s.cor.cl <- cleancorpus(s.cor)
    s.tdm <- tm::TermDocumentMatrix(s.cor.cl)
    #s.tdm <- removeSparseTerms(s.tdm,spar)
    #s.tdm=as.matrix(s.tdm)
    result <- list(name=cand,tdm=s.tdm)
    print(cand)
    return(result)
  }


  tdm=lapply(qtr,generateTDM, path=pathname)




  bindCandidateToDtm <- function(tdm) {
    s.mat <- t(data.matrix(tdm[["tdm"]]))
    s.df <- as.data.frame(s.mat,stringAsFactors=FALSE)
    s.df <- cbind(s.df,rep(tdm[["name"]],nrow(s.df)))
    colnames(s.df)[ncol(s.df)] <- "targetcandidate"
    return(s.df)
  }

  CandTdm <- lapply(tdm,bindCandidateToDtm)



  Tdm.stack <- do.call(rbind.fill,CandTdm)
  Tdm.stack[is.na(Tdm.stack)]=0
  Tdm.stack1 <- tbl_df(Tdm.stack)
  t=list(Tdm.stack$targetcandidate)
  Tdm.stack$targetcandidate <- NULL
  Tdm.sum <- aggregate(Tdm.stack,by=t,FUN=sum)
  data_words <- as.matrix(Tdm.sum[,2:ncol(Tdm.sum)])


  tfidf<- function(x) {
    xx <- as.matrix(x)
    ndoc <- nrow(x)

    II = xx>0
    II_sum <- apply(II,2,FUN=sum)
    nct <- II_sum
    idf <- log(ndoc/nct)

    xx_tfidf <- matrix(NA,nrow=nrow(x),ncol=ncol(x))
    #xx_log=log(1+xx)
    for (i in 1:ncol(x)) {
      xx_tfidf[,i] =idf[i]  * xx[,i]
    }
    xx_tfidf_sum <- apply(xx_tfidf,2,FUN=mean)
    return(xx_tfidf)
  }


  tfidfsum<- function(x) {
    xx <- as.matrix(x)
    ndoc <- nrow(x)

    II <- xx>0
    II_sum <- apply(II,2,FUN=sum)
    nct <- II_sum
    idf <- log(ndoc/nct)

    #xx_log=log(1+xx)

    xx_tfidf <- matrix(NA,nrow=nrow(x),ncol=ncol(x))
    #xx_log=log(1+xx)
    for (i in 1:ncol(x)) {
      xx_tfidf[,i] <- idf[i]*xx[,i]
    }

    xx_tfidf_sum <- apply(xx_tfidf,2,FUN=mean)
    xx_tfidf_sum[is.nan(xx_tfidf_sum)]=0
    return(xx_tfidf_sum)
  }


  tfidfsum1<- function(x) {
    xx <- as.matrix(x)
    ndoc <- nrow(x)

    II <- xx>0
    II_sum <- apply(II,2,FUN=sum)
    nct <- II_sum
    idf <- log(ndoc/nct)



    tf <- apply(xx,2,FUN=sum)

    xx_tfidf_sum <- tf*idf
    xx_tfidf_sum[is.nan(xx_tfidf_sum)]=0

    return(xx_tfidf_sum)
  }



  if(ncol(data_words)>ntrms) {
    m_data=tfidfsum1(data_words)
    m_data_srt=sort(m_data,decreasing=TRUE)
    meanfilter=m_data_srt[ntrms]
    II=m_data>=meanfilter
    data_words1=as.matrix(data_words[,II])
  } else {
    data_words1 <- as.matrix(data_words)
  }

  data_words <-  Matrix::Matrix(data_words, sparse = TRUE)
  data_words1 <-  Matrix::Matrix(data_words1, sparse = TRUE)
  data_lst = list(data_words,data_words1)
  return(data_lst)
}


