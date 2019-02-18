#' get_collocations function
#'
#' @param corpus_dates a character vector indicating the subfolders where are located the texts.
#' @param path_name the folders path where the subfolders with the dates are located.
#' @param ntrms maximum numbers of collocations  that will be filtered by tf-idf. We rank the collocations by tf-idf in a decreasing order. Then, after we select the words with the ntrms highest tf-idf.
#' @param ngrams_number integer indicating the size of the collocations. Defaults to 2, indicating to compute bigrams. If set to 3, will find collocations of bigrams and trigrams.
#' @param min_freq integer indicating the frequency of how many times a collocation should at least occur in the data in order to be returned.
#' @param language the texts language. Default is english.
#'
#' @return a list containing  a matrix with the all collocations couting and another with a tf-idf filtered collocations counting according to the ntrms.
#' @import udpipe
#' @import tm
#' @import pdftools
#' @import SnowballC
#' @import rpart
#' @import tidytext
#' @import text2vec
#' @import class
#' @import rpart
#' @importFrom dplyr tbl_df
#' @importFrom plyr rbind.fill
#' @importFrom stats aggregate
#' @export
#'
#' @examples
#' \donttest{
#' st_year=2017
#' end_year=2018
#' path_name=system.file("news",package="TextForecast")
#' qt=paste0(sort(rep(seq(from=st_year,to=end_year,by=1),12)),
#' c("m1","m2","m3","m4","m5","m6","m7","m8","m9","m10","m11","m12"))
#' z_coll=get_collocations(corpus_dates=qt[1:23],path_name=path_name,
#' ntrms=500,ngrams_number=3,min_freq=10)
#' }
#' path_name=system.file("news",package="TextForecast")
#' days=c("2019-30-01","2019-31-01")
#' z_coll=get_collocations(corpus_dates=days[1],path_name=path_name,
#' ntrms=500,ngrams_number=3,min_freq=1)
#'



get_collocations <- function(corpus_dates,path_name,ntrms,ngrams_number,min_freq,language) {
ngram <-NULL
freq <- NULL

 if(missing(language)){
   language="english"
   }

  qtr <- corpus_dates
  options(stringAsFactors = FALSE)
  pathname=path_name
  ud_model <- udpipe_download_model(language = language)
  ud_model <- udpipe_load_model(ud_model$file_model)



  cleancorpus <- function(corpus) {
    corpus.tmp <-  tm_map(corpus,removePunctuation)
    corpus.tmp1 <- tm_map(corpus.tmp,content_transformer(tolower))
    corpus.tmp2 <- tm_map(corpus.tmp1,stripWhitespace)
    corpus.tmp3 <- tm_map(corpus.tmp2,removeNumbers)
    corpus.tmp4 <- tm_map(corpus.tmp3,removeWords,c(tidytext::stop_words$word))
    corpus.tmp5 <- tm_map(corpus.tmp4,removeWords,stopwords("english"))
    #corpus.tmp10 <- tm_map(corpus.tmp9, stemDocument, language = "english")
    return(corpus.tmp5)
  }

  Rpdf <- readPDF(control = list(text = "-layout"))


  generateTDM <- function(cand,path,ngrams_number,min_freq) {
    s.dir <- sprintf("%s/%s",path,cand)
    s.cor <- Corpus(DirSource(directory=s.dir,encoding = "UTF-8"),readerControl=list(reader = Rpdf) )
    s.cor.cl <- cleancorpus(s.cor)
    ff=tidy(s.cor.cl)
    #ud_model <- udpipe_download_model(language = "english")
    #ud_model <- udpipe_load_model(ud_model$file_model)
    x <- udpipe_annotate(ud_model, x = ff$text, doc_id = ff$id)
    x <- as.data.frame(x)
    #col_type<- c_type
    #stats <- keywords_rake(x = x, term = "lemma", group = "doc_id",
    # relevant = x$upos %in% col_type)
    stats <- keywords_collocation(x = x, term = "lemma", group = "doc_id", n_min=1,ngram_max=ngrams_number)
    stats <- subset(stats, ngram > 1 & freq > 1)
    #stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
    #barchart(key ~ rake, data = head(subset(stats, freq > 3), 20), col = "cadetblue",
    #        main = "Keywords identified by RAKE",
    #       xlab = "Rake")
    coll <- subset(stats, freq >= min_freq)
    s.tdm <- document_term_frequencies(coll,  term = "keyword")
    #s.tdm <- TermDocumentMatrix(s.cor.cl)
    #s.tdm <- removeSparseTerms(s.tdm,spar)
    s.tdm <- document_term_matrix(s.tdm)
    s.tdm_m=base::as.matrix(s.tdm)
    s.tdm_sum=apply(s.tdm_m,1,FUN = sum)
    s.tdm.sum_rk=sort(s.tdm_sum,decreasing = TRUE)
    if(length(s.tdm_sum)>50){
      s=s.tdm.sum_rk[50]
    } else {
      s=0
    }
    #s=1
    s.tdm <- dtm_remove_lowfreq(s.tdm, minfreq = s)
    print(cand)
    print(Sys.time())
    #print(ncol(as.matrix(s.tdm)))
    result <- list(name=cand,tdm=s.tdm)
    return(result)
  }







  tdm=lapply(qtr,generateTDM, path=pathname,ngrams_number=ngrams_number,min_freq=min_freq)




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

  tfidfsum1<- function(x) {
    xx=as.matrix(x)
    ndoc=nrow(x)

    II = xx>0
    II_sum = apply(II,2,FUN=sum)
    nct=II_sum
    idf=log(ndoc/nct)



    tf=apply(xx,2,FUN=sum)

    xx_tfidf_sum=tf*idf
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
    data_words1=as.matrix(data_words)
  }
  data_lst = list(data_words,data_words1)
  return(data_lst)


}

