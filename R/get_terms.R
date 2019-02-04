#' Title
#'
#' @param corpus_dates a character vector indicating the subfolders where the texts are located.
#' @param ntrms_words maximum numbers of words  that will be filtered by tf-idf. We rank the word by tf-idf in a decreasing order. Then, we select the words with the ntrms highest tf-idf.
#' @param st set 0 to stem the words and 1 otherwise.
#' @param path.name the folders path where the subfolders with the dates are located.
#' @param ntrms_collocation maximum numbers of collocations  that will be filtered by tf-idf. We rank the collocations by tf-idf in a decreasing order. Then, after we select the words with the ntrms highest tf-idf.
#' @param ngrams_number integer indicating the size of the collocations. Defaults to 2, indicating to compute bigrams. If set to 3, will find collocations of bigrams and trigrams.
#' @param min_freq integer indicating the frequency of how many times a collocation should at least occur in the data in order to be returned.
#' @param language the texts language. Default is english.
#'
#' @return a list containing  a matrix with the all collocations and words couting and another with a td-idf filtered collocations and words counting according to the ntrms.
#' @export
#'
#' @examples
#'\donttest{
#' st_year=2017
#' end_year=2018
#' path_name=system.file("news",package="TextForecast")
#' qt=paste0(sort(rep(seq(from=st_year,to=end_year,by=1),12)),
#' c("m1","m2","m3","m4","m5","m6","m7","m8","m9","m10","m11","m12"))
#' z_terms=get_terms(corpus_dates=qt[1:23],path.name=path_name,
#' ntrms_words=500,ngrams_number=3,st=0,ntrms_collocation=500,min_freq=10)
#' }
#' path_name=system.file("news",package="TextForecast")
#' days=c("2019-30-01","2019-31-01")
#' z_terms=get_terms(corpus_dates=days[1],path.name=path_name,
#' ntrms_words=500,ngrams_number=3,st=0,ntrms_collocation=500,min_freq=1)
#'


get_terms <- function(corpus_dates,ntrms_words,st,path.name,ntrms_collocation,ngrams_number,min_freq,language){


  if(missing(language)){
    language="english"
  }


  z_wrd=get_words(corpus_dates=corpus_dates,ntrms=ntrms_words,st=st,path_name=path.name,language=language)
  z_coll=get_collocations(corpus_dates=corpus_dates,path_name=path.name,ntrms=ntrms_collocation,ngrams_number=ngrams_number,min_freq = min_freq,language=language)

  z_full=cbind(z_wrd[[1]],z_coll[[1]])
  z_full=as.matrix(z_full)
  z_tf=cbind(z_wrd[[2]],z_coll[[2]])
  z_tf=as.matrix(z_tf)
  results = list(z_full,z_tf)
  return(results)
}


