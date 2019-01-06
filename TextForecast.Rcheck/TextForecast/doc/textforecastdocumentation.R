## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>" , warning=FALSE, results = "hide",eval=FALSE
)

## ----get words example, results = "hide",eval=FALSE----------------------
#  ## Example from function get_words.
#  library(TextForecast)
#  st_year=2017
#  end_year=2018
#  path_name=system.file("news",package="TextForecast")
#  qt=paste0(sort(rep(seq(from=st_year,to=end_year,by=1),12)),
#  c("m1","m2","m3","m4","m5","m6","m7","m8","m9","m10","m11","m12"))
#  z_wrd=get_words(corpus_dates=qt[1:6],path_name=path_name,ntrms=10,st=0)
#  zz=z_wrd[[2]]
#  head(zz)

## ----get collocations example, results = "hide",eval=FALSE---------------
#  library(TextForecast)
#  st_year=2017
#  end_year=2018
#  path_name=system.file("news",package="TextForecast")
#  qt=paste0(sort(rep(seq(from=st_year,to=end_year,by=1),12)),
#  c("m1","m2","m3","m4","m5","m6","m7","m8","m9","m10","m11","m12"))
#  z_coll=get_collocations(corpus_dates=qt[1:23],path_name=path_name,
#  ntrms=20,ngrams_number=3,min_freq=10)
#  zz=z_coll[[2]]
#  #head(zz)
#  knitr::kable(head(zz, 23))
#  

## ----get terms example, results = "hide", eval=FALSE---------------------
#  library(TextForecast)
#  st_year=2017
#  end_year=2018
#  path_name=system.file("news",package="TextForecast")
#  qt=paste0(sort(rep(seq(from=st_year,to=end_year,by=1),12)),
#  c("m1","m2","m3","m4","m5","m6","m7","m8","m9","m10","m11","m12"))
#  z_terms=get_terms(corpus_dates=qt[1:23],path.name=path_name,ntrms_words=10,
#  ngrams_number=3,st=0,ntrms_collocation=10,min_freq=10)
#  zz=z_terms[[2]]
#  #head(zz,23)
#  knitr::kable(head(zz, 23))

## ----tf-idf example, results = "hide"------------------------------------
#  library(TextForecast)
#   data("news_data")
#   X=as.matrix(news_data[,2:ncol(news_data)])
#    tf_idf=tf_idf(X)
#    head(tf_idf[[1]])

## ----optimal alphas example, results = "hide"----------------------------
#  library(TextForecast)
#   set.seed(1)
#   data("stock_data")
#   data("news_data")
#   y=as.matrix(stock_data[,2])
#   w=as.matrix(stock_data[,3])
#   data("news_data")
#   X=news_data[,2:ncol(news_data)]
#   x=as.matrix(X)
#   grid_alphas=seq(by=0.05,to=0.95,from=0.05)
#   cont_folds=TRUE
#   t=length(y)
#   optimal_alphas=optimal_alphas(x[1:(t-1),],
#   w[1:(t-1),],y[2:t],grid_alphas,TRUE,"gaussian")
#   print(optimal_alphas)

## ----tv dictionary example, results = "hide"-----------------------------
#   library(TextForecast)
#   set.seed(1)
#   data("stock_data")
#   data("news_data")
#   y=as.matrix(stock_data[,2])
#   w=as.matrix(stock_data[,3])
#   data("news_data")
#   X=news_data[,2:ncol(news_data)]
#   x=as.matrix(X)
#   grid_alphas=seq(by=0.05,to=0.95,from=0.05)
#   cont_folds=TRUE
#   t=length(y)
#   optimal_alphas=optimal_alphas(x=x[1:(t-1),],w=w[1:(t-1),],
#   y=y[2:t],grid_alphas=grid_alphas,cont_folds=TRUE,family="gaussian")
#   x_star=tv_dictionary(x=x[1:(t-1),],w=w[1:(t-1),],
#   y=y[2:t],alpha=optimal_alphas[1],lambda=optimal_alphas[2],newx=x,family="gaussian")
#   optimal_alphas1=optimal_alphas(x=x[1:(t-1),],y=y[2:t],
#   grid_alphas=grid_alphas,cont_folds=TRUE,family="gaussian")
#   x_star1=tv_dictionary(x=x[1:(t-1),],y=y[2:t],alpha=optimal_alphas1[1],
#   lambda=optimal_alphas1[2],newx=x,family="gaussian")

## ----optimal factor example, results = "hide"----------------------------
#  library(TextForecast)
#  data("optimal_x")
#  optimal_factor <- TextForecast::optimal_factors(data=optimal_x,kmax=8)
#  head(optimal_factor[[1]])

## ----hard thresholding example-------------------------------------------
#  library(TextForecast)
#  data("stock_data")
#  data("optimal_factors")
#  y=as.matrix(stock_data[,2])
#  y=as.vector(y)
#  w=as.matrix(stock_data[,3])
#  pc=as.matrix(optimal_factors)
#  t=length(y)
#  news_factor <- hard_thresholding(w=w[1:(t-1),],
#  x=pc[1:(t-1),],y=y[2:t],p_value = 0.01,newx = pc)

## ----Text Forecast Example, results = "hide"-----------------------------
#  library(TextForecast)
#  set.seed(1)
#  data("stock_data")
#  y=as.matrix(stock_data[,2])
#  w=as.matrix(stock_data[,3])
#  data("optimal_factors_data")
#  pc=as.matrix(optimal_factors)
#  z=cbind(w,pc)
#  fcsts=text_forecast(z,y,1,TRUE)
#  print(fcsts)

## ----Text Nowcast Example, results = "hide"------------------------------
#   library(TextForecast)
#   set.seed(1)
#   data("stock_data")
#    data("news_data")
#   y=as.matrix(stock_data[,2])
#   w=as.matrix(stock_data[,3])
#   data("news_data")
#   data("optimal_factors_data")
#   pc=as.matrix(optimal_factors)
#   z=cbind(w,pc)
#   t=length(y)
#   ncsts=text_nowcast(z,y[1:(t-1)],TRUE)
#   print(ncsts)

## ----Top Terms Example, results = "hide"---------------------------------
#  library(TextForecast)
#   set.seed(1)
#   data("stock_data")
#   data("news_data")
#   y=as.matrix(stock_data[,2])
#   w=as.matrix(stock_data[,3])
#   data("news_data")
#   X=news_data[,2:ncol(news_data)]
#   x=as.matrix(X)
#   grid_alphas=seq(by=0.05,to=0.95,from=0.05)
#   cont_folds=TRUE
#   t=length(y)
#   optimal_alphas=optimal_alphas(x[1:(t-1),],w[1:(t-1),],
#   y[2:t],grid_alphas,TRUE,"gaussian")
#   top_trms<- top_terms(x[1:(t-1),],w[1:(t-1),],y[2:t],optimal_alphas[[1]],
#  optimal_alphas[[2]],10,TRUE,10,c(5,0.15),.15,"gaussian")

## ----TV sentiment index example, results = "hide", eval=FALSE------------
#  library(TextForecast)
#   set.seed(1)
#   data("stock_data")
#   data("news_data")
#   y=as.matrix(stock_data[,2])
#   w=as.matrix(stock_data[,3])
#   data("news_data")
#   X=news_data[,2:ncol(news_data)]
#   x=as.matrix(X)
#   grid_alphas=seq(by=0.05,to=0.95,from=0.05)
#   cont_folds=TRUE
#   t=length(y)
#   optimal_alphas=optimal_alphas(x[1:(t-1),],w[1:(t-1),],
#   y[2:t],grid_alphas,TRUE,"gaussian")
#    tv_index <- tv_sentiment_index(x[1:(t-1),],w[1:(t-1),],
#   y[2:t],optimal_alphas[[1]],optimal_alphas[[2]],x,"gaussian",2)
#   head(tv_index)

