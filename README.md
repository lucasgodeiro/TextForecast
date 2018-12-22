
<!-- README.md is generated from README.Rmd. Please edit that file -->
TextForecast Package
====================

The goal of TextForecast is to carries out forecasting and regression analysis using textual analysis and supervised machine learning techniques as LASSO, Elastic Net and Ridge Regression to select the mostpredictive words/terms.

Installation
------------

You can install the released version of TextForecast from github with:

``` r
install.packages("devtools")
library(devtools)
devtools::install_github("lucasgodeiro/TextForecast")
```

get\_words function
-------------------

### Arguments

**corpus\_dates:** A vector of characters indicating the subfolders where are located the texts.

**ntrms:** maximum numbers of words that will be filtered by tf-idf. We rank the word by tf-idf in a decreasing order. Then, after we select the words with the ntrms highst tf-idf.

**st:** set 0 to stem the words and 1 otherwise.

**path\_name:** the folders path where the subfolders with the dates are located.

### Value

a list containing a matrix with the all words couting and another with a td-idf filtered words couting according to the ntrms.

### Example

This is a basic example which shows you how to solve do a word counting form a PDF file.

``` r
## Example from function get_words. 
library(TextForecast)
st_year=2017
end_year=2018
path_name=system.file("news",package="TextForecast")
qt=paste0(sort(rep(seq(from=st_year,to=end_year,by=1),12)),c("m1","m2","m3","m4","m5","m6","m7","m8","m9","m10","m11","m12"))
z_wrd=get_words(corpus_dates=qt[1:2],path_name=path_name,ntrms=10,st=0)
#> [1] "2017m1"
#> [1] "2017m2"
zz=z_wrd[[2]]
head(zz)
#>      bitcoin broeksmit coop jain litvak liu presidentelect reagan ackman
#> [1,]      30        28   30   30     28  19             41     20      0
#> [2,]       0         0    0    0      0   0              0      0     19
#>      berkshir klarman valeant
#> [1,]        0       0       0
#> [2,]       29      19      27
```
