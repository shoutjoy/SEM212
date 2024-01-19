#' The heterotrait-monotrait ratio of correlations (HTMT).



# The heterotrait-monotrait ratio of correlations (HTMT).
# 이종특성 -단일 특성 상관 관계 비율(HTMT)
#' Title
#'
#' @param model lavaan model syntax
#' @param dataset data.frame dataset
#' @param format "markdown' and 'html' and %>% kableExtra::kable_classic()
#' @param digits round default 3 digits
#' @param sample.cov  cov matrix
#' @param missing fill listwise
#' @param ordered default NULL
#' @param absolute default NULL
#' @param htmt2 default NULL
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#'
#'
#' library(lavaan)
#' model1 <- '
#'   # measurement model
#'     ind60 =~ x1 + x2 + x3
#'     dem60 =~ y1 + y2 + y3 + y4
#'     dem65 =~ y5 + y6 + y7 + y8
#'   # regressions
#'     dem60 ~ ind60
#'     dem65 ~ ind60 + dem60
#'   # residual correlations
#'     y1 ~~ y5
#'     y2 ~~ y4 + y6
#'     y3 ~~ y7
#'     y4 ~~ y8
#'     y6 ~~ y8
#' '
#'
#' fit1 <- sem(model1, data = PoliticalDemocracy)
#' # summary(fit2, standardized = TRUE)

#' HTMT(model = model1, dataset = PoliticalDemocracy)
#' }
#'
HTMT <- function(model= NA,
                 dataset=NA,
                 format="markdown", digits= 3,
                 sample.cov = NULL, missing = "listwise",
                 ordered = NULL, absolute = TRUE,
                 htmt2 = TRUE
){
  library(semTools)
  library(tidyverse)

  #test model syntax
  if( is.character(model)==TRUE |
      is.data.frame(dataset)==TRUE){

    options(knitr.kable.NA = '') #NA감추기

    #dataframe생성
    htmt0 <- semTools::htmt(model, dataset) %>%
      as.data.frame()

    htmt0[lower.tri(htmt0)==FALSE] <- 0 #대각성분을 0으로 만들기
    htmt0NA  <- htmt0 #NA데이터 처리
    htmt0NA[lower.tri(htmt0)==FALSE] <- NA   #상위성분 NA로 변경
    htmt1 <- htmt0 %>%   #유의성값 만들기
      mutate(Max = apply(htmt0, 1, max, na.rm=T),  #최댓값
             dis = ifelse(0.9 - Max== 0.9, 0, 0.9 - Max),  #판별
             sig = ifelse(0.9- Max >= 0,"*","ns")) #유의성

    htmt <- htmt1  %>%
      kable(format=format, digits=digits,
            caption="The heterotrait-monotrait ratio of correlations (HTMT).
          이종 특성 -단일 특성 상관 관계 비율(HTMT)
          All correalation < 0.9 --> discriminant Accept(robusrst)
          general accept: < 1
          (Henseler, Ringlet & Sarstedt, 2015)

          ")


  }else{
    htmt <- print("Not calculation HTMT, input syntax is [ model = lavaan model,dataset = data] ")

  }

  res = list(htmt,    #판별값
             semTools::htmt(model = model, data=dataset) %>%
               as.data.frame()   )#오리지널값
  res
}


