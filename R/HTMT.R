#' The heterotrait-monotrait ratio of correlations (HTMT).
#' @param model 	lavaan model.syntax of a confirmatory factor analysis model where at least two factors are required for indicators measuring the same construct.
#' @param dataset A data.frame or data matrix
#' @param format 'markdown' and 'html' and kableExtra::kable_classic()
#' @param digits round default 3 digits
#' @param sample.cov  A covariance or correlation matrix can be used, instead of data, to estimate the HTMT values.
#' @param missing If "listwise", cases with missing values are removed listwise from the data frame. If "direct" or "ml" or "fiml" and the estimator is maximum likelihood, an EM algorithm is used to estimate the unrestricted covariance matrix (and mean vector). If "pairwise", pairwise deletion is used. If "default", the value is set depending on the estimator and the mimic option (see details in lavCor).
#' @param ordered Character vector. Only used if object is a data.frame. Treat these variables as ordered (ordinal) variables. Importantly, all other variables will be treated as numeric (unless is.ordered in data). (see also lavCor)
#' @param absolute logical indicating whether HTMT values should be estimated based on absolute correlations (default is TRUE). This is recommended for HTMT but required for HTMT2 (so silently ignored).
#' @param htmt2 logical indicating whether to use the geometric mean (default, appropriate for congeneric indicators) or arithmetic mean (which assumes tau-equivalence).
#'
#'
#' @examples
#' # example code
#' \dontrun{
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
#' ## covariance
#' HS.cov <- cov(HolzingerSwineford1939[, paste0("x", 1:9)])
#' ## HTMT using arithmetic mean
#' htmt(HS.model, sample.cov = HS.cov, htmt2 = FALSE)
#' }
#'
#' @export

HTMT <- function(model= NA,
                 dataset=NA,
                 format="markdown",
                 digits= 3,
                 sample.cov = NULL,
                 missing = "listwise",
                 ordered = NULL,
                 absolute = TRUE,
                 htmt2 = TRUE
){
  library(semTools)
  library(tidyverse)

  #test model syntax
  if( is.character(model)==TRUE |
      is.data.frame(dataset)==TRUE){

    options(knitr.kable.NA = '') # hide NA

    # generate data.frame
    htmt0 <- semTools::htmt(model, dataset,sample.cov = NULL, missing = "listwise",
                            ordered = NULL, absolute = TRUE, htmt2 = TRUE) %>%
      as.data.frame()

    htmt0[lower.tri(htmt0)==FALSE] <- 0 # diag to 0
    htmt0NA  <- htmt0 #NA data
    htmt0NA[lower.tri(htmt0)==FALSE] <- NA   # upper
    htmt1 <- htmt0 %>%
      dplyr::mutate("Max" = apply(htmt0, 1, max, na.rm=T),  #max value
             dis = ifelse(0.9 - Max == 0.9, 0, 0.9 - Max),  #discriminant
             sig = ifelse(0.9- Max >= 0,"*","ns")) #sig

    htmt <- htmt1  %>%
      knitr::kable(format = format,
            digits = digits,
            caption="The heterotrait-monotrait ratio of correlations (HTMT).
          All correalation < 0.9 --> discriminant Accept(robusrst)
          general accept: < 1
          (Henseler, Ringlet & Sarstedt, 2015)

          ")


  }else{
    htmt <- print("Not calculation HTMT, input syntax is [ model = lavaan model,dataset = data] ")

  }

  res = list(htmt,
             semTools::htmt(model = model, data=dataset) %>%
               as.data.frame()   )#
  res
}


