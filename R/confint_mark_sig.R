#' Confint mark significant
#'
#' @param data data.frame Confidence interval significance test: When the confidence interval is in the last column.  Significance test when the last two columns of data are confidence intervals
#' @param unite TRUE is combine CI and sig
#' @param digits default 3
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' lm(mpg ~ hp + wt, data=mtcars)%>%
#'  confint()%>%as.data.frame()%>%
#'   rownames_to_column()%>%
#'   confint_mark_sig()
#'
#' ## combind data unite=TRUE
#'   lm(mpg ~ hp + wt, data=mtcars)%>%
#'      confint()%>%
#'      as.data.frame()%>%
#'      rownames_to_column()%>%
#'      confint_mark_sig(unite=TRUE, digits=2)
#'
#'
#'
#'   }
#'
confint_mark_sig <- function(data, unite=FALSE,digits=3){

  data <- data %>% data.frame()
  ndata <- data%>%
    mutate(sig = ifelse( data[, ncol(data)-1]*data[, ncol(data)] > 0, "*","ns"))

  #변수를 CHARACTER로 변경
  ndata$sig <- as.character(ndata$sig)
  res = ndata %>% tibble::tibble() %>%
    dplyr::mutate_if(is.numeric, round, digits)



  if(unite){
    #  ci = res %>% select(ncol(res)-1, ncol(res))
    #  colnames(ci) = c("LCI","UCI")
    res = res %>% tidyr::unite(CI, ncol(res)-2, ncol(res)-1, sep=", ")
    # res = res %>% tidyr::unite(CI, ncol(res)-1, ncol(res), sig, sep=", ")
    res$CI = format(paste0("[",res$CI,"]"), digits)
    res = res %>% tidyr::unite(CI, CI, sig, sep="")
    res
  }else{
    res }
}
