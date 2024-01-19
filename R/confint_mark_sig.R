#' Confint mark significant
#'
#' @param data data.frame Confidence interval significance test: When the confidence interval is in the last column.  Significance test when the last two columns of data are confidence intervals
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
#'   }
#'
confint_mark_sig <- function(data){

  data <- data %>% data.frame()
  ndata <- data%>%
    mutate(sig = ifelse( data[, ncol(data)-1]*data[, ncol(data)] > 0, "*","ns"))

  #변수를 CHARACTER로 변경
  ndata$sig <- as.character(ndata$sig)
  ndata %>% tibble()

}
