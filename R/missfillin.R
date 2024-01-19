#' Function to determine missing values and then fill in any missing values
#'
#' @param data data.frame
#' @param fn mean, median
#'
#' @return missing data fill in
#' @export
#'
#' @examples
#' \dontrun{
#' sleep$NonD %>% missfillin()
#' sleep$NonD %>% missfillin(fn=median)
#' sleep$Exp %>% missfillin()
#' }
#'
missfillin <- function(data, fn=mean){
  library(VIM)
  library(Hmisc)

  data<- as.data.frame(data)
  name <- colnames(data)

  if(aggr(data, prop=FALSE, numbers= TRUE, plot = FALSE)$missings[2]!=0){
    data <-  Hmisc::impute(unlist(data), fn(unlist(data), na.rm=T))
    # data <- as_tibble(data)
  }else{
    data
  }
  data
}
