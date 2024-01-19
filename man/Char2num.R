#' character datq tranfomation double
#'
#' @param data data.frame or matrix
#' @param colnumber 'auto', 'manual'
#' @param iter iteration n
#'
#' @return char to numeric
#' @export
#'
#' @examples
Char2num <- function(data, colnumber ="auto", iter=NULL){
  if(colnumber =="auto"){
    iter=1:ncol(data)
  }else if(colnumber =="manual"){
    iter = iter
  }

  for( i in iter){
    data[[i]] <- suppressWarnings(as.numeric(data[[i]]))
    data[[i]][is.na(data[[i]])  ] <- 0
  }

  data
}
