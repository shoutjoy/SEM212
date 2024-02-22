
#' centering each vector
#'
#' @param variable  vector variable in data
#' @param na.rm missing data chedk
#' @export
#'
#'
meanCentering <- function(variable, na.rm = TRUE){
  res = variable - mean(variable, na.rm = na.rm )
  res
}



#' centering each vector
#'
#' @param variable  vector variable in data
#' @param na.rm missing data chedk
#' @export
#'
#'
centering <- function(variable, na.rm = TRUE){
  res = variable - mean(variable, na.rm = na.rm )
  res
}
