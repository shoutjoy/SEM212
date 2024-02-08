#' mydescriptive 'n','mean','sd','min','max'
#'
#'
#' @param myvariable variable vector
#' @export
#' @examples
#' \dontrun{
#'
#' mydescriptive(mtcars$mpg)
#'
#' }
#'
mydescriptive <- function(myvariable){
  mysize <- length(myvariable)
  mymean <- round(mean(myvariable),3)
  mysd <- round(sd(myvariable),3)
  mymin <- round(min(myvariable),3)
  mymax <- round(max(myvariable),3)
  mydes <- matrix(c(mysize,mymean,mysd,mymin,mymax),ncol=5)
  colnames(mydes) <- c('n','mean','sd','min','max')
  mydes
}
