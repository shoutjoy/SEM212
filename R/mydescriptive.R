#' mydescriptive 'n','mean','sd','min','max'
#'
#'
#' @param myvariable variable vector
#' @param digits digits
#' @export
#' @examples
#' \dontrun{
#'
#' mydescriptive(mtcars$mpg)
#'
#' }
#'
mydescriptive <- function(myvariable, digits = 3){
  mysize <- length(myvariable)
  mymean <- round(mean(myvariable),digits)
  mysd <- round(sd(myvariable),digits)
  mymin <- round(min(myvariable),digits)
  mymax <- round(max(myvariable),digits)
  mydes <- matrix(c(mysize, mymean, mysd, mymin, mymax), ncol=5)
  colnames(mydes) <- c('n','mean','sd','min','max')
  mydes
}




#' mydes 'n','mean','sd','min','max'
#'
#'
#' @param myvariable variable vector
#' @param digits digits
#' @export
#' @examples
#' \dontrun{
#'
#' mydes(mtcars$mpg)
#'
#' }
#'
mydes <- function(myvariable, digits = 2){
  N <- length(myvariable)
  Mean <- round(mean(myvariable),digits)
  SD <- round(sd(myvariable),digits)
  Min <- round(min(myvariable),digits)
  Max <- round(max(myvariable),digits)
  Skew <- round(SKEW(myvariable),digits)
  Kurt <- round(KURT(myvariable),digits)

  mydes <- cbind.data.frame(N, Mean, SD, Min, Max, Skew, Kurt)
  # colnames(mydes) <- c('n','mean','sd','min','max')
  mydes <- tibble::tibble(mydes)
  mydes
}




#' my summary descriptive statistics
#'
#' @param myobject data.frame, matrix
#' @param myvariable column variable
#'
#' @return size   MEAN    SD  MIN  MAX
#' @export
#'
#' @examples
#' \dontrun{
#' mysummary(mtcars, "mpg")
#' }
mysummary <- function(myobject, myvariable){
  # myobject[[myvariable]]
  myresult <- dplyr::summarise(myobject,

                               size = length(myobject[[myvariable]]),
                               MEAN = mean(myobject[[myvariable]]),
                               SD = sd(myobject[[myvariable]]),
                               MIN = min(myobject[[myvariable]]),
                               MAX = max(myobject[[myvariable]]))
  round(myresult,3)
}



