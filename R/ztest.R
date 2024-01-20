#' Z test compare coefficient
#'
#' @param b1 b1 coeff
#' @param se1  se1 standard error
#' @param b2  b2 coeff
#' @param se2 se2 se
#'
#' @return z value and p value
#' @export
#'
#' @examples
#' \dontrun{
#' # hp, wt
#' ztest(b1= -0.032230,se1= 0.00892,   b2=-3.23 , se2= 0.796 )
#' }
#'
#'
ztest<- function(b1, se1, b2, se2){
  library(broom)

  z = (b1 - b2)/ sqrt(se1^2 + se2^2)
  p = 2*(1-pnorm(abs(z)))


  res = broom::tidy(c(z.value= z, pvalue=p))
  colnames(res)=c("statistics","value")
  res
}
