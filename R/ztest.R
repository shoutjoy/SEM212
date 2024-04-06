#' Z test compare coefficient
#'
#' @param b1 b1 coeff
#' @param se1  se1 standard error
#' @param b2  b2 coeff
#' @param se2 se2 se
#' @param type result type res1, res2
#' @param digits rounding defualt 3
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
ztest<- function(b1, se1, b2, se2, type="res", digits = 3){
  library(broom)
  library(dplyr)
  t1 = b1/se1
  t2 = b2/se2

  z = (b1 - b2)/ sqrt(se1^2 + se2^2)
  p = 2*(1-pnorm(abs(z)))

  var1 = paste0("est1 = ", b1,", se = ", round(se1, digits), ", t = ",
                round(t1, digits),"." )
  var2 = paste0("est2 = ", b2,", se = ", round(se2, digits), ", t = ",
                round(t2, digits),"." )

  res = data.frame(
    value = c( z, p))

  rownames(res) = c("z.value","pvalue")
  res = res %>%tibble::rownames_to_column("statistics")

  res1 = res
  res2 = res%>%dplyr::mutate(coef = c(var1, var2))

  switch(type,
         res = res1,
         res2 = res2)

}

