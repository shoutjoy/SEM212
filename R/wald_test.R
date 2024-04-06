
#' wald_test
#'
#' @param b1  fires coef
#' @param se1 firtse se
#' @param b2 secoond coef
#' @param se2 second se
#' @param est1 est1 name
#' @param est2 est2 name
#' @param digits round
#'
#' @return test result
#' @export
#'
#' @examples
#' \dontrun{
#' ##Clogg, C. C., Petkova, E., & Haritou, A. (1995).
#' Statistical methods for comparing regression coefficients between models.
#' American Journal of Sociology, 100(5), 1261-1293.
#' ##https://blog.naver.com/shoutjoy/223294372137
#'
#'  #' wald_test(-0.795,-0.795/-1.138, -0.795+0.149,  (-0.795+0.149)/1.344)
#'
#' wald_test(-0.795,-0.795/-1.138,-0.795+0.149,
#'           (-0.795+0.149)/1.344,
#'            est1 = "남성",
#'            est2 = "여성")
#'
#' }
#'
#'
# wald_test <- function(b1, se1, b2, se2, est1=NULL, est2=NULL){
#   library(broom)
#   t1 = b1/se1
#   t2 = b2/se2
#
#   z = (b1 - b2)/ sqrt(se1^2 + se2^2)
#   p = 2*(1-pnorm(abs(z)))
#
#   var1 = paste0(est1,"_est1 = ", b1,", se = ", round(se1, 3), ", t = ", t1,"." )
#   var2 = paste0(est2,"_est2 = ", b2,", se = ", round(se2, 3), ", t = ", t2,"." )
#
#   res = broom::tidy(c(z.value = z,  pvalue = p))
#   colnames(res)=c("statistics","value")
#
#   res = res%>% dplry::mutate(coef=c(var1, var2))
#   res
#   # list(res, var1, var2)
# }
#


wald_test <- function(b1,
                      se1,
                      b2,
                      se2,
                      est1=NULL,
                      est2=NULL,
                      digits = 3,
                      type="res2"){
  library(broom)
  library(dplyr)


  t1 = b1/se1
  t2 = b2/se2

  z = (b1 - b2)/ sqrt(se1^2 + se2^2)
  p = 2*(1-pnorm(abs(z)))

  var1 = paste0(est1,"_est1 = ", b1,", se = ", round(se1, digits),
                ", t = ", round(t1, digits),"." )
  var2 = paste0(est2,"_est2 = ", b2,", se = ", round(se2, digits),
                ", t = ", round(t2, digits),"." )

  res = data.frame(
    value = c( round(z, digits),
               round(p, digits))
                 )

  rownames(res) = c("z.value","pvalue")

  res = res %>%tibble::rownames_to_column("statistics")%>%tibble::tibble()



  # res = res%>%dplyr::mutate(coef = c(var1, var2))
  res1 = res %>%tibble::tibble()
  res2 = res%>%dplyr::mutate(coef = c(var1, var2)) %>%tibble::tibble()

  options(pillar.sigfig = digits)
  switch(type,
         res = res1,
         res1 = res1,
         res2 = res2,
         all = res2
         )
  # list(res, var1, var2)
}

#' wald_test2 input t value
#'
#' @param b1  fires coef
#' @param t1 firtse t
#' @param b2 secoond coef
#' @param t2 second t
#' @param est1 est1 name
#' @param est2 est2 name
#' @param gender For dichotomous variables with intercepts, the Gender
#' @param digits round
#' @param type res=res1. res2 = all
#'
#' @return ztest
#' @export
#'
#' @examples
#' \dontrun{
#'
#' wald_test2(-0.795,8.245, -0.795+0.149,  (-0.795+0.149)/1.344)
#'
#' wald_test2(-0.795,-0.795/-1.138,-0.795+0.149,
#'            (-0.795+0.149)/1.344,est1="남성",est2="여성")
#'
#'
#' ##gender variable
#' ##first method
#' wald_test2(1.311, 1.628, 1.311-0.366,  -2.732)
#' ##second method
#' wald_test2(1.311, 1.628, -0.366,  -2.732, gender=T)
#' wald_test2(1.311, 1.628, -0.366,  -2.732,"male","female", gender=T)
#'
#' }
#'
#'
#'
wald_test2 <- function(b1, t1, b2, t2,
                       est1 = NULL,
                       est2 = NULL,
                       gender = FALSE,
                       digits = 3,
                       type= "res1"){
  library(broom)
  library(dplyr)
  if(gender){

    se1 = b1/t1
    se2 = (b1+b2)/t2
    B1 = b1
    B2 = b1+b2

    z = (B1 - B2)/ sqrt(se1^2 + se2^2)
    p = 2*(1-pnorm(abs(z)))

    var1 = paste0(est1,"_est1(intercept) = ", b1,", se = ",
                  round(se1, digits), ", t = ",
                  round(t1,digits), "." )
    var2 = paste0(est2,"_est2","(",b2,") = ", B2,", se = ",
                  round(se2, digits), ", t = ",
                  round(t2, digits),"." )

    res = data.frame(
      value = c( round(z, digits),
                 round(p, digits))
    )

    rownames(res) = c("z.value","pvalue")
    res = res %>%tibble::rownames_to_column("statistics") %>% tibble::tibble()
    res1 = res%>%dplyr::mutate(coef = c(var1, var2))%>% tibble::tibble()
    # res %>% tibble::tibble()
  }else{
    se1 = b1/t1
    se2 = b2/t2

    z = (b1 - b2)/ sqrt(se1^2 + se2^2)
    p = 2*(1 - pnorm(abs(z)))

    var1 = paste0(est1,"_est1 = ", b1,", se = ", round(se1, digits), ", t = ",
                  round(t1, digits),"." )
    var2 = paste0(est2,"_est2 = ", b2,", se = ", round(se2, digits), ", t = ",
                  round(t2, digits),"." )

    res = data.frame(
      value = c( z, p))
    rownames(res) = c("z.value","pvalue")
    res = res %>%tibble::rownames_to_column("statistics")%>% tibble::tibble()
    res1 = res%>%dplyr::mutate(coef = c(var1, var2))%>% tibble::tibble()
  }


  options(pillar.sigfig = digits)
  switch(type,
         res = res,
         res1 = res,
         res2 = res1,
         all = res1
  )

}



#' wald_test2 input t value gender
#'
#' @param b1  fires coef
#' @param t1 firtse t
#' @param b2 secoond coef
#' @param t2 second t
#' @param est1 est1 name
#' @param est2 est2 name
#' @param gender For dichotomous variables with intercepts, the Gender
#' @param digits round
#'
#' @return ztest
#' @export
#'
#' @examples
#' \dontrun{
#'
#' wald_test2(-0.795,8.245, -0.795+0.149,  (-0.795+0.149)/1.344)
#'
#' #Regression: gender variable should consider intercept.
#' wald_test2(-0.795,-0.795/-1.138,-0.795+0.149,
#'            (-0.795+0.149)/1.344,est1="남성",est2="여성")
#'
#'
#' ##gender variable
#' ##first method
#' wald_test_gender(1.311, 1.628, 1.311-0.366,  -2.732)
#' ##second method
#' wald_test_gender(1.311, 1.628, -0.366,  -2.732, gender=T)
#' wald_test_gender(1.311, 1.628, -0.366,  -2.732,"male","female", gender=T)
#'
#' }
#'
#'
#'
wald_test_gender <- function(b1, t1, b2, t2,
                       est1 = NULL,
                       est2 = NULL,
                       gender = TRUE,
                       digits = 3){
  library(broom)
  library(dplyr)
  if(gender){

    se1 = b1/t1
    se2 = (b1+b2)/t2
    B1 = b1
    B2 = b1+b2

    z = (B1 - B2)/ sqrt(se1^2 + se2^2)
    p = 2*(1-pnorm(abs(z)))

    var1 = paste0(est1,"_est1(intercept) = ", b1,", se = ",
                  round(se1, digits), ", t = ",
                  round(t1,digits),"." )
    var2 = paste0(est2,"_est2","(",b2,") = ", B2,", se = ",
                  round(se2, digits), ", t = ",
                  round(t2, digits),"." )


    res = data.frame(
      value = c( z, p))

    rownames(res) = c("z.value","pvalue")
    res = res %>%tibble::rownames_to_column("statistics")
    res = res%>%dplyr::mutate(coef = c(var1, var2))
    # res %>% tibble::tibble()
  }else{
    se1 = b1/t1
    se2 = b2/t2

    z = (b1 - b2)/ sqrt(se1^2 + se2^2)
    p = 2*(1 - pnorm(abs(z)))

    var1 = paste0(est1,"_est1 = ", b1,", se = ", round(se1, digits), ", t = ",
                  round(t1, 3),"." )
    var2 = paste0(est2,"_est2 = ", b2,", se = ", round(se2, digits), ", t = ",
                  round(t2, 3),"." )

    res = data.frame(
      value = c( round(z, digits),
                 round(p, digits))
    )


    rownames(res) = c("z.value","pvalue")
    res = res %>%tibble::rownames_to_column("statistics")
    res = res%>%dplyr::mutate(coef = c(var1, var2))
  }

  res = res%>% tibble::tibble()
  options(pillar.sigfig = digits)
  print(res)


}




