#' report nomality , t.test , var.test
#'
#' @param data test result
#' @param type normality, t.test, var.test(leven)
#' @param digits  default 3
#' @param trans translate TRUE korean language
#' @param show  normal is source and output, data is output
#'
#' @return report data
#' @export
#'
#' @examples
#' \dontrun{
#'
#' shapiro.test(final) %>% report_stat()
#'
#' var.test(mpg ~ vs, data = mtcars)%>%
#' report_stat("var.test")
#'
#' car::leveneTest(mpg~ factor(vs), mtcars)%>%
#'  report_stat("leven.test")
#'
#' car::leveneTest(mpg~ factor(vs), mtcars)%>%
#' report_stat("leven.test", trans=TRUE)
#'
#'
#' fligner.test(mpg ~ vs,data= mtcars) %>%
#' report_stat("t.test")
#'
#' t.test(mpg ~ vs, data= mtcars, var.equal = FALSE)%>%
#' report_stat("t.test")
#'
#' t.test(len ~ supp, data = ToothGrowth, var.equal=TRUE) %>%
#' report_stat("t.test")
#'
#' shapiro.test(final) %>% report_stat()%>% jjstat::k("en","ko")
#' shapiro.test(final) %>% report_stat(trans =TRUE)
#'
#' car::leveneTest(mpg~ factor(vs), mtcars)%>%
#' report_stat("leven.test", trans=TRUE)
#'
#'
#' }
#'
report_stat <- function(data, type="normality",
                        digits = 3,
                        trans=FALSE, show="data") {
  # Extract relevant information from the data
  data = broom::tidy(data)
  #   #
  if(type=="leven.test"){
    data<- data %>% dplyr::mutate(method="Leven test")
    # data <- data %>% transform(method="Leven test")
  }else{
    data
  }

  method <- data$method
  statistic <- data$statistic
  p_value <- data$p.value


  if(p_value < 0.001){p_value_res <- "< .001"
  }else{
    p_value_res <- paste0("= ", round(data$p.value, digits))}
  # Determine the significance based on p-value

  if(type=="t.test"){
    significance <- ifelse(p_value < 0.05, "It was statistically significant t = ", "It was not statistically significant. t = ")

  }else if(type =="normality"){
    significance <- ifelse(p_value < 0.05, "Statistically significant, the null hypothesis was rejected and normality was not secured.  statistic = ",
                           "the normality was secured by rejecting the alternative hypothesis because it was not statistically significant. statistic = ")

  }else if(type =="var.test"| type=="leven.test"){
    significance <- ifelse(p_value < 0.05, "Statistically significant, the null hypothesis is rejected, so equidistribution can not be assumed.  F = ",
                           "It was not statistically significant, and the null hypothesis can be adopted to assume equal variance. F = ")
  }
  # Create the sentence based on the method and p-value
  result_sentence <- paste0( method," showed that ",
                             significance,
                             #  "(stat = ",
                             #  statistic,
                             round(statistic ,digits),
                             ", p ",
                             p_value_res, ")")
  #  p_value , ")")

  # Print the result
  if(trans){
    jjstat::kakaoi(result_sentence,"en","ko", show=show)
  }else{
    result_sentence
  }

}




#' var.test report
#'
#' @param var_test var.test result
#' @param trans tans true english to korean
#'
#' @return explain result apa
#' @export
#'
#' @examples
#' \dontrun{
#' var.test(mpg ~ vs, data = mtcars)%>%
#'      report_vartest()
#'
#' var.test(mpg ~ vs, data = mtcars)%>%
#'      report_vartest() %>%
#'      jjstat::kakaoi()
#'
#' var.test(mpg ~ vs, data = mtcars)%>%
#' report_vartest(trans=TRUE)
#'
#' }
report_vartest = function(var_test, trans = FALSE){
  var_test_resul0<-  broom::tidy(var_test)
  var_test_result<-  var_test

  dd = var_test_result$data.name%>% strsplit(" by ") %>% unlist()
  dv = dd[1]
  iv = dd[2]
  iv_name_1 =  attributes(var_test_result$parameter)[[1]][1]
  iv_name_2 =  attributes(var_test_result$parameter)[[1]][2]

  # Output the results of the homogeneity of variance test in tibble format
  var_test_result_tibble <- tibble::tibble(
    Var = iv,
    df = var_test_result$parameter,
    f_value = var_test_result$statistic,
    p_value = var_test_result$p.value
  )

  var_test_report <- paste0(
    "The t-test results between the two groups '",iv_name_1, "' and '",iv_name_2,
    "' of the independent variable '",iv,"'",
    " with respect to the dependent variable '", dv, "' showed a",
    ifelse(var_test_result$p.value < 0.05, "statistically significant. F(",
           "not statistically significant. F("),
    var_test_result$parameter[1], ", ",
    var_test_result$parameter[2], ") = ",
    round(var_test_result$statistic, 2), ",",
    " p = ",
    round(var_test_result$p.value, 2), ". ",
    ifelse(var_test_result$p.value < 0.05,
           "This indicates that the variances of the two groups are not equal..",
           "This indicates that the variances of the two groups are equal.")
  )
  if(trans){
    jjstat::kakaoi(var_test_report)
  }else{ var_test_report}
}



