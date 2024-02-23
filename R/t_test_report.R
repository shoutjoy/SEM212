#' Advanceed t test report function
#' @param data data.frame
#' @param iv independent variable, if you want to change the name, you can change the setting
#' @param dv dependent variable, if you want to change the name, you can change the setting
#' @param xlab graph name dependent variable
#' @param ylab graph name dependent variable
#' @param type  type option default is 'all'. and  'var.test', 'var.test.report', 't.test','t.test.report', 'boxplot', 'var.test.full', 't.test.full', 'descriptive' output each result
#' @param translate report translate defualt FALSE, TRUE is korean
#' @param mu a number indicating the true value of the mean (or difference in means if you are performing a two sample test).
#' @param paired a logical indicating whether you want a paired t-test.
#' var.equal a logical variable indicating whether to treat the two variances as being equal. If TRUE then the pooled variance is used to estimate the variance otherwise the Welch (or Satterthwaite) approximation to the degrees of freedom is used.
#' @param conf.level confidence level of the interval.
#' @examples
#' # mtcars data
#' \dontrun{
#' ##default analysis
#' t_test_report(mtcars, iv="vs",dv="mpg")

#' ##$var_test_report
#'
#' ##
#' ##$t_test_result_tibble
#' ## A tibble: 1 × 7
#' ##IV    DV       df t.value   p.value sig   test_type
#' ##<chr> <chr> <dbl>   <dbl>     <dbl> <chr> <chr>
#' ##  1 vs    mpg      30   -4.86 0.0000342 ***   student t.test
#' ##
#' ##$t_test_report
#' ## New namaing xlab and ylab
#' t_test_report(mtcars, iv="vs",dv="mpg",xlab="transmission", ylab= "males per galon")
#' }
#' @export
#'
#'

t_test_report <- function(data,
                          iv = NULL,
                          dv = NULL,
                          type = "all",
                          xlab = "independent Variable",
                          ylab = "dependent Variable",
                          translate = FALSE,
                          mu = 0,
                          paired = FALSE,
                          conf.level = 0.95) {
  # library(tidyverse)

  # if(is.na(data)){
  #   stop("check your data exist NA")
  # }

  # Extract independent and dependent variables
  iv_value <- data[[iv]]
  dv_value <- data[[dv]]

  #descriptive statistics

  f_summary <- function(x, ...) c(n=length(x,...), mean=mean(x, ...), sd=sd(x, ...))
  descriptive = aggregate(formula( paste(dv, "~", iv) ),
                          data = data, FUN = f_summary) %>% tibble::tibble()

  ## mean calculation
  mean_data <- aggregate(formula( paste(dv, "~", iv) ), data = data, mean)%>%
    tibble::tibble()




  # extracted iv level named
  iv_name <-   unique(data[,iv] )
  # Test for homogeneity of variance
  var_test_result <- var.test(dv_value ~ iv_value)

  # Output the results of the homogeneity of variance test in tibble format
  var_test_result_tibble <- tibble::tibble(
    Var = unique(iv_value),
    df = var_test_result$parameter,
    f_value = var_test_result$statistic,
    p_value = var_test_result$p.value
  )

  var_test_report <- paste0(
    "The t-test results between the two groups '",iv_name[1], "' and '",iv_name[2],
    "' of the independent variable '",iv,"'",
    " with respect to the dependent variable '", dv, "' showed a",
    ifelse(var_test_result$p.value < 0.05, "statistically significant. F(",
           "not statistically significant. F("),
    var.test(dv_value ~ iv_value)$parameter[1], ",",
    var.test(dv_value ~ iv_value)$parameter[2], ") = ",
    round(var_test_result$statistic, 2), ",",
    " p = ",
    round(var_test_result$p.value, 2), ". ",
    ifelse(var_test_result$p.value < 0.05,
           "This indicates that the variances of the two groups are not equal..",
           "This indicates that the variances of the two groups are equal.")
  )



  # Perform t test

  #welch
  if (var_test_result$p.value < 0.05) {
    t_test_result <- t.test(dv_value ~ iv_value, var.equal = FALSE,
                            mu = mu,
                            paired = paired,
                            conf.level = conf.level)
    t_test_result_tibble <- tibble::tibble(
      DV = dv,
      IV = iv,
      IV.1 = paste0(iv_name[1],"(",round(mean_data[1, 2],2),")" ),
      IV.2 = paste0(iv_name[2],"(",round(mean_data[2, 2],2),")" ),
      df = t_test_result$parameter,
      t.value = t_test_result$statistic,
      p.value = t_test_result$p.value,
      sig  = dplyr::case_when( p.value < 0.001 ~ "***",
                               p.value < 0.01 ~ "**",
                               p.value < 0.05 ~ "*",
                               TRUE ~ "ns" ),
      test_type ="welch's t.test"
    )

    #using result report
    t_test_report <- paste0(
      "The t-test results between the two groups '",
      iv_name[1], "' and '",iv_name[2],
      "' of the independent variable '",iv,"' ",
      "with respect to the dependent variable '", dv, "' showed a",
      ifelse(t_test_result$p.value < 0.05,
             "statistically significant. t(",
             "not statistically significant. t(" ),
      round(t_test_result$parameter, 2), ") = ",
      round(t_test_result$statistic, 2), ",",
      ifelse(t_test_result$p.value< 0.001," p < .001",
             paste0("p = ",round(t_test_result$p.value, 2)))
      , ".")


    t_test_report_sub <- paste(
      "The t-test between the two groups '",
      iv_name[1], "' and '",iv_name[2],
      "' of the '",iv,"'",
      "'with respect to the '", dv, "' showed a",
      ifelse(t_test_result$p.value < 0.05,
             "significant, t(",
             "not significant, t(" ),
      round(t_test_result$parameter, 2), ") =",
      round(t_test_result$statistic, 2), ",",
      ifelse(t_test_result$p.value< 0.001," p < .001",
             paste0("p = ",round(t_test_result$p.value, 2)))
      , ".")





  } else if (var_test_result$p.value > 0.05){
    #student
    t_test_result <- t.test(dv_value ~ iv_value, var.equal = TRUE,
                            mu = mu,
                            paired = paired,
                            conf.level = conf.level)
    t_test_result_tibble <- tibble(
      DV = dv,
      IV = iv,
      IV.1 = paste0(iv_name[1],"(",round(mean_data[1, 2],2),")" ),
      IV.2 = paste0(iv_name[2],"(",round(mean_data[2, 2],2),")" ),
      df = t_test_result$parameter,
      t.value = t_test_result$statistic,
      p.value = t_test_result$p.value,
      sig = dplyr::case_when( p.value < 0.001 ~ "***",
                              p.value < 0.01 ~ "**",
                              p.value < 0.05 ~ "*",
                              TRUE ~ "ns" ),
      test_type = "student's t.test"
    )

    #using result report
    t_test_report <- paste0(
      "The t-test results between the two groups '",
      iv_name[1], "' and '",iv_name[2],
      "' of the independent variable ' ",iv," '",
      "'with respect to the dependent variable '", dv, "' showed a",
      ifelse(t_test_result$p.value < 0.05,
             "statistically significant. t(",
             "not statistically significant. t(" ),
      round(t_test_result$parameter, 2),") = ",
      round(t_test_result$statistic, 2), ",",
      ifelse(t_test_result$p.value< 0.001," p < .001",
             paste0("p = ",round(t_test_result$p.value, 2)))
      , ".")


    #using graph
    t_test_report_sub <- paste(
      "The t-test between the two groups '",
      iv_name[1], "' and '",iv_name[2],
      "' of the '",iv,"'",
      "'with respect to the '", dv, "' showed a",
      ifelse(t_test_result$p.value < 0.05,
             " significant, t(",
             "not significant, t(" ),
      round(t_test_result$parameter, 2), ") =",
      round(t_test_result$statistic, 2), ",",
      ifelse(t_test_result$p.value< 0.001," p < .001",
             paste0("p = ",round(t_test_result$p.value, 2)))
      , ".")


    # box plot output
    gg <- ggplot2::ggplot(data,
                 aes(x = factor(iv_value),
                     y = dv_value,
                     group = iv_value)) +
      geom_boxplot(color="black", fill=c("steelblue","gold")) +
      labs(title = "t-test Result and Comparison mean",
           x = xlab,
           y = ylab,
           subtitle = t_test_report_sub)+theme_bw()

### translate fucntion eng to kor
if(translate == TRUE){
  var_test_report = g(var_test_report,"en","ko",show = "data")
  t_test_report = g(t_test_report,"en","ko",show = "data")
}else if(translate == FALSE){
  var_test_report = g(var_test_report, "en","en",show = "data")
  t_test_report = g(t_test_report, "en","en",show = "data")
}



    # Output results
    switch(type,
           'all' = list(
             var_test_result_tibble = var_test_result_tibble,
             var_test_report = var_test_report,
             # var_test_report_kor = var_test_report %>% g("en","kr", show = "data"),
             t_test_result_tibble = t_test_result_tibble,
             t_test_report = t_test_report,
             # t_test_report_kor = t_test_report %>% g("en","ko", show = "data"),
             descriptive = descriptive,
             boxplot= gg),
           'var.test' = var_test_result_tibble,
           'var.test.report' = var_test_report,
           't.test' = t_test_result_tibble,
           't.test.report' =t_test_report,
           'boxplot' = gg,
           'descriptive' = descriptive ,
           'var.test.full'= var_test_result,
           't.test.full' = t_test_result
    )
  }
}





# t_test_report(mtcars, iv="vs",dv="mpg")
