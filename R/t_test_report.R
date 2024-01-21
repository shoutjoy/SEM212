#' Advanceed t test report function
#' @param data data.frame
#' @param iv independent variable
#' @param dv dependent variable
#' @param type  type option default is 'all'. and  'var.test', 'var.test.report', 't.test','t.test.report', 'boxplot', 'var.test.full', 't.test.full' output each result
#' @examples
#' # mtcars data
#' \dontrun{
#' t_test_report(mtcars, iv="vs",dv="mpg")
#' }
#' @export
#'
#'

t_test_report <- function(data, iv=NULL, dv = NULL, type = "all") {
  library(tidyverse)


  # Extract independent and dependent variables
  iv_value <- data[[iv]]
  dv_value <- data[[dv]]

  # Test for homogeneity of variance
  var_test_result <- var.test(dv_value ~ iv_value)

  # Output the results of the homogeneity of variance test in tibble format
  var_test_result_tibble <- tibble(
    Var = unique(iv_value),
    df = var_test_result$parameter,
    f_value = var_test_result$statistic,
    p_value = var_test_result$p.value
  )
  var_test_report <- paste(
    "The test of equality of variances between independent variable '",
    iv, "' and dependent variable '", dv, "' was",
    ifelse(var_test_result$p.value < 0.05, "statistically significant, F(", "not statistically significant, F("),
    var.test(dv_value ~ iv_value)$parameter[1], ",",
    var.test(dv_value ~ iv_value)$parameter[2], ") = ",
    round(var_test_result$statistic, 2), ",",
    " p = ",
    round(var_test_result$p.value, 2), ",",
    ifelse(var_test_result$p.value < 0.05, "This suggests that the variances of the two groups are not equal.", "This suggests that the variances of the two groups are equal.")
  )


  # Perform t test

  #welch
  if (var_test_result$p.value < 0.05) {
    t_test_result <- t.test(dv_value ~ iv_value, var.equal = FALSE)
    t_test_result_tibble <- tibble(
      IV = iv,
      DV = dv,
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
    t_test_report <- paste(
      "The t-test between independent variable '", iv,
      "' and dependent variable '", dv,
      "' was",
      ifelse(t_test_result$p.value < 0.05,
             "statistically significant, t(",
             "not statistically significant, t(" ),
      round(t_test_result$parameter, 2), ") =",
      round(t_test_result$statistic, 2), ",",
      "p = ", round(t_test_result$p.value, 2), ".")

    t_test_report_sub <- paste(
      "The t-test between '", iv,
      "' and '", dv,
      "' was",
      ifelse(t_test_result$p.value < 0.05,
             "significant, t(",
             "not significant, t(" ),
      round(t_test_result$parameter, 2), ") =",
      round(t_test_result$statistic, 2), ",",
      "p = ", round(t_test_result$p.value, 2), ".")



  } else if (var_test_result$p.value > 0.05){
    #student
    t_test_result <- t.test(dv_value ~ iv_value,  var.equal = TRUE)
    t_test_result_tibble <- tibble(
      IV = iv,
      DV = dv,
      df = t_test_result$parameter,
      t.value = t_test_result$statistic,
      p.value = t_test_result$p.value,
      sig = dplyr::case_when( p.value < 0.001 ~ "***",
                              p.value < 0.01 ~ "**",
                              p.value < 0.05 ~ "*",
                              TRUE ~ "ns" ),
      test_type = "student t.test"
    )

    #using result report
    t_test_report <- paste(
      "The t-test between independent variable '", iv,
      "' and dependent variable '", dv,
      "' was ",
      ifelse(t_test_result$p.value < 0.05,
             "statistically significant, t(",
             "not statistically significant, t(" ),
      round(t_test_result$parameter, 2),") =",
      round(t_test_result$statistic, 2), ",",
      "p = ", round(t_test_result$p.value, 2), ".")

    #using graph
    t_test_report_sub <- paste(
      "The t-test between '", iv,
      "' and '", dv,
      "' was",
      ifelse(t_test_result$p.value < 0.05,
             " significant, t(",
             "not significant, t(" ),
      round(t_test_result$parameter, 2), ") =",
      round(t_test_result$statistic, 2), ",",
      "p = ", round(t_test_result$p.value, 2), ".")


    # box plot output
    gg <- ggplot(data,
                 aes(x = factor(iv_value),
                     y = dv_value,
                     group = iv_value)) +
      geom_boxplot(color="black", fill=c("steelblue","gold")) +
      labs(title = "t tset Result",
           x ="independent Variable",
           y = "dependent Variable",
           subtitle = t_test_report_sub)+
      theme_bw()

    # Output results
    switch(type,
           'all' = list(
             var_test_result_tibble = var_test_result_tibble,
             var_test_report = var_test_report,
             t_test_result_tibble = t_test_result_tibble,
             t_test_report = t_test_report,
             boxplot= gg),
           'var.test' = var_test_result_tibble,
           'var.test.report' = var_test_report,
           't.test' = t_test_result_tibble,
           't.test.report' =t_test_report,
           'boxplot' = gg,
           'var.test.full'= var_test_result,
           't.test.full' = t_test_result
    )
  }
}


# t_test_report(mtcars, iv="vs",dv="mpg")
