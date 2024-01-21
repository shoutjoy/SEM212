#' Advanceed t test report function
#' @param data data.frame
#' @param iv independent variable, if you want to change the name, you can change the setting
#' @param dv dependent variable, if you want to change the name, you can change the setting
#' @param xlab graph name dependent variable
#' @param ylab graph name dependent variable
#' @param type  type option default is 'all'. and  'var.test', 'var.test.report', 't.test','t.test.report', 'boxplot', 'var.test.full', 't.test.full', 'descriptive' output each result
#' @examples
#' # mtcars data
#' \dontrun{
#' ##default analysis
#' t_test_report(mtcars, iv="vs",dv="mpg")
#'
#' ## $var_test_result_tibble
#' ### A tibble: 2 × 4
#' ##Var    df f_value p_value
#' ##<dbl> <int>   <dbl>   <dbl>
#' ##  1     0    17   0.515   0.200
#' ##2     1    13   0.515   0.200
#' ##
#' ##$var_test_report
#' ##[1] "The test of equality of variances between independent variable ' vs ' and dependent variable ' mpg ' was not statistically significant, F( 17 , 13 ) =  0.52 ,  p =  0.2 , This suggests that the variances of the two groups are equal."
#' ##
#' ##$t_test_result_tibble
#' ## A tibble: 1 × 7
#' ##IV    DV       df t.value   p.value sig   test_type
#' ##<chr> <chr> <dbl>   <dbl>     <dbl> <chr> <chr>
#' ##  1 vs    mpg      30   -4.86 0.0000342 ***   student t.test
#' ##
#' ##$t_test_report
#' ##[1] "The t-test between independent variable ' vs ' and dependent variable ' mpg ' was  statistically significant, t( 30 ) = -4.86 , p =  0 ."
#' ##
#' ##$boxplot
#' ## draw image
#'
#'
#'
#' ## New namaing xlab and ylab
#' t_test_report(mtcars, iv="vs",dv="mpg",xlab="transmission", ylab= "males per galon")
#' }
#' @export
#'
#'

t_test_report <- function(data,
                          iv = NULL, dv = NULL,
                          type = "all",
                          xlab = "independent Variable",
                          ylab = "dependent Variable") {
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
    ifelse(var_test_result$p.value < 0.05, "statistically significant, F(",
           "not statistically significant, F("),
    var.test(dv_value ~ iv_value)$parameter[1], ",",
    var.test(dv_value ~ iv_value)$parameter[2], ") = ",
    round(var_test_result$statistic, 2), ",",
    " p = ",
    round(var_test_result$p.value, 2), ",",
    ifelse(var_test_result$p.value < 0.05,
           "This suggests that the variances of the two groups are not equal.",
           "This suggests that the variances of the two groups are equal.")
  )

  #descriptive statistics
  f_summary <- function(x, ...) c(n=length(x,...), mean=mean(x, ...), sd=sd(x, ...))
  descriptive = aggregate(formula( paste(dv, "~", iv) ), data = data, FUN= f_summary)%>%tibble()
  mean_data <-aggregate(formula( paste(dv, "~", iv) ), data = data, mean)%>% tibble()

  # Perform t test

  #welch
  if (var_test_result$p.value < 0.05) {
    t_test_result <- t.test(dv_value ~ iv_value, var.equal = FALSE)
    t_test_result_tibble <- tibble(
      DV = dv,
      IV = iv,
      IV.1 = paste0(mean_data[1,1],"(",round(mean_data[1,2],2),")" ),
      IV.2 = paste0(mean_data[2,1],"(",round(mean_data[2,2],2),")" ),
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
      DV = dv,
      IV = iv,
      IV.1 = paste0(mean_data[1,1],"(",round(mean_data[1,2],2),")" ),
      IV.2 = paste0(mean_data[2,1],"(",round(mean_data[2,2],2),")" ),
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
           x = xlab,
           y = xlab,
           subtitle = t_test_report_sub)+
      theme_bw()

    # Output results
    switch(type,
           'all' = list(
             var_test_result_tibble = var_test_result_tibble,
             var_test_report = var_test_report,
             t_test_result_tibble = t_test_result_tibble,
             t_test_report = t_test_report,
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
