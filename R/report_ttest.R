
#' report_ttest
#'
#' @param ttest_result t.test result
#' @param trans true english to korean
#' @param type default 'report', 'all' is t.table and APA
#'
#' @return APA report
#' @export
#'
#' @examples
#' \dontrun{
#' ## english report
#' t.test(mpg ~ vs, mtcars, var.equal=TRUE) %>%
#'         report_ttest()
#'
#' ## translation
#' t.test(mpg ~ vs, mtcars, var.equal=TRUE) %>%
#' report_ttest(trans=T)
#'
#' t.test(mpg ~ vs, mtcars) %>% report_ttest("all")
#' }
report_ttest <- function(ttest_result,
                         trans = FALSE,
                         type ="report"){

  ## t-test result
  t_test_result <- broom::tidy(ttest_result)
  ## processing
  dd = ttest_result$data.name%>% strsplit(" by ") %>% unlist()
  dv = dd[1]
  iv = dd[2]
  #iv variable group
  iv_name1 = paste0( gsub("mean in group ","g",
                          attributes(ttest_result$estimate)[[1]][1]),
                     "(", round(ttest_result$estimate[1],2),")" )

  iv_name2 = paste0( gsub("mean in group ","g",
                          attributes(ttest_result$estimate)[[1]][2]),
                     "(", round(ttest_result$estimate[2],2),")" )
  ## t.test table
  t_test_result_tibble <- tibble::tibble(
    DV = dv,
    IV = iv,
    IV.1 = iv_name1,
    IV.2 = iv_name2,
    df = t_test_result$parameter,
    t.value = round(t_test_result$statistic, 2) ,
    p.value = t_test_result$p.value,
    sig  = dplyr::case_when( p.value < 0.001 ~ "***",
                             p.value < 0.01 ~ "**",
                             p.value < 0.05 ~ "*",
                             TRUE ~ "ns" ),
    test_type = t_test_result$method
  ) %>% tidyr::unite(t.value, t.value, sig, sep="")
  ### using result report
  t_test_report <- paste0(
    t_test_result$method," results between the two groups '",
    iv_name1, "' and '", iv_name2,
    "' of the independent variable '",iv,"' ",
    "with respect to the dependent variable '", dv, "' showed a ",
    ifelse(t_test_result$p.value < 0.05,
           "statistically significant( t(",
           "not statistically significant( t(" ),
    round(t_test_result$parameter, 2), ") = ",
    round(t_test_result$statistic, 2), ",",
    ifelse(t_test_result$p.value< 0.001," p < .001",
           paste0("p = ",round(t_test_result$p.value, 2)))
    , ")")

  if(trans){
    t_test_report = jjstat::kakaoi(t_test_report)
  }else{
    t_test_report
  }

  res= list(t_table = t_test_result_tibble,
            APA = t_test_report )

  switch(type,
         all = res,
         table = t_test_result_tibble,
         report = cat("\n", t_test_report ,"\n\n")
  )
}
