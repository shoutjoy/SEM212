
#' kge_chisq_table chisq table observed/Expected table-
#'
#' @param dataset data.frame
#' @param v1 col
#' @param v2 row
#' @param title title
#' @param type res2 default
#' @param digits 3
#' @param yadd up yadd
#' @param Ncol grid
#' @param trans transpose
#' @param simple * style
#' @param ko graph korean
#' @param simulate.p.value exact
#' @param correct exactt
#' @param size_bartext graph text
#' @param strip_size strip
#' @param axis_size axis_size
#' @param text_size text_size
#' @param xlab xlab
#' @param ylab ylab
#' @param cramer cramer cor
#'
#' @return multiple data
#' @export
#'
#' @examples
#'
#' \dontrun{
#' kge_chisq_table(mtcars,"am","cyl")
#'
#' }
kge_chisq_table = function(dataset,
                           v1="a1",
                           v2="성조",
                           title ="Table",
                           type = "res2",
                           digits = 3,
                           yadd=0.4,
                           Ncol=NULL,
                           trans = FALSE,
                           simple= FALSE, #유의성 종류
                           ko = TRUE,
                           simulate.p.value=FALSE,
                           correct= FALSE,
                           size_bartext=5,
                           strip_size = 16,
                           axis_size = 16,
                           text_size = 13,
                           xlab = "관측기대비율",
                           ylab = "성조형",
                           cramer="adjust"
){

  data =  dataset %>%
    dplyr::select(all_of(c(v1)), all_of(c(v2))) %>%
    table()



  #  margn sum
  data_margin0 = data %>% addmargins()

  data_rowsum0 = data %>%  apply(., MARGIN = 2 , FUN = sum)
  data_rowsum_df = data %>%  rbind(SUM=apply(., MARGIN = 2 , FUN = sum) )
  data_colsum = data_rowsum_df %>% apply(., MARGIN = 1 , FUN = sum)

  #
  data_margin = data  %>%
    accent_table( v1, v2, trans = trans, type = "ratio")

  #margin sum
  data_margin = cbind(
    rbind(data_margin, SUM=data_rowsum0 ),
    SUM=data_colsum )
  #Cramer boss
  cramer_cor = cramers_v(data, type = cramer)
  cramer_cor_v = cramers_v(data, type = "cramer", digit = digits)
  cramer_cor_v_table = cramers_v(data, type = "data1", digits = 2)

  # data_margin = cbind(data_margin, data_margin0[, ncol(data_margin0)])

  #chisq.test
  Onset_or_Coda_Accent_Contingency_table <- data
  #chi-squared dataprem variants
  res_df = chisq.test(data)%>% broom::tidy()


  res = chisq.test(Onset_or_Coda_Accent_Contingency_table,
                   correct = correct,
                   simulate.p.value = simulate.p.value)
  # Judgmental statistics
  chi2 = res$statistic
  p_vlaue_chi = res$p.value
  # df_chi = res$parameter

  msg_sig_chi = ifelse(p_vlaue_chi < 0.05,"significant", "not significant")


  chi_mag = paste0(" [chisq = ",round(res$statistic, 2),
                   ", df = ",res$parameter,
                   ", p = ", format_number(res$p.value, 2),"]" )

  #Create a chi-squared report
  v1_input = ifelse(v1=="a1", "Onset",
                    ifelse(grepl("^w", v1), "Weight",
                           ifelse(v1=="a3", "Coda", v1)))

  v2_input = ifelse(v2=="성조", "Accent", v1)


  res_report = paste0("The Pearson's Chi-squared test of independence between ",
                      v1_input," and ",v2_input,
                      " suggests that the effect is statistically ", msg_sig_chi,
                      chi_mag,
                      ".; ",
                      cramer_cor_v, ".")



  if(nrow(data) != 1){
    chi_table = (res$observed / res$expected)%>% as.data.frame() %>%#
      tidyr::pivot_wider(names_from = v2, values_from = Freq) %>%
      tibble::column_to_rownames(v1) %>%
      jjstat::Round(digits)
  }else{
    chi_table =
      rbind(
        observed = data %>%
          accent_table( v1, v2, trans = trans),
        expected = res$expected,
        obs_expected_ratio = (res$observed / res$expected)
      ) %>% jjstat::Round(digits)

    # g= NULL
  }
  #observer/Expected 에 유의성 표시
  chi_table_sig = format(calculate_chi_sig(data, simple = simple), 3)


  ###마크다운   유의성표시된 것으로 변경----
  chi_table_md0 = chi_table_sig %>%data.frame() %>%
    rownames_to_column("syllable-accent")
  #열이름 재설정
  colnames(chi_table_md0) <- c("syllable-accent",colnames(chi_table_sig))

  chi_table_md = chi_table_md0 %>%
    jjstat::markdown_table(caption = paste0(title, chi_mag,"; ",
                                            cramer_cor_v_table,"."),
                           digits = digits,
                           general = NULL)

  cat("\n\nNOTE: ***: p < .001, **: p < .01, *: p < .05\n\n ")


  # 결과를 정리하여 나타내는 값들
  result = list(chisq_test = res,
                margin = data_margin %>%
                  jjstat::markdown_table(caption = paste0(title,"Contingency table"),
                                         general = NULL),
                chi_table_md,
                chi_table = chi_table)


  #패턴 그래프 ko적용--raq =TRUE, type="g"
  data_graph = data  %>% accent_table( v2, v1, type = "res")


  # 교차표 contigency table
  contigency_table_md0 = data_margin %>% data.frame() %>%
    rownames_to_column("syllable-accent")
  #열이름 재설정
  colnames(contigency_table_md0) <- c("syllable-accent",
                                      colnames(data_graph),
                                      "SUM")

  contigency_table_md =  contigency_table_md0 %>%
    jjstat::markdown_table(caption = paste0(title," Contingency table"),
                           general = NULL)


  if(ko){
    graph = patternGraph_obs_exp_ko(data_graph,
                                    raw = TRUE,
                                    size_bartext=size_bartext,
                                    strip_size = strip_size,
                                    axis_size = axis_size,
                                    text_size = text_size,
                                    yadd = yadd,
                                    Ncol = Ncol,
                                    xlab = xlab,
                                    ylab = ylab)
  }else{
    graph = patternGraph_obs_exp(data_graph,
                                 raw = TRUE,
                                 yadd = yadd,
                                 size_bartext=size_bartext,
                                 strip_size = strip_size,
                                 axis_size = axis_size,
                                 text_size = text_size,
                                 Ncol = Ncol,
                                 xlab = xlab,
                                 ylab = ylab)
  }


  #최종 출력
  result1 = list(
    # data_rowsum0,data_colsum,
    # msg=msg,
    data_graph = data_graph,
    contigency_table_margin = data_margin0,
    contigency_table = data_margin,
    CRAMER_V.adusted = cramer_cor,
    CRAMER_V = cramer_cor_v,
    chi_table = chi_table ,
    chi_table_sig_each_variable = chi_table_sig,

    g = graph,
    contigency_table_md = contigency_table_md,
    chi_table_md = chi_table_md,
    chisq_test_overall = res,
    chisq_report_overall = res_report
  )

  result2 = list(
    # msg=msg,
    crosstable = data_margin,
    data_margin %>%
      jjstat::markdown_table(caption = paste0(title," Contingency table"),
                             general = NULL),
    chisq_test = res,
    # chi_df = res_df,
    chisq_report = res_report,
    chi_table = chi_table ,
    # g = patternGraph1(chi_table,raw = FALSE),
    chi_table_md)


  switch(type,
         ct = data,
         df = data.frame(data),
         data_graph = data_graph,
         margin = data_margin,
         chisq_test = res,
         chisq_df = res_df,
         chisq_report = res_report,
         chi_table = chi_table,

         contigency_table_margin = contigency_table_margin,
         contigency_table = contigency_table,
         chi_table_sig_each_variable=chi_table_sig_each_variable,
         chi_table_sig_each_variable2=chi_table_sig_each_variable2,
         chisq_test_overall=chisq_test_overall,
         chisq_report_overal=chisq_report_overal,
         CRAMER_V=CRAMER_V,
         CRAMER_V.adusted=CRAMER_V.adusted,
         contigency_table_md= contigency_table_md,
         chi_table_md=chi_table_md,
         g = graph,
         res1 = result,
         res2 = result1,
         res3 = result2)
}
