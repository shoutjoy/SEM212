
#' Create oserved/expected tapes after chisq calculation and give them individual significance
#'
#' @param observed contigency table
#' @param type data, Res(all), chisq_test, cramers v, observed_over_expected_ratios,cell_p_values, p_sig, data, data2
#' @param simple simple sar one *
#'
#' @return  matrix, data.frame
#' @export
#'
#' @examples
#'
#' \dontrun{
#' # # calculate_chi_sig(data)
#' # Example data
#' matrix(c(36, 67, 11,
#'           0, 35, 44,
#'          41, 108, 1,
#'           56, 87, 54),
#'            nrow = 4, byrow = TRUE.
#'    dimnames = list(c("공명음", "마찰음",
#'      "유기음_경음", "평파열음_평파찰음"),  c("H", "H(H)", "L"))) %>%
#'      calculate_chi_sig("p_sig")
#'
#'
#'      matrix(c(36, 67, 11,
#'               40, 35, 44,
#'               41, 108, 1,
#'               56, 87, 54), nrow = 4, byrow = TRUE,
#'                      dimnames = list(c("공명음", "마찰음",
#'                                     "유기음_경음", "평파열음_평파찰음"),
#'                                  c("H", "H(H)", "L"))) %>%
#'                 calculate_chi_sig(simple = TRUE)
#'
#'
#'
#'   chisq.test(matrix(c(36, 67, 11,
#'                       40, 35, 44,
#'                    41, 108, 1,
#'               56, 87, 54), nrow = 4, byrow = TRUE,
#'                dimnames = list(c("공명음", "마찰음",
#'               "유기음_경음", "평파열음_평파찰음"),
#'               c("H", "H(H)", "L")))   )
#'
#'
#'
#' }
#'
#'
calculate_chi_sig <- function(observed, type = "data", simple=FALSE) {
  # 기대값 계산
  row_totals <- rowSums(observed)
  col_totals <- colSums(observed)
  grand_total <- sum(observed)

  expected <- outer(row_totals, col_totals, "*") / grand_total

  # 카이제곱 통계량 계산
  chi_square_statistic <- sum((observed - expected)^2 / expected)

  # 자유도
  df <- (nrow(observed) - 1) * (ncol(observed) - 1)

  # p-값 계산
  p_value <- pchisq(chi_square_statistic, df, lower.tail = FALSE)

  # 각 셀에 대한 p-값 반환
  cell_p_values <- matrix(0, nrow = nrow(observed), ncol = ncol(observed))
  for (i in 1:nrow(observed)) {
    for (j in 1:ncol(observed)) {
      cell_observed <- observed[i, j]
      if (cell_observed == 0) {
        cell_p_values[i, j] <- 1  # If observed is 0, set p-value to 1
      } else {

        cell_expected <- expected[i, j]
        cell_chi_square <- ((cell_observed - cell_expected)^2) / cell_expected
        cell_p_values[i, j] <- pchisq(cell_chi_square, df = 1, lower.tail = FALSE)
      }}

  }
  observed_over_expected_ratios = round(observed / expected, 3)



  colnames(cell_p_values) = colnames(observed)
  rownames(cell_p_values) = rownames(observed)

  p_sig = add_significance_symbols(cell_p_values, simple= simple)
  colnames(p_sig) = colnames(observed)
  rownames(p_sig) = rownames(observed)



  #처리한 값 :관측값과
  chi_combine  =  combine_data(observed,
                               observed_over_expected_ratios,
                               left = "(", right=")")
  # chi_sig =  combine_data(observed_over_expected_ratios, p_sig)
  #처리한 값
  chi_sig =  combine_data(observed_over_expected_ratios, p_sig)
  chi_sig2 =  format(combine_data(chi_combine, p_sig), 5)



  # cramers' v
  cramersv = cramers_v(observed)
  # chisq
  chisq = chisq.test(observed)

  Res = list(
    # chi_square_statistic = chi_square_statistic,
    #           degrees_of_freedom = df,
    #           p_value = p_value,
    chisq_test= chisq,
    cramersv= cramersv,
    observed_over_expected_ratios = observed_over_expected_ratios,
    cell_p_values = cell_p_values,
    p_sig = p_sig,
    chi_sig=chi_sig,
    chi_sig2=chi_sig2
  )

  switch(type,
         res= Res,
         all= Res,
         chisq_test= chisq,
         cramersv= cramersv,
         observed_over_expected_ratios = observed_over_expected_ratios,
         cell_p_values = cell_p_values,
         p_sig = p_sig,
         data = chi_sig,
         data2 = chi_sig2
  )

}



#
#' T관측 기대 테이블
#'
#' @param obs_data matrix
#'
#' @return matrix
#' @export
#'
obs_exp_table = function(obs_data){
  obs_data = as.matrix(obs_data)
  res = calculate_chi_sig(obs_data, type="observed_over_expected_ratios")
  res
}

#' T관측 기대 테이블
#'
#' @param obs_data matrix
#'
#' @return matrix
#' @export
#'
p_value_cal = function(obs_data){
  obs_data = as.matrix(obs_data)
  res = calculate_chi_sig(obs_data, type="cell_p_values")
  res
}

#' T관측 기대 테이블
#'
#' @param obs_data matrix
#'
#' @return matrix
#' @export
#'
p_sig_cal = function(obs_data){

  res = calculate_chi_sig(obs_data, type="p_sig")
  res
}





