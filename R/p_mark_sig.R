#' p_mark_sig is Function that displays significance in data
#'
#' @param data input data.frame or matrix
#' @param col 'col' specifies the name of the column representing the p value in the data. By default, it is set to p.value. For example, if it is written as p, pvalue, p_value, etc., you can change the name.
#'

#' @export
#'
#' @examples
#'
#' \dontrun{
#' ## data(ToothGrowth)
#' library(dplyr)
#' library(rstatix)
#'
#' # ToothGrowth %>%
#'   group_by(dose) %>%
#'   t_test(data =., len ~ supp) %>%
#'   p_mark_sig("p")
#'
#' ToothGrowth %>%
#'   group_by(dose) %>%
#'   p_mark_sig()
#' }
#'
#'
p_mark_sig <-function(data, col="p.value"){
  #변수 선택
  library(tidyverse)

  p.value <- data %>% as.data.frame() %>%
    dplyr::select(all_of(col))
  # any_of() does not check for missing variables. This is especially useful for negative selection when you want to check if a variable has been removed.
  # all_of() is for strict selection. An error occurs if any of the variables in the character vector are missing.
  # all_of : correct, any_of: some
  ndata <- data %>%
    as.data.frame() %>%
    dplyr::mutate(sig = ifelse(p.value < 0.001, "***",
                        ifelse(p.value < 0.01, "**",
                               ifelse(p.value < 0.05, "*",
                                      "ns"))))
  #Change variable to CHARACTER
  ndata$sig <- as.character(ndata$sig)
  ndata %>% tibble::tibble()

}


