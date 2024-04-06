#' wald-test using data.frame(or tibble)
#'
#' @param data lm() and data.frame
#' @param row1  row1 = 2
#' @param row2  row2 = 3
#' @param rname1 select first row name
#' @param rname2 select second row name
#' @param sel row is number, term rowname
#'
#' @return diff, z, p value
#' @export
#'
#' @examples
#' \dontrun{
# # default
#'  lm(mpg ~ hp + wt + drat, mtcars) %>%
#'  ztest_df()
#'
#' # #selecting variable
#' lm(mpg ~ hp + wt + drat, mtcars) %>%
#'    ztest_df(row1=2, row2=3)
#' #
#' # # When other variables are selected and analyzed
#'  lm(mpg ~ hp + wt + drat, mtcars) %>%
#'    ztest_df(row1=2, row2=4)
#' #
#' # # When variable-names are selected
#'  lm(mpg ~ hp + wt + drat, mtcars) %>%
#'    ztest_df(sel="term", rname1="hp", rname2="wt")
#' #
#'  lm(mpg ~ hp + wt + drat, mtcars) %>%
#'    ztest_df(sel="term", rname1="hp", rname2="drat")

#'  }
#'
#'
wald_test_df <- function(data, row1=2, row2=3,
                     rname1="", rname2="",
                     sel="row"){

  library(broom)
  library(tidyverse)
  data <- tidy(data)

  if(sel=="row"){
    b1 = data[row1, 2] %>%unlist()
    b2 = data[row2, 2] %>%unlist()
    se1 = data[row1, 3]%>%unlist()
    se2 = data[row2, 3]%>%unlist()
    se1_square = se1^2
    se2_square = se2^2
    diff= b1 - b2

    z = (b1 - b2)/ sqrt(se1_square + se2_square)
    p = 2*(1-pnorm(abs(z)))
    statistics = tibble::tibble(b1,b2,se1,se2,diff, z, p)
    #colnames
    colnames(statistics)=c(
      paste0(data[row1, 1] %>%unlist(),"_est"),
      paste0(data[row2, 1] %>%unlist(),"_est"),
      paste0(data[row1, 1] %>%unlist(),"_se"),
      paste0(data[row2, 1] %>%unlist(),"_se"),
      "diff","z","p.value")

    #selcting by variable name
  }else if(sel=="term"){
    b1  = data %>% dplyr::filter(term==rname1) %>%
      dplyr::select(2) %>%unlist()
    b2  = data %>% dplyr::filter(term==rname2) %>%
      dplyr::select(2) %>%unlist()
    se1 = data %>% dplyr::filter(term==rname1) %>%
      dplyr::select(3) %>%unlist()
    se2 = data %>% dplyr::filter(term==rname2) %>%
      dplyr::select(3)%>%unlist()
    se1_square = se1^2
    se2_square = se2^2
    diff= b1 - b2
    z = (b1 - b2)/ sqrt(se1_square + se2_square)
    p = 2*(1-pnorm(abs(z)))
    statistics = tibble::tibble(b1,b2,se1,se2, diff, z, p)

    colnames(statistics)=c(
      paste0(rname1,"_est"),
      paste0(rname2,"_est"),
      paste0(rname1,"_se"),
      paste0(rname2,"_se"),
      "diff","z","p.value")
  }
  statistics
}
