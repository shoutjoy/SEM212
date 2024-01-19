#' Modification Indices
#'
#' @param x lavaan result
#' @param op1  regression '~~'
#' @param mi.level mi over 10, default
#' @description
#' Given a fitted lavaan object, compute the modification indices (= univariate score tests) for a selected set of fixed-to-zero parameters.
#'
#' @return chisquare
#' @export
#'
#' @examples
#' \dontrun{
#' library(lavaan)
#' model1 <- '
#'   # measurement model
#'     ind60 =~ x1 + x2 + x3
#'     dem60 =~ y1 + y2 + y3 + y4
#'     dem65 =~ y5 + y6 + y7 + y8
#'   # regressions
#'     dem60 ~ ind60
#'     dem65 ~ ind60 + dem60
#'  # residual correlations
#'     y1 ~~ y5
#'     y2 ~~ y4 + y6
#'     y3 ~~ y7
#'     y4 ~~ y8
#'     y6 ~~ y8
#' '
#'
#' fit1 <- sem(model1, data = PoliticalDemocracy)
#'
#'  summary(fit1, standardized = TRUE)
#' cfa2(fit1)
#'
#' Modindices(fit1)
#' }
#'
#'
#'
Modindices<- function(x,
                      op1 = "~~",
                      mi.level = 10){
  modindices(x) %>%
    filter(op==op1 & mi> mi.level) %>%
    arrange(desc(mi))

}
