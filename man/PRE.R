#' PRE(proportional reduction in error ) in mixed model
#'
#' @param model1 model 1
#' @param model2 model 2
#' @description
#' PRE stands for Proportional Reduction in Error, which is a measure of how much the prediction error of the response variable is reduced by using the information of the predictor variable. In other words, it is a measure of the association between the predictor and the response variable. PRE ranges from 0 to 1, and the larger it is, the better the predictor predicts the response variable. PRE can be applied to various statistical models, and lambda, gamma, eta squared are some examples.
#'
#' @return intercept and slope effect ratio
#' @export
#'
#' @examples
#' \dontrun{
#' ## PRE(gmlm.dich.m1b, gmlm.dich.m2c)
#' data(sleepstudy)
#' model1 = lmer(Reaction ~ 1 + (Days | Subject), sleepstudy)
#' model2 = lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
#' PRE(model1, model2)
#' ##                       value   ratio
#' ## PRE_Intercept 6.066842e-02  6.07 %
#' ## PRE_Slope     7.534410e-01 75.34 %
#' ## PRE_level1    1.231324e-06     0 %
#'
#' }
PRE <- function(model1, model2){
  #proportional reduction in error
  var_cov_model1 <- data.frame(VarCorr(model1))
  var_cov_model2 <- data.frame(VarCorr(model2))
  #random_intercep
  pre_random_intercept = (var_cov_model1$vcov[1]-var_cov_model2$vcov[1])/var_cov_model1$vcov[1]
  #random_slope
  pre_random_slope = (var_cov_model1$vcov[2]-var_cov_model2$vcov[2])/var_cov_model1$vcov[2]

  pre_level1 = (var_cov_model1$vcov[4]-var_cov_model2$vcov[4])/var_cov_model1$vcov[4]

  res = data.frame(PRE_Intercept = pre_random_intercept,
                   PRE_Slope = pre_random_slope,
                   PRE_level1 = pre_level1 )
  res = res|>t() |>`colnames<-`(c("value"))
  # is.na(res)
  # NA data existed row is eliminate
  res = na.omit(res)|>data.frame()
  res$ratio = paste(round(res$value * 100,2),"%")
  res
}
