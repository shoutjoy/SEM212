#' PRE(proportional reduction in error)
#'
#' @param model1 model 1
#' @param model2 model q mixed model
#'
#' @return random slope, inetcept
#' @export
#'
#' @examples
#' \duntrun{
#'  PRE(gmlm.dich.m1b, gmlm.dich.m2c)
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

  data0 = data.frame( model_1 = c(var_cov_model1$vcov[1],
                                  var_cov_model1$vcov[2]),
                      model_2 = c(var_cov_model2$vcov[1], var_cov_model2$vcov[2]),
                      diff =c(var_cov_model1$vcov[1]-var_cov_model2$vcov[1],
                              var_cov_model1$vcov[2]-var_cov_model2$vcov[2])
  )


  Res  = cbind(RPE=c("PRE_Intercept","PRE_Slope"),data0, res)

  Res|>tibble()
}
