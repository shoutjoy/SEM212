#' ICC.logit generalized linear model ICC
#'
#' @param gmlm.dichotomous  logit model analysis result
#'
#' @return ICCresult
#' @export
#'
#' @examples
#' \dontrun{
#' lme4::glmer(am ~ wt + (wt |vs ), mtcars,
#'             family=binomial(link='logit'))|>
#'             ICC.logit()
#' }
ICC.logit <- function(gmlm.dichotomous){

  var.cov <- data.frame(VarCorr(gmlm.dichotomous))
  icc2.dich <- var.cov$vcov/(sum(var.cov$vcov)+((pi^2)/3))
  var.cov$ICC = icc2.dich
  var.cov$ICC_ratio = paste0(round(icc2.dich,4) * 100,"%")
  #result
  var.cov
}
