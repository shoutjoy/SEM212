#' ICC_pois  poisson model
#'
#' @param gmlm.poisson model result
#'
#' @return possion ICC
#' @export
#'
#' @examples
#' \dontrun{
#' lme4::glmer(cyl ~ wt + (wt |vs ), mtcars,
#'             family= poisson(link='log'))|>
#'             ICC_pois()
#' }
ICC_pois <- function(gmlm.poisson){
  var.cov <- data.frame(lme4::VarCorr(gmlm.poisson))
  icc2.pois <- var.cov$vcov/(sum(var.cov$vcov) + 1)
  var.cov$ICC = icc2.pois
  var.cov$ICC_ratio = paste0(round(icc2.pois, 4) * 100,"%")
  var.cov
}
