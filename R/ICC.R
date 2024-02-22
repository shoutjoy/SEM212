#' ICC
#' @param lmedata lmer result
#' @param type 'mixed', 'logit', 'pois'
#' @param digits digits = 3 default
#' @export
#' @examples
#' \dontrun{
#' data(sleepstudy)
#' sleepstudy |> str()
#' lmer(Reaction ~ Days + (Days | Subject), sleepstudy) |> ICC()
#'
#'
#' }
# ICC = function(lmedata){
#   #random effect
#   random_effect <- data.frame(lme4::VarCorr(lmedata))
#
#
#   icc =  random_effect |>
#     dplyr::mutate(Sum = sum(vcov),
#                   ICC = (vcov/Sum),
#                   ICC_ratio = paste0(round((vcov/Sum)*100, 2),"%"),
#                   ICC_rank = rank(dplyr::desc(ICC))
#     ) |>  dplyr::select(1:2,7,8,9)
#   icc
# }

ICC <- function(lmedata, type = "mixed", digits=4){

  ranef <- data.frame(lme4::VarCorr(lmedata))

  if(type=="mixed"){
    icc2 <- ranef$vcov/(sum(ranef$vcov) )
  }else if(type=="logit"){
    icc2 <- ranef$vcov/(sum(ranef$vcov)+((pi^2)/3))
  }else if(type=="pois"){
    icc2 <- ranef$vcov/(sum(ranef$vcov) + 1)
  }
  ranef$vcov =  round(ranef$vcov, digits)
  ranef$ICC =  round(icc2, digits)
  ranef$ICC_ratio = paste0(round(icc2, 4) * 100,"%")
  ranef$ICC_rank = rank(dplyr::desc(ranef$ICC))
  ranef<- ranef |>  dplyr::select(1:4,6,7)
  return(ranef)
}
