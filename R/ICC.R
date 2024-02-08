#' ICC
#' @param lmedata lmer result
#' @export
#' @examples
#' \dontrun{
#' data(sleepstudy)
#' sleepstudy |> str()
#' lmer(Reaction ~ Days + (Days | Subject), sleepstudy) |> ICC()
#'
#'
#' }
ICC = function(lmedata){
  #random effect
  random_effect <- data.frame(lme4::VarCorr(lmedata))


  icc =  random_effect |>
    dplyr::mutate(Sum = sum(vcov),
                  ICC = (vcov/Sum),
                  ICC_ratio = paste0(round((vcov/Sum)*100, 2),"%"),
                  ICC_rank = rank(dplyr::desc(ICC))
    ) |>
    dplyr::select(1:2,7,8,9)
  icc
}

