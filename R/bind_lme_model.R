#' Tbind_lme_model
#'
#' @param ... input
#' @param model_name  model name
#' @param title model title
#'
#' @return  table
#' @export
#'
#' @examples
#' \dontrun{
#' data(birthwt)
#' str(birthwt)
#' fitm <- lm(bwt ~ age+lwt+factor(race)+smoke+ptl+ht+ui, data=birthwt)
#' fitm2 <- lm(bwt ~ lwt+factor(race)+smoke+ht+ui, data=birthwt)
#' bind_lme_model(fitm, fitm2)
#'
#' }
bind_lme_model = function(...,
                          model_name = paste("모형", 1:length(model) ) ,
                          title = "모형의 비교"){

  model = list(...)
  # Comparison model
  sjPlot::tab_model(
    model,
    show.ci = FALSE,
    show.df = FALSE,
    show.aic = TRUE,
    show.loglik = TRUE,
    p.style = "star",
    dv.labels = model_name,
    title = title
  )
}
