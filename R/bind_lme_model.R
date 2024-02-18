#' Tbind_lme_model
#'
#' @param ... model name list
#' @param model_name auto name, model
#' @param title title input
#'
#' @return table data of comparison model
#' @export
#'
#' @examples
#' \dontrun{
#' bind_lme_model(gmlm.pois.m1,  gmlm.pois.m2a,  gmlm.pois.m2b)
#' }
bind_lme_model = function(...,
                          model_name = paste("model_", 1:length(model) ) ,
                          title = "Comparison of model"){

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
