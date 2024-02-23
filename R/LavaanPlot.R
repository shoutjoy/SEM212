#' Lavaan plot draw
#'
#' @param x lavaan result
#' @param stand  standardzied TRUE
#' @param rotation rotation 'LR','RL', 'TB','BT'
#' @param coefs TRUE
#' @param col default 'gray60'
#' @param covs  TRUE
#' @param stars  stars = c("covs", "latent","regress"),
#' @param stand  std coeff
#' @param label  input name labels=c(f1_1="exp", f1_2="want",f1_3="problem") etc
#' @param overlap  default FALSE
#' @param layout  'dot', 'neato', 'fdp', 'sfdp', 'circo', 'twopi', 'nop','nop2', 'osage', 'patchwork'
#' @param digits digits = 3#'
#' @return plot
#' @export
#' @examples
#' # lavaan code
#'
#' ## Bollen (1989), page 332
#' model <- '
#'   # latent variable definitions
#'      ind60 =~ x1 + x2 + x3
#'      dem60 =~ y1 + a*y2 + b*y3 + c*y4
#'      dem65 =~ y5 + a*y6 + b*y7 + c*y8
#'
#'   # regressions
#'     dem60 ~ ind60
#'   dem65 ~ ind60 + dem60
#'
#'   # residual correlations
#'     y1 ~~ y5
#'     y2 ~~ y4 + y6
#'     y3 ~~ y7
#'     y4 ~~ y8
#'     y6 ~~ y8
#' '
#'
#' fit <- lavaan::sem(model, data = PoliticalDemocracy)
#'  ## summary
#' summary(fit, fit.measures = TRUE, stand=T)
#' ##
#' LavaanPlot(fit, cov=F)
## change layout
#' LavaanPlot(fit, cov=F, layout="circo")
#'
#' LavaanPlot(fit, cov=F, layout="dot",
#'            label=c(x1="ind1",ind60="IND60"))
#'
#'
LavaanPlot <- function(x,
                     rotation = "LR",
                     coefs = TRUE,
                     col = "gray60",
                     stand = FALSE,
                     label = NULL,
                     covs = FALSE,
                     stars = c("covs", "latent","regress"),
                     overlap = FALSE,
                     layout = "dot",
                     digits = 3
){
  # library(lavaanPlot)
  lavaanPlot::lavaanPlot(model = x,
             edge_options = list(color = c(col), digits = digits),
             coefs = coefs,
             covs = covs,
             stars = stars,
             graph_options = list(layout = layout,
                                  rankdir = rotation,
                                  overlap = overlap),
             # node_options = list(shape = "box", fontname = "Helvetica"),
             stand = stand,
             labels = label

             # ,
             # graph_options = list(overlap = "true", fontsize = "10" ),
             # labels = labels
  )
}
