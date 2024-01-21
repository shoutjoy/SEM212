#' Lavaan plot draw
#'
#' @param x lavaan result
#' @param stand  standardzied TRUE
#' @param rotation rotation 'LR','RL', 'TB','BT'
#' @param coefs TRUE
#' @param col default 'gray60'
#' @param covs  TRUE
#' @param stand  std coeff
#' @param label  input name labels=c(f1_1="exp", f1_2="want",f1_3="problem") etc
#' @param overlap  default FALSE
#' @param layout  'dot', 'neato', 'fdp', 'sfdp', 'circo', 'twopi', 'nop','nop2', 'osage', 'patchwork'
#' @param digits digits = 3#'
#' @return plot
#' @export
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
