#' Lavaan plot draw
#'
#' @param x lavaan result
#' @param stand  standardzied TRUE
#' @param rotation rotation 'LR','RL', 'TB','BT'
#' @param coefs TRUE
#' @param col default 'gray60'
#' @param covs  TRUE
#'
#' @return plot
#' @export
#'

LavaanPlot<-function(x, stand=TRUE,
                     rotation="LR", #option ="RL, Top
                     coefs=TRUE,col="gray60",
                     covs= TRUE){
  # library(lavaanPlot)
  lavaanPlot::lavaanPlot(model = x,
             edge_options = list(color = c(col)),
             coefs=coefs,
             covs= covs,
             stars = c ("covs", "latent","regress"),
             stand = stand,
             graph_options = list(layout="dot",rankdir = rotation)

             # ,
             # graph_options = list(overlap = "true", fontsize = "10" ),
             # labels = labels
  )
}
