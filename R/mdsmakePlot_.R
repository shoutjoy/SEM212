
#' New MDS function  plot comparison ----
#'
#' @param mds_data mds analysis result data
#' @param grid_title  your title
#' @param xl graph position x low default -3
#' @param xh graph position x high default 3
#' @param yl graph position y low default -2.3
#' @param yh graph position y high default 2
#' @param variable current 6. c("ZOOM","Webex","googleMEET", "Youtube","naveBand","prism")
#' @param show "manual","auto","line"
#' @param linecolor line gray50
#' @param linetype type 1
#' @param linewidth widtih 1
#' @param what input your mind
#' @export
#'
#' @examples
#'
#' ma_new(8- mds_onl[1:20,], grid_title = "2020 full group awareness ")
#' # mds_onl_2020
#' mds_onl_i
#' mds_onl_i %>% slice(1:64) %>%
#'   mds_makePlot(grid_title = gtl("total percetption "))
#'
#'
#'
#'
mds_makePlot <- function(mds_data,
                         grid_title="",
                         xl = -3,
                         xh =  3,
                         yl = -2.3,
                         yh =  2,
                         variable= c("ZOOM","Webex","googleMEET",
                                     "Youtube","naveBand","prism"),
                         show="manual", #"auto","line"
                         linecolor="gray50", #shape analysis
                         linetype= 1,
                         linewidth=1,
                         what=""


){
  library(psych)
  library(lavaan)
  library(tidyverse)
  library(MASS)
  library(gridExtra)
  library(ggrepel)

  if(is.character(variable ==FALSE)){
    stopifnot("input again")
  }
  #

  var= variable

  # generate matrix
  mds_data <- mds_data %>% as.data.frame()
  mds0 = rep(0, nrow(mds_data)) %>% tibble::as_tibble()

  dms_raw <-cbind(mds0,mds_data[,1:5],
                  mds0,mds_data[,6:9],
                  mds0,mds_data[,10:12],
                  mds0,mds_data[,13:14],
                  mds0,mds_data[,15],mds0)

  des.data <- psych::describe(dms_raw) %>% as.data.frame()

  result.matrix <- des.data[,3] %>% lavaan::lav_matrix_upper2full()


  if(length(variable) != 0){
    colnames(result.matrix)=variable
    rownames(result.matrix)=variable}
  #result.matrix





  #scale- dist - mds
  cmd.data <- result.matrix %>%
    scale() %>%
    dist() %>%
    cmdscale(k=2, eig = T)

  cmdplot <- cmd.data$points
  cmdgof <- cmd.data$GOF[1]

  iso.data <- result.matrix %>%
    scale() %>%
    dist() %>%
    MASS::isoMDS(k=2)

  isoplot <- iso.data$points
  isostress <- iso.data$stress
  iso_sig = cut(isostress, breaks = c(0, 0.0025, 0.05, 0.1, 0.2,Inf),
                label=c("perfect","excellent","good","fair","poor" ))

  colName= c("x","y")
  colnames(cmdplot)= colName
  colnames(isoplot)= colName





  #shepard plot data
  mat <- result.matrix %>% scale() # standardization
  dist <-mat %>% dist() #dist function
  #shepard plot data
  dist_sh <- MASS::Shepard(dist, iso.data$points) #shepard plot data
  cdist_sh=cbind(dist_sh$x, dist_sh$y, dist_sh$yf)
  colnames(cdist_sh)=c("원거리", "FitDist", "FitDATA")


  separd_plot_data = cdist_sh[,c(1,3)]
  image_plot_data = cdist_sh[,c(1,2)]


  # library(gridExtra)

  if(show=="manual"){
    cmd_plot = cmdplot %>% data.frame() %>%
      dplyr::mutate(app= var) %>%
      ggplot2::ggplot(aes(x=x, y=y))+
      geom_point(aes(color=app), size= 5, show.legend = FALSE)+
      geom_text_repel(aes(label=app),size=5,
                      box.padding = 1)+
      ylim(yl, yh)+xlim(xl, xh)+
      theme_bw()+
      geom_hline(yintercept=0, linetype="dashed",
                 linewidth=0.8, col="gray40")+
      geom_vline(xintercept=0, linetype="dashed",
                 linewidth=0.8, col="gray40")+
      labs(x= paste0("(a) metric MDS [GOF:",round(cmdgof*100,2),"%]"))+
      theme(axis.title.x= element_text(size=15))

    iso_plot = isoplot %>% data.frame() %>%
      dplyr::mutate(app= var) %>%
      ggplot2::ggplot(aes(x=x, y=y))+
      geom_point(aes(color=app), size= 5, show.legend = FALSE)+
      geom_text_repel(aes(label=app),size=5,
                      box.padding = 1)+
      ylim(yl, yh)+xlim(xl, xh)+
      theme_bw()+
      geom_hline(yintercept=0, linetype="dashed",
                 linewidth=0.8, col="gray40")+
      geom_vline(xintercept=0, linetype="dashed",
                 linewidth=0.8, col="gray40")+
      labs(x=paste0( "(b) Non-metric MDS [stress:",
                     round(isostress,3),"]"))+
      theme(axis.title.x= element_text(size=15) )

  }else if(show=="auto"){


    cmd_plot = cmdplot %>% data.frame() %>%
      dplyr::mutate(app= var) %>%
      ggplot2::ggplot(aes(x=x, y=y))+
      geom_point(aes(color=app), size= 5, show.legend = FALSE)+
      geom_text_repel(aes(label=app),size=5,
                      box.padding = 1)+
      # ylim(-max(abs(cmdplot$y))-1, max(abs(cmdplot$y))+1)+
      # xlim(-max(abs(cmdplot$x))-1, max(abs(cmdplot$x))+1)+
      theme_bw()+
      geom_hline(yintercept=0, linetype="dashed",
                 linewidth=0.8, col="gray40")+
      geom_vline(xintercept=0, linetype="dashed",
                 linewidth=0.8, col="gray40")+
      labs(x= paste0("(a) metric MDS [GOF:",round(cmdgof*100,2),"%]"))+
      theme(axis.title.x= element_text(size=15))



    iso_plot = isoplot %>% data.frame() %>%
      dplyr::mutate(app = var) %>%
      ggplot2::ggplot(aes(x=x, y=y))+
      geom_point(aes(color=app), size= 5, show.legend = FALSE)+
      geom_text_repel(aes(label=app),size=5,
                      box.padding = 1)+
      # ylim(-max(abs(isoplot$y))-1, max(abs(isoplot$y))+1)+
      # xlim(-max(abs(isoplot$x))-1, max(abs(isoplot$x))+1)+
      theme_bw()+
      geom_hline(yintercept=0, linetype="dashed",
                 linewidth=0.8, col="gray40")+
      geom_vline(xintercept=0, linetype="dashed",
                 linewidth=0.8, col="gray40")+
      labs(x=paste0( "(b) Non-metric MDS [stress:",
                     round(isostress,3),"]"))+
      theme(axis.title.x= element_text(size=15) )
  }else if(show=="line"){

    cmd_plot = cmdplot %>% data.frame() %>%
      dplyr::mutate(app= var) %>%
      ggplot2::ggplot(aes(x=x, y=y))+
      geom_path(linewidth=linewidth,
                color=linecolor, linetype= linetype)+  # added path
      geom_point(aes(color=app), size= 5, show.legend = FALSE)+
      geom_text_repel(aes(label=app),size=5,
                      box.padding = 1)+
      ylim(yl, yh)+xlim(xl, xh)+
      theme_bw()+
      geom_hline(yintercept=0, linetype="dashed",
                 linewidth=0.8, col="gray40")+
      geom_vline(xintercept=0, linetype="dashed",
                 linewidth=0.8, col="gray40")+
      labs(x= paste0("(a) metric MDS [GOF:",round(cmdgof*100,2),"%]"))+
      theme(axis.title.x= element_text(size=15))


    iso_plot = isoplot %>% data.frame() %>%
      dplyr::mutate(app= var) %>%
      ggplot2::ggplot(aes(x=x, y=y))+
      geom_path(linewidth= linewidth,
                color=linecolor, linetype= linetype) +
      geom_point(aes(color=app), size= 5, show.legend = FALSE)+
      geom_text_repel(aes(label=app),size=5,
                      box.padding = 1)+
      ylim(yl, yh)+xlim(xl, xh)+
      theme_bw()+
      geom_hline(yintercept=0, linetype="dashed",
                 linewidth=0.8, col="gray40")+
      geom_vline(xintercept=0, linetype="dashed",
                 linewidth=0.8, col="gray40")+
      labs(x=paste0( "(b) Non-metric MDS [stress:",
                     round(isostress,3),"]"))+
      theme(axis.title.x= element_text(size=15) )  # added path
  }



  # if(show=="graph"){
  gridExtra::grid.arrange( cmd_plot,
                iso_plot,
                ncol=2,
                nrow=1,
                bottom= paste0(grid_title,
                               " ( Non-metric stress=",
                               round(isostress,3),"->",iso_sig,")") )

}




