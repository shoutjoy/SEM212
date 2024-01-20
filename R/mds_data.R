#' MDS data output
#'
#' @param mds_data mda_data input
#' @param show  'all','cmdscale','isoMDS','cmdplot_data','isoplot_data', 'plot_data', 'cmd_plot','iso_plot', 'shepard_plot', 'image_plot', 'iso_plot_line', 'iso_line', 'iso_line_auto', 'cmd_plot_line', 'cmd_line', 'cmd_line_auto', 'cmd_gof', 'stress','shepard_data',  'image_data', 'shepardimage_data', 'mat', 'data_mat','scale', 'scale_mat', 'dist','dist_mat','des', 'gof', 'index'
#' @param variable ex c("ZOOM","Webex","googleMEET", "Youtube","NaverBand","Prism")
#' @param addxy default 0.5
#' @param linetype linetype 1
#' @param linecolor  linecolor
#' @param linewidth linewidth= 0.8
#' @param xl xl = -3
#' @param xh xh = 3
#' @param yl yl = -2.3
#' @param yh yh = 2
#' @param text_size text_size = 5
#' @param size.x text_size = 5
#' @param caption caption="MDS"
#'
#' @return MDS result overall
#' @export
#'
#' @examples
#'
#' \dontrun{
#' # mds_onl_i %>% mds_data()
#' mds_onl_i %>% mds_data(show="all")
#'
#' mds_onl_i %>% mds_data(show="data_mat")
#' mds_onl_i %>% mds_data(show="data_mat") %>% dist()
#' mds_onl_i %>% mds_data(show="dist_mat")
#' mds_onl_i %>% mds_data(show="dist_mat") %>% as.matrix() %>%
#'   markdown_table("Distance matrix", digits=3)
#' }
mds_data <- function(mds_data,
                    show = "all",
                    variable = c("ZOOM","Webex","googleMEET",
                                "Youtube","NaverBand","Prism"),
                    addxy = 0.5,
                    linetype = 1,
                    linecolor = 'gray40',
                    linewidth = 0.8,
                    xl = -3,
                    xh = 3,
                    yl = -2.3,
                    yh = 2,
                    text_size = 5,
                    size.x= 14,
                    caption="MDS"
){
  library(psych)
  library(lavaan)
  library(tidyverse)
  library(MASS)
  library(ggrepel)
  library(knitr)
  # library(cluster)
  # Set it globally:
  options(ggrepel.max.overlaps = Inf)

  #박중희 2023을 위해 필요
  load(file ="mds_list.RData")
  index = mds_list

  if(is.character(variable ==FALSE)){
    stopifnot("input again")
  }



  var <- variable


  mds_data <- mds_data %>% as.data.frame()

  mds0 = rep(0, nrow(mds_data)) %>% as_tibble()

  dms_raw <- cbind(mds0,mds_data[,1:5],
                   mds0,mds_data[,6:9],
                   mds0,mds_data[,10:12],
                   mds0,mds_data[,13:14],
                   mds0,mds_data[,15],mds0)

  des.data <- psych::describe(dms_raw) %>% as.data.frame()

  result.matrix <- des.data[,3] %>% lav_matrix_upper2full()




  if(length(variable) != 0){
    colnames(result.matrix)=variable
    rownames(result.matrix)=variable}
  #result.matrix

  cmd.data <- result.matrix %>%
    scale() %>%
    dist() %>%
    cmdscale(k=2, eig = T)

  cmdplot <- cmd.data$points
  cmdgof <- cmd.data$GOF[1]


  iso.data <- result.matrix %>%
    scale() %>%
    dist() %>%
    isoMDS(k=2)
  isoplot <- iso.data$points
  isostress <- iso.data$stress
  # iso_sig = cut(isostress, breaks = c(0, 0.0025, 0.05, 0.1, 0.2,Inf),
  #     label=c("perfect","excellent","good","fair","poor" ))
  iso_sig = cut(isostress,
                breaks = c(0, 0.0024, 0.049, 0.099, 0.19,Inf),
                label=c("perfect","excellent","good","fair","poor" ))


  colName= c("x","y")
  colnames(cmdplot)= colName
  colnames(isoplot)= colName





  mat <- result.matrix
  dist <- mat %>% dist() #거리함수

  # 척도가 다른 경우 표준화 진행 할 것
  mat_scale <- result.matrix %>% scale() #표준화
  dist_scale <- mat_scale %>% dist() # 표준화 거리함수
  # dist_diasy <-mat %>% diasy(metric= "grower") # 비계량 거리함수

  #shepard plot data
  # dist_sh <- Shepard(dist, iso.data$points) #shepard plot 데이터
  # cdist_sh=cbind(dist_sh$x, dist_sh$y, dist_sh$yf)
  # colnames(cdist_sh)=c("원거리", "FitDist", "FitDATA")
  dist_sh <- Shepard(dist, iso.data$points) #shepard plot 데이터
  cdist_sh=cbind(dist_sh$x, dist_sh$y, dist_sh$yf)
  colnames(cdist_sh)=c("Dissimilarity_dist", "FitDist", "FitData")
  shepardImage_data = cbind(index,cdist_sh )


  separd_plot_data= cdist_sh[,c(1,3)]
  image_plot_data= cdist_sh[,c(2,3)]

  #plot
  cmd_plot = cmdplot %>% data.frame() %>% mutate(app = var) %>%
    ggplot(aes(x=x, y=y))+
    geom_point(aes(color=app), size= 5, show.legend = FALSE)+
    geom_text_repel(aes(label=app),size= 6,
                    box.padding = 1)+
    ylim(yl, yh)+xlim(xl, xh)+
    theme_bw()+
    geom_hline(yintercept=0, linetype="dashed",
               linewidth=0.8, col="gray40")+
    geom_vline(xintercept=0, linetype="dashed",
               linewidth=0.8, col="gray40")+
    labs(x= paste0(caption, "[GOF:",round(cmdgof*100,2),"%]"))+
    theme(axis.title.x= element_text(size=size.x))



  # plot
  iso_plot = isoplot %>% data.frame() %>% mutate(app= var) %>%
    ggplot(aes(x=x, y=y))+
    geom_point(aes(color=app), size= 5, show.legend = FALSE)+
    geom_text_repel(aes(label=app),size=text_size,
                    box.padding = 1)+
    ylim(yl, yh)+xlim(xl, xh)+
    theme_bw()+
    geom_hline(yintercept=0, linetype="dashed",
               linewidth=0.8, col="gray40")+
    geom_vline(xintercept=0, linetype="dashed",
               linewidth=0.8, col="gray40")+
    labs(x=paste0(caption,"[stress:",
                  round(isostress,3),"(",iso_sig,")]"))+
    theme(axis.title.x= element_text(size=size.x) )




  Shepard = separd_plot_data %>% data.frame() %>%
    ggplot(aes(x=Dissimilarity_dist, y=FitData))+
    geom_point(size=3)+
    geom_line(linewidth=1, linetype ="dashed")+
    theme_bw()+
    geom_smooth(method=lm, se=T, fill="gray75")


  Imageplot= image_plot_data%>% data.frame() %>%
    ggplot(aes(x=FitDist, y=FitData))+
    geom_point(size=3, col="red")+
    geom_line(linewidth=1, linetype ="dashed")+
    theme_bw()+
    geom_smooth(method=lm, se=T, fill="gray70")

  #shape analysis
  iso_plot_line = isoplot %>% data.frame() %>% mutate(app= var) %>%
    ggplot(aes(x=x, y=y))+
    geom_path(linewidth= linewidth,
              color=linecolor, linetype= linetype) +
    geom_point(aes(color=app), size= 5, show.legend = FALSE)+
    geom_text_repel(aes(label=app),size=text_size,
                    box.padding = 1)+
    ylim(yl, yh)+xlim(xl, xh)+
    theme_bw()+
    geom_hline(yintercept=0, linetype="dashed",
               linewidth=0.8, col="gray40")+
    geom_vline(xintercept=0, linetype="dashed",
               linewidth=0.8, col="gray40")+
    labs(x=paste0(caption,"[stress:",
                  round(isostress,3),"(",iso_sig,")]"))+
    theme(axis.title.x= element_text(size=15) )  # added path
  #shape analysis
  cmd_plot_line = cmdplot %>% data.frame() %>% mutate(app= var) %>%
    ggplot(aes(x=x, y=y))+
    geom_path(linewidth=linewidth,
              color=linecolor, linetype= linetype)+  # added path
    geom_point(aes(color=app), size= 5, show.legend = FALSE)+
    geom_text_repel(aes(label=app),size=text_size,
                    box.padding = 1)+
    ylim(yl, yh)+xlim(xl, xh)+
    theme_bw()+
    geom_hline(yintercept=0, linetype="dashed",
               linewidth=0.8, col="gray40")+
    geom_vline(xintercept=0, linetype="dashed",
               linewidth=0.8, col="gray40")+
    labs(x= paste0(caption, "[GOF:",round(cmdgof*100,2),"%]"))+
    theme(axis.title.x= element_text(size=size.x))


  #plot size auto
  #shape analysis
  iso_plot_line_auto = isoplot %>% data.frame() %>% mutate(app= var) %>%
    ggplot(aes(x=x, y=y))+
    geom_path(linewidth= linewidth,
              color=linecolor, linetype= linetype) +
    geom_point(aes(color=app), size= 5, show.legend = FALSE)+
    geom_text_repel(aes(label=app),size=text_size,
                    box.padding = 1)+
    # ylim(yl, yh)+xlim(xl, xh)+
    theme_bw()+
    geom_hline(yintercept=0, linetype="dashed",
               linewidth=0.8, col="gray40")+
    geom_vline(xintercept=0, linetype="dashed",
               linewidth=0.8, col="gray40")+
    labs(x=paste0(caption,"[stress:",
                  round(isostress,3),"(",iso_sig,")]"))+
    theme(axis.title.x= element_text(size=size.x) )  # added path
  #shape analysis
  cmd_plot_line_auto = cmdplot %>% data.frame() %>% mutate(app= var) %>%
    ggplot(aes(x=x, y=y))+
    geom_path(linewidth=linewidth,
              color=linecolor, linetype= linetype)+  # added path
    geom_point(aes(color=app), size= 5, show.legend = FALSE)+
    geom_text_repel(aes(label=app),size=text_size,
                    box.padding = 1)+
    # ylim(yl, yh)+xlim(xl, xh)+
    theme_bw()+
    geom_hline(yintercept=0, linetype="dashed",
               linewidth=0.8, col="gray40")+
    geom_vline(xintercept=0, linetype="dashed",
               linewidth=0.8, col="gray40")+
    labs(x= paste0(caption, "[GOF:",round(cmdgof*100,2),"%]"))+
    theme(axis.title.x= element_text(size=size.x))






  switch(show,
         all = list(
           data_matrix = result.matrix,
           scale_mat = mat,
           dist_matrix = dist,

           cmdScale=cmdplot,
           isoMDS=isoplot,
           cmd_GOF= paste0(round(cmdgof*100,2),"(%)"),
           strss = paste0(round(isostress,4), " (",iso_sig,")" ),

           separd = separd_plot_data,
           image= image_plot_data
         ),

         cmdscale = cmd.data,
         isoMDS = iso.data,

         cmdplot_data = cmdplot,
         isoplot_data = isoplot,

         plot_data = list(cmd=cmdplot, iso=isoplot),
         cmd_plot = cmd_plot,
         iso_plot = iso_plot,

         shepard_plot = Shepard,
         image_plot = Imageplot,
         iso_plot_line = iso_plot_line, #shape anlaysis
         iso_line = iso_plot_line, #shape anlaysis
         iso_line_auto = iso_plot_line_auto, #shape anlaysis
         cmd_plot_line = cmd_plot_line, #shape anlaysis
         cmd_line = cmd_plot_line, #shape anlaysis
         cmd_line_auto = cmd_plot_line_auto, #shape anlaysis

         cmd_gof= paste0(round(cmdgof*100,2),"(%)"),
         stress = paste0(round(isostress,4), " (",iso_sig,")" ),

         shepard_data = separd_plot_data,
         image_data= image_plot_data,
         shepardimage_data= shepardImage_data, #shepard and image data
         mat = result.matrix,
         data_mat = result.matrix,
         scale = mat_scale,
         scale_mat = dist_scale,
         dist = dist,
         dist_mat = dist,
         des = des.data,
         gof = cbind(Metric_MDS_gof= cmdgof, Non_metric_MDS_stress= isostress),
         index = cbind.data.frame(gof= cmdgof, stress=isostress))

}
