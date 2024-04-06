
#' interplot_data is matrix, long_data, plotting function
#'
#' @param ... input data c() or as select()
#' @param col matrix ncol setting
#' @param row matrix nrow
#' @param type type is mat, long, all, plot
#' @param title plot title
#' @param xlab  plot xlab
#' @param ylab  plot ylab
#' @param vjust  vjust text in plot postion
#' @param hjust  hjust text in plot position
#' @param size_element  plot element text size
#' @param byrow  byrow=TRUE is default
#' @param xy  plot transpose default FALSE
#' @param yadd  plot range add
#'
#' @return  data and plot
#' @export
#'
#' @examples
#' \dontrun{
#'  ## data output
#' interplot_data( 6, 1, 4, 5, type = "mat")
#' #' interplot_data( 6, 1, 4, 5, type = "all")
#' interplot_data( 6, 1, 4, 5, type = "long")
#' interplot_data( 6, 1, 4, 5, type = "plot")
#'
#' # 2*3 data
#' interplot_data( c(6, 1, 4, 5,6,7), type = "mat",  col = 3,row = 2)
#' interplot_data( c(6, 1, 4, 5,6,7), type = "long",  col = 3,row = 2)
#' interplot_data( 6, 1, 4, 5,6,7, type = "all",  col = 3,row = 2)
#' interplot_data( 6, 1, 4, 5,6,7, type = "plot",  col = 3,row = 2)
#'
#' ## 3*2 data
#' interplot_data( 6, 1, 4, 5,6,7, type = "mat",  col = 2,row = 3)
#' interplot_data( 6, 1, 4, 5,6,7, type = "all",  col = 2,row = 3)
#' interplot_data( 6, 1, 4, 5,6,7, type = "long",  col = 2, row = 3)
#' interplot_data( c(6, 1, 4, 5,6,7), type = "plot",  col = 2,row = 3)
#'
#' ### ii is very hard example
#' interplot_data( 6, 1, 4, 5) %>%
#' ggplot(aes(x = v1, y = value))+
#' geom_point(size=4, aes(color=v2))+
#' geom_line(aes(group = v2, linetype = v2 ), linewidth = 1)+
#' geom_text(aes(label = value), vjust= -0.4, hjust= 2, size= 5)+
#' theme_bw()
#'
#' }
interplot_data = function(...,
                          col = 2,
                          row = 2,
                          type = "long",
                          title = NULL,
                          xlab = "x",
                          ylab = "y",
                          vjust= -0.4,
                          hjust = 0.5,
                          size_element = 12,
                          byrow= TRUE,
                          xy=FALSE,
                          yadd=0.3){
  a = c(...)

  # ddf = data.frame(
  #       B1 = c(a[1], a[3]),
  #       B2 = c(a[2], a[4])
  #       )%>%`rownames<-`(c("A1","A2"))

  ddf = matrix(a, ncol = col, nrow = row, byrow = byrow)
  colnames(ddf) = c(paste0("B",1:ncol(ddf) ))
  rownames(ddf) = c(paste0("A",1:nrow(ddf) ))

  longcol = ncol(ddf)+1

  ddf_long = ddf%>% as.data.frame() %>%
    tibble::rownames_to_column("v1") %>%
    tidyr::pivot_longer(cols = 2:all_of(longcol),
                        names_to = "v2", values_to="value")
  # list(ddf, a[1],a[2],a[3],a[4])

  #plot transpose default FALSE
  if(xy){

    g = ddf_long%>%
      ggplot(aes(x = v1, y = value))+
      geom_point(size = 4, aes(shape = v2, col= v2),show.legend = FALSE)+
      geom_line(aes(group = v2, linetype = v2 ), linewidth = 1)+
      geom_text(aes(label = value), vjust = vjust, hjust = hjust, size= 5)+
      labs(title = title, x = xlab, y = ylab)+
      theme_bw()+
      theme(axis.text = element_text(size = size_element),
            axis.title = element_text(size = size_element+2) )+
      ylim(min(ddf_long$value)-yadd, max(ddf_long$value)+yadd)

  }else{

    g = ddf_long%>%
      ggplot(aes(x = v2, y = value))+
      geom_point(size = 4, aes(shape = v1, col =v1),show.legend = FALSE)+
      geom_line(aes(group = v1, linetype = v1 ), linewidth = 1)+
      geom_text(aes(label = value), vjust= vjust, hjust = hjust, size= 5)+
      labs(title = title, x = xlab, y = ylab)+
      theme_bw()+
      theme(axis.text = element_text(size = size_element),
            axis.title = element_text(size = size_element+2) )+
      ylim(min(ddf_long$value)- yadd, max(ddf_long$value)+ yadd)

  }

  res = list(ddf, ddf_long, g)
  switch(type,
         all = res,
         mat = ddf,
         long = ddf_long,
         plot = g )
}



#' interplot is using data
#'
#' @param data_long long foramt data
#' @param size_point point size default 4
#' @param size_text text size  default 5
#' @param linewidth line width default 1
#' @param vjust  vjust
#' @param hjust hjust
#' @param title title null
#' @param xlab xlab
#' @param ylab ylab
#' @param yadd fig size add value
#' @param legend_title Change legend_title
#' @param size_element axis element size
#' @param t matrix transpose efault FALSE, use when matrix data is wide format
#' @param xy plot ranspose default FALSE
#' @param digits default 3
#'
#' @return plot
#' @export
#'
#' @examples
#' \dontrun{
#'
#' ## interplot_data use!!
#' interplot_data( 6, 1, 4, 5, type = "long")%>%
#' interplot(size_element = 10)
#'
#' interplot_data( 2, 1, 4, 5, type="long")%>%interplot()
#'
#' ## 2 *3 matriix
#' interplot_data( 4, 1, 2, 5, 6, 3, type="long", row=3, col=2)%>%interplot()
#'
#' ##3*2
#' interplot_data( 4, 1, 2, 5, 6, 3, type="long", row=2, col=3)%>%interplot()
#'
#' ##all data output
#' interplot_data( 4, 1, 2, 5, 6, 3, type="long", row=2, col=3)%>%interplot("all")
#'
#' #generated direct data matrix and data frame
#' matrix(c(4, 1, 2, 5, 6, 3), nrow=2,ncol=3)  #marix
#'
#' #long format
#' matrix(c(4, 1, 2, 5, 6, 3), nrow=2,ncol=3) %>%
#' data.frame() %>% rownames_to_column("v1") %>%
#' pivot_longer(cols=2:4)
#'
#' ##plot
#' matrix(c(4, 1, 2, 5, 6, 3), nrow=2,ncol=3) %>%
#'  data.frame() %>%
#' rownames_to_column("v1") %>% pivot_longer(cols=2:4)%>%
#' interplot()
#'
#' ## onestep method when use this function
#' matrix(c(4, 1, 2, 5, 6, 3), nrow=2, ncol=3) %>% interplot(t=TRUE)
#'
#'
#' #  3 * 2
#' matrix(c(4, 1, 2, 5, 6, 3), nrow=3, ncol=2)
#'
#' matrix(c(4, 1, 2, 5, 6, 3), nrow=3, ncol=2) %>%
#' data.frame() %>%
#' rownames_to_column("v1") %>%
#' pivot_longer(cols=2:3)
#'
#' matrix(c(4, 1, 2, 5, 6, 3), nrow=3, ncol=2) %>%
#'  data.frame() %>%
#'  rownames_to_column("v1") %>% p
#'  ivot_longer(cols=2:3)%>%
#'  interplot()
#'
#'
#' ##one step output
#' matrix(c(4, 1, 2, 5, 6, 3), nrow=3, ncol=2) %>%
#' interplot(t=TRUE)
#'
#'
#' #### all data output
#' matrix(c(4, 1, 2, 5, 6, 3), nrow=3, ncol=2) %>% interplot(t=TRUE, type="all")
#'
#' }
#'
interplot = function(data_long,
                     type="plot",
                     size_point = 4,
                     size_text = 5,
                     linewidth = 1,
                     vjust = - 0.4,
                     hjust = -2,
                     title = NULL,
                     xlab = "x_variable",
                     ylab = "y_variable",
                     legend_title= "v2",
                     yadd = 0.5,
                     size_element = 12,
                     t = FALSE,
                     xy = FALSE,
                     digits = 3

){


  library(tidyverse)

  if(t){
    data_wide <-  data_long
    longcol = ncol(data_wide)+1

    data_long = data_wide%>% as.data.frame() %>%
      tibble::rownames_to_column("v1") %>%
      tidyr::pivot_longer(cols = 2:all_of(longcol),
                          names_to = "v2", values_to="value")
  }else{
    data_long <- as.data.frame(data_long %>% dplyr::select(1:3))
    colnames(data_long) = c("v1", "v2", "value")
  }

  #generate matrix
  mat = data_long  %>%
    tidyr::pivot_wider(names_from = "v2",
                       values_from ="value")%>%
    tibble::column_to_rownames("v1")
  #col, row names paste
  colnames(mat) = c( paste0("B",1:ncol(mat)) )
  rownames(mat) = c( paste0("A",1:nrow(mat)) )
  #matrix transpose
  view_mat = t(mat)

  ## rounding numeric variables
  data_long = data_long %>% dplyr::mutate_if(is.numeric, round, digits)
  view_mat = view_mat %>% round(digits)




if(xy){
  g =  data_long %>%
    ggplot(aes(x = v1, y = value))+
    geom_point(size = size_point , aes(shape = v2, col = v2),
               show.legend = FALSE)+
    geom_line(aes(group = v2, linetype = v2),
              linewidth = linewidth)+
    geom_text(aes(label = value),
              vjust= vjust,
              hjust = hjust,
              size = size_text)+
    labs(title= title, x = xlab, y = ylab)+
    theme_bw()+
    theme(axis.text = element_text(size = size_element),
          axis.title = element_text(size = size_element + 3))+
    ylim(min(data_long$value)-yadd, max(data_long$value)+yadd)+
    # guides(col= guide_legend(title= legend_title))
  labs(linetype =legend_title)

}else{
  g =  data_long %>%
    ggplot(aes(x = v2, y = value))+
    geom_point(size = size_point , aes(shape = v1, col = v1),show.legend = FALSE)+
    geom_line(aes(group = v1, linetype = v1),
              linewidth = linewidth)+
    geom_text(aes(label = value),
              vjust= vjust,
              hjust = hjust,
              size = size_text)+
    labs(title= title, x = xlab, y = ylab)+
    theme_bw()+
    theme(axis.text = element_text(size = size_element),
          axis.title = element_text(size = size_element + 3))+
    ylim(min(data_long$value)-yadd, max(data_long$value)+yadd)+
    # guides(col= guide_legend(title= legend_title))
    labs(linetype =legend_title)
}


  res = list(martrix = mat, plot = view_mat, plot = g)

  switch(type,
         all = res,
         plot = g,
         mat = mat,
         plot_mat = view_mat,
         data = data_long

  )

}
