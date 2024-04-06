#' lm() confint plot
#'
#' @param lm_data  lm(formula. data )
#' @param size_text text size 12
#' @param color  color = "steelblue"
#' @param linewidth linewidth =1
#' @param alpha alpha = 0.9
#' @param intercept default FALSE
#' @param type graph is g, data =df , all is res
#'
#' @return plot and data
#' @export
#'
#' @examples
#' \dontrun{
#'
#' lm(mpg ~ hp+ wt+ disp, mtcars) %>% confint_plot_lm()
#'
#' lm(mpg ~ hp+ wt+ disp, mtcars) %>% confint_plot_lm(intercept=T)
#'
#' }
#'
confint_plot_lm <- function(lm_data,
                            size_text = 12,
                            color = "steelblue",
                            linewidth = 1,
                            alpha = 1,
                            intercept = FALSE,
                            type="g"){

  lm_data0 <- lm_data%>% tidy(conf.int = TRUE)
  if(intercept){
    lm_data <- lm_data%>% tidy(conf.int = TRUE)
  }else{
    lm_data <- lm_data%>% tidy(conf.int = TRUE) %>%
      slice(-1)
  }



  g = lm_data%>% ggplot(aes(x=estimate,   #x는 value
                            y=term,    #Y는 variable
                            xmin = conf.low,
                            xmax = conf.high,
                            height = 0)) +
    geom_errorbarh(color = color,
                   linewidth = linewidth,
                   alpha = alpha)+
    geom_point(size=3) +
    geom_vline(xintercept = 0, lty = 2, color = "red") +
    theme_bw()+
    theme(axis.text.y = element_text(size=size_text))


  res = list(lm_data0, g)

  switch(type,
         res = res,
         g =  g,
         df = lm_data0)
}






