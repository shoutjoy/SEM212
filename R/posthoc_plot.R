
#' Post-hoc results and plots
#'
#' @param aov_data aov data
#' @param type type is all, res, g, post hoc
#' @param adjust
#'
#' @return plot and analysis result
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' #Need to replace cyl with factor first. Change to as_trt() first
#' aov(mpg ~ cyl, data=mtcars %>%as_trt("cyl"))%>%posthoc_plot()
#'
#' ##Error in emmeans::emmeans(., iv) :
#' aov(mpg ~ factor(cyl), data=mtcars)%>%posthoc_plot()
#' }
#'
posthoc_plot = function(aov_data,
                        caption = "Table. Multiple comparison",
                        type = "all",
                        console = FALSE,
                        group = TRUE,
                        adjust = "none"){
  plot_data <- aov_data
  form = plot_data$call[2]%>%formula()%>% as.character()
  iv  = form[3]


  LSD_test = agricolae::LSD.test(aov_data,iv, group = group, console=console)

  contrast = aov_data %>%
    emmeans::emmeans(iv)%>%
    pairs(simple = iv, adjust = adjust)%>%p_mark_sig()


  contrast_md = aov_data %>%
    emmeans::emmeans(iv)%>%
    pairs(simple = iv, adjust = adjust)%>%p_mark_sig() %>%
    markdown_table(caption = caption)


  g = plot_data %>%
    emmeans::emmeans(iv)%>%
    pairs(simple = iv, adjust = adjust)%>%plot()+
    geom_vline(xintercept = 0, col="red", linetype=2)+
    theme(axis.text = element_text(size=12))+
    theme_bw()

  res = list(LSD_test = LSD_test,POSTHOC_comparison = contrast,
             contrast_md = contrast_md,  confint_sig = g)
  res2 = list(POSTHOC_comparison = contrast, contrast_md = contrast_md,
              confint_sig = g)

  switch(type,
         all= res,
         res = res2,
         g = g,
         posthoc = LSD_test,
         contrast = contrast,
         lsd = contrast,
         )
}
