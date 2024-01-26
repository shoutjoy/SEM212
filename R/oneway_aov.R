
#' oneway ANVOA calculation
#'
#' @param formula  ex)mpg ~ cyl
#' @param data data.frame
#' @param format 'markdwon', 'pandoc', 'html'

#' @return anova result
#' @export
#'
#' @examples
#' \dontrun{
#' oneway_aov(mpg ~ cyl, data = mtcars)
#' }
#'
oneway_aov <- function(formula,
                       data,
                       format="pandoc") {

  # ANOVA table to tibble
  options(knitr.kable.NA = '')
  anova_table <- broom::tidy(aov(formula, data = data)) %>%
    p_mark_sig() %>%
    knitr:::kable(format = format, digits = 2)

  # extraction variable name
  sy <- as.character(formula)[1]  # ~
  dv <- as.character(formula)[2]  # dependent variable
  iv <- as.character(formula)[3]  # independent variable

  # Ensure iv is a factor
  data[[iv]] <- factor(data[[iv]])  # Add this line

  anova_res <- aov(formula(paste(dv, sy, as.character(iv))), data = data)

  # Descriptive statistics
  descriptives <- aggregate(data[[dv]], by = list(data[[iv]]),
                            function(x) c(Mean = mean(x),
                                          SD = sd(x),
                                          N = length(x)),
                            simplify=T)

  colnames(descriptives) <- c(iv, dv)

  #data processing:  agg to tibble data
  des_dv = descriptives[[2]] %>%
    `rownames<-`(descriptives[[1]]) %>%
    data.frame()

  des_dv <- tibble::rownames_to_column( des_dv) %>%
    dplyr::rename(iv_level = rowname) %>%
    tibble::tibble()



  # # posthoc <-  TukeyHSD( summary(anova_res )  )
  # posthoc <-   anova_res  %>%
  #   rstatix::emmeans_test( p.adjust.method=  p.adjust.method) %>%
  #   dplyr::select(-4)  #remove NULL
  #
  # posthoc <-  TukeyHSD( summary(anova_res )  )
  posthoc <- anova_res  %>%
    emmeans::emmeans(iv) %>%
    pairs() %>% p_mark_sig()
  # emmeans::contrast(p.adjust.method = p.adjust.method,
  #                   detailed = detailed)

  # posthoc <-  TukeyHSD( summary(anova_res )  )
  posthoc_plot <- anova_res  %>%
    emmeans::emmeans(iv) %>%
    pairs() %>%
    plot(comparisons = TRUE)+
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray40")+
    labs(title = "Post-hoc of differences between variables")
    theme_bw()


  gg = des_dv %>%
    ggplot(aes(x = iv_level, y = Mean))+
    geom_bar(stat = "identity", aes(fill = iv_level), alpha = 0.3, show.legend = FALSE)+
    geom_boxplot(aes(fill = iv_level), show.legend = FALSE)+
    geom_point(size=3)+
    ylim(0, max(des_dv$Mean)+ 10 )+
    geom_text(aes(label = paste0("M=",round(Mean,2),", SD=",round(SD,2),", N=", N )),
              vjust =-.8, size=3)+
    labs(title = "Descriptive statistics between groups")
    theme_bw()

  graph_grid<- gridExtra::grid.arrange(gg, posthoc_plot,
                                       ncol =2)

  # result output
  res = list(ANOVA_Table = anova_table,
             Descriptives = des_dv,
             posthoc = posthoc,
             # graph =  gg,
             # posthoc_plot=posthoc_plot
             plot = graph_grid

  )

  res
}
