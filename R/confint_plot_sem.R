#'
#' #' lavaan data confin tplot
#' #'
#' #' @param data lavaan data parameterEstimates %>% filter(op==":=")
#' #' @param y lhs data input
#' #' @param x estdata input
#' #' @param lower lower = ci.lower
#' #' @param upper upper ci.upper
#' #' @param size_text default 12
#' #' @param color cor setting "steelblue"
#' #' @param linewidth width 1
#' #' @param alpha Transparency 0.8
#' #' @param vlinewidth  veritcal line 1,2
#' #' @param vlinecolor vertical line color red
#' #'
#' #' @return plot
#' #' @export
#' #'
#' #' @examples
#' #'
#' #' \dontrun{
#' #'
#' #' # Create a tibble named semdata with the given data
#' #'
#' #' semdata%>%confint_plot_sem()
#' #'
#' #' }
#' confint_plot_sem <- function(data,
#'                              y="lhs",
#'                              x="est",
#'                              lower="ci.lower",
#'                              upper="ci.upper",
#'                              size_text=12,
#'                              color="steelblue",
#'                              linewidth=1,
#'                              alpha=0.8,
#'                              vlinewidth=1.2,
#'                              vlinecolor="red"
#' ){
#'   library(tidyverse)
#'
#'
#'   data%>% rename(
#'     Lhs = all_of(y),
#'     Est = all_of(x),
#'     ci.Lower = all_of(lower),
#'     ci.Upper = all_of(upper),
#'   )%>%
#'     tibble::tibble()%>%
#'     ggplot(aes(y = Lhs,
#'                x = Est,
#'                xmin = ci.Lower,
#'                xmax = ci.Upper,
#'                height = 0)) +
#'     geom_errorbarh(color=color,
#'                    linewidth=linewidth+1,
#'                    alpha=alpha)+
#'     geom_point(size=3) +
#'     geom_vline(xintercept = 0,
#'                lty = 2,
#'                color=vlinecolor,
#'                linewidth = vlinewidth-0.2) +
#'     theme_bw()+
#'     theme(axis.text.y = element_text(size=size_text))
#' }
