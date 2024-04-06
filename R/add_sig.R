#' mutate data generate
#'
#' @param p.value pvalue
#'
#' @return in data
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' aov_table(data = mtcars, dv_var = "mpg", iv_vars = c("cyl", "gear", "carb"), grp_mean = T, unite_F=TRUE) %>%  mutate(STAR= add_sig(p_value))
#'
#' }
add_sig <- function(var){


  ifelse(var < 0.001, "***",
         ifelse(var < 0.01, "**",
                ifelse(var < 0.05, "*",
                       " ns")))
}

