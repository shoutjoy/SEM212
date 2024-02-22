#' bind_mysummaryBy is group mean of mean
#'
#' @param data data.frame
#' @param ... fomula data ...
#' @param unite ... bind persional data and group data  TRUE
#'
#' @return Use group data obtained from group descriptive statistics to obtain group-level descriptive statistics (means, standard deviations, etc.) and to analyze a mixed model
#' @export
#'
#' @examples
#' \dontrun{
#' ##
#' bind_mysummaryBy(mtcars, mpg ~ am)
#' ##compare
#' mysummaryBy(data=mtcars, mpg ~ am)
#'
#' group_by(mtcars, am) %>%  summarise(mpg =mean(mpg))
#' ## A tibble: 2 × 2
#' ##    am   mpg
#' ##<dbl> <dbl>
#' ## 1     0  17.1
#' ##2     1  24.4
#'
#' group_by(mtcars, am) %>%
#'  summarise(mpg =mean(mpg)) %>%
#'   summarise(mean(mpg),sd(mpg),length(mpg), min(mpg),max(mpg))
#' ### A tibble: 1 × 5
#' ###`mean(mpg)` `sd(mpg)` `length(mpg)` `min(mpg)` `max(mpg)`
#' ###<dbl>     <dbl>         <int>      <dbl>      <dbl>
#' ###  1        20.8      5.12             2       17.1       24.4
#'
#' bind_mysummaryBy(mtcars, mpg ~ am, mpg ~ vs)
#' ## grp   dv        N  MEAN    SD   MIN   MAX
#' ##  <chr> <chr> <int> <dbl> <dbl> <dbl> <dbl>
#' ##1 am    mpg       2  20.8  5.12  17.1  24.4
#' ##2 vs    mpg       2  20.6  5.61  16.6  24.6
#' ##
#' bind_mysummaryBy(mtcars, mpg ~ am, mpg ~ vs, mpg ~cyl)
#'
#'
#'
#' }
bind_mysummaryBy <- function(data, ..., unite=FALSE) {

  form = list(...)

  if(length(form)==1){
    result = mysummaryBy(form[[1]], data,  gm = TRUE )
    result

  }else if(length(form) > 1 ){
    result = mysummaryBy(form[[1]], data,  gm = TRUE )
    for (i in 2:length(form)) {
      result <- rbind(result,
                      mysummaryBy(form[[i]], data,   gm = TRUE )     )
    }
  } #if
  if(unite){
    result = result |> tidyr::unite(var, grp:dv)
  }else{
    result
  }
  return(result)
}
