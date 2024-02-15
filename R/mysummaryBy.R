#' statistics by group
#'
#' @param formula general formula
#' @param data data.frame
#' @param add_var one sample name, Used to obtain statistics for one variable
#'
#' @return Mea, SD, N, min, max
#' @export
#'
#' @examples
#' \donrun{
#' ## Used to obtain statistics for one variable
#' mysummaryBy(mpg ~ 1, data = mtcars)
#' mysummaryBy(mpg ~ 1, data = mtcars, "mpg")
#' mysummaryBy(mpg ~ 1, data = mtcars, add_var = "mpg")
#'
#' ##The group variable is 2 or greater
#' mysummaryBy(mpg ~ vs, data = mtcars)
#' mysummaryBy(mpg ~ vs+am, data = mtcars)
#' mysummaryBy(mpg ~ vs+am+cyl, data = mtcars)
#'
#' }
mysummaryBy <- function(formula, data, add_var=NULL) {
  # Make sure the data object is provided
  if (missing(data)) stop("Please provide the data object as an argument.")
  # Import dplyr if needed
  if (requireNamespace("dplyr")) library(dplyr)
  # Aggregate with summary statistics
  func = formula(formula) #formula extraction

  #analysis
  result <- aggregate(formula(formula), data,
                      FUN = function(x) {
                        c(
                          Mean = mean(x, na.rm = TRUE),
                          SD = sd(x, na.rm = TRUE),
                          N = length(x),
                          Min = min(x, na.rm = TRUE),
                          Max = max(x, na.rm = TRUE))
                      })

  if(func[3]!='1()'){
    res = dplyr::bind_cols(var=result[,1: (ncol(result)-1) ],
                           result[[ncol(result)]] ) |> tibble::tibble()
    return(res)

  }else{ #Used to obtain statistics for one variable
    res = dplyr::bind_cols(stat_var=add_var,
                           result[[ncol(result)]] ) |> tibble::tibble()
    return(res)
  }
}
