#' statistics by group
#'
#' @param formula general formula
#' @param data data.frame
#' @param add_var one sample name, Used to obtain statistics for one variable
#' @param stat TRUE t.test and aov(), thes stat ="t.test", or 'aov'
#' @return Mea, SD, N, min, max
#' @export
#'
#' @examples
#' \dontrun{
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
#' ##statistic data output
#'mysummaryBy(mpg ~ 1, data = mtcars, "mpg", stat="t.test")
#'mysummaryBy(mpg ~ vs, data = mtcars, stat="t.test")
#'mysummaryBy(mpg ~ vs+am, data = mtcars, stat="aov")
#'mysummaryBy(mpg ~ vs*cyl, data = mtcars, stat="aov")
#'
#' }
#'
mysummaryBy <- function(formula,
                        data,
                        add_var=NULL,
                        stat=FALSE) {
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
                          Max = max(x, na.rm = TRUE),
                          Skew = SKEW(x),
                          Kurt =KURT(x)
                          )
                      })


  if(stat=="t.test"){
    stat_res = t.test(formula, data = data) |> broom::tidy()|>select(1:5)
  }else if(stat=="aov"){
    stat_res = aov(formula(formula), data = data) |> broom::tidy()
  }else{
    stat_res=NULL
  }


  if(func[3]!='1()'){
    res = dplyr::bind_cols(var=result[,1: (ncol(result)-1) ],
                           result[[ncol(result)]] ) |> tibble::tibble()

    if(is.null(stat_res)){
      res
    }else{
      res = list(descriptive = res, statistic = stat_res)
      res
    }

  }else{ #Used to obtain statistics for one variable
    res = dplyr::bind_cols(stat_var=add_var,
                           result[[ncol(result)]] ) |> tibble::tibble()
    if(is.null(stat_res)){
      res
    }else{
      res = list(descriptive = res, statistic = stat_res)
      res
    }
    #  res = list(res, stat_res)
    #  return(res)
  }
}
#' #'
#' mysummaryBy <- function(formula, data, add_var=NULL) {
#'   # Make sure the data object is provided
#'   if (missing(data)) stop("Please provide the data object as an argument.")
#'   # Import dplyr if needed
#'   if (requireNamespace("dplyr")) library(dplyr)
#'   # Aggregate with summary statistics
#'   func = formula(formula) #formula extraction
#'
#'   #analysis
#'   result <- aggregate(formula(formula), data,
#'                       FUN = function(x) {
#'                         c(
#'                           Mean = mean(x, na.rm = TRUE),
#'                           SD = sd(x, na.rm = TRUE),
#'                           N = length(x),
#'                           Min = min(x, na.rm = TRUE),
#'                           Max = max(x, na.rm = TRUE))
#'                       })
#'
#'   if(func[3]!='1()'){
#'     res = dplyr::bind_cols(var=result[,1: (ncol(result)-1) ],
#'                            result[[ncol(result)]] ) |> tibble::tibble()
#'     return(res)
#'
#'   }else{ #Used to obtain statistics for one variable
#'     res = dplyr::bind_cols(stat_var=add_var,
#'                            result[[ncol(result)]] ) |> tibble::tibble()
#'     return(res)
#'   }
#' }
