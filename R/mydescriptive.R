#' mydescriptive 'n','mean','sd','min','max'
#'
#'
#' @param myvariable variable vector
#' @param digits digits
#' @export
#' @examples
#' \dontrun{
#'
#' mydescriptive(mtcars$mpg)
#'
#' }
#'
mydescriptive <- function(myvariable, digits = 3){
  mysize <- length(myvariable)
  mymean <- round(mean(myvariable),digits)
  mysd <- round(sd(myvariable),digits)
  mymin <- round(min(myvariable),digits)
  mymax <- round(max(myvariable),digits)
  mydes <- matrix(c(mysize, mymean, mysd, mymin, mymax), ncol=5)
  colnames(mydes) <- c('n','mean','sd','min','max')
  mydes
}




#' mydes 'n','mean','sd','min','max'
#'
#'
#' @param myvariable variable vector
#' @param var input variable name
#' @param digits digits
#' @export
#' @examples
#' \dontrun{
#' ## view variable
#' mydes(mtcars$mpg,"mpg")
#' ##not see variable
#' mydes(mtcars$mpg)
#'
#' }
#'
mydes <- function(myvariable, var = NULL, digits = 2){
  Var = var
  N <- length(myvariable)
  Mean <- round(mean(myvariable),digits)
  SD <- round(sd(myvariable),digits)
  Min <- round(min(myvariable),digits)
  Max <- round(max(myvariable),digits)
  Skew <- round(SKEW(myvariable),digits)
  Kurt <- round(KURT(myvariable),digits)

  if(is.null(Var)){
    mydes <- cbind.data.frame(N, Mean, SD, Min, Max, Skew, Kurt)
  }else{
    mydes <- cbind.data.frame(Var, N, Mean, SD, Min, Max, Skew, Kurt)
  }
  mydes <- tibble::tibble(mydes)
  mydes
}




#' my summary descriptive statistics
#'
#' @param myobject data.frame, matrix
#' @param ... column variable
#'
#' @return size   MEAN    SD  MIN  MAX
#' @export
#'
#' @examples
#' \dontrun{
#' mysummary(mtcars, "mpg","wt","hp")
#' ## size      MEAN         SD    MIN     MAX
#' ## 1   32  20.09062  6.0269481 10.400  33.900
#' ## 2   32   3.21725  0.9784574  1.513   5.424
#' ## 3   32 146.68750 68.5628685 52.000 335.000
#'
#'
#' }
mysummary <- function(myobject, ...){
  #  Returning more (or less) than 1 row per `summarise()` group was deprecated in dplyr
  # 1.1.0.
  # ℹ Please use `reframe()` instead.
  # ℹ When switching from `summarise()` to `reframe()`, remember that `reframe()` always
  #   returns an ungrouped data frame and adjust accordingly.
  myvars <- c(...)
  myresult <- dplyr::reframe(myobject,
                             var = myvars,
                             N = sapply(myobject[myvars], length),
                             MEAN = sapply(myobject[myvars], mean),
                             SD = sapply(myobject[myvars], sd),
                             MIN = sapply(myobject[myvars], min),
                             MAX = sapply(myobject[myvars], max),
                             Skew = sapply(myobject[myvars], SKEW),
                             Kurt = sapply(myobject[myvars], KURT),
                             )

    myresult

}





#' Describedescriptive statistics cusum
#'
#' @param myobject data.frame, matrix
#' @param ... column variable
#'
#' @return size   MEAN    SD  MIN  MAX
#' @export
#'
#' @examples
#' \dontrun{
#' mysummary(mtcars, "mpg","wt","hp")
#' ## size      MEAN         SD    MIN     MAX
#' ## 1   32  20.09062  6.0269481 10.400  33.900
#' ## 2   32   3.21725  0.9784574  1.513   5.424
#' ## 3   32 146.68750 68.5628685 52.000 335.000
#'
#'
#' }
#'
Describe <- function(myobject, ...){
  #  Returning more (or less) than 1 row per `summarise()` group was deprecated in dplyr
  # 1.1.0.
  # ℹ Please use `reframe()` instead.
  # ℹ When switching from `summarise()` to `reframe()`, remember that `reframe()` always
  #   returns an ungrouped data frame and adjust accordingly.
  myvars <- c(...)
  myresult <- dplyr::reframe(myobject,
                             var = myvars,
                             N = sapply(myobject[myvars], length),
                             MEAN = sapply(myobject[myvars], mean),
                             SD = sapply(myobject[myvars], sd),
                             Var = sapply(myobject[myvars], var),
                             MIN = sapply(myobject[myvars], min),
                             MAX = sapply(myobject[myvars], max),
                             Skew = sapply(myobject[myvars], SKEW),
                             Kurt = sapply(myobject[myvars], KURT)

  )
  myresult

}







#
# mysummary <- function(myobject, ...){
#   # ... is a factor that can receive multiple variables.
#   myvars <- list(...) # Converting to a list.
#   myresult <- lapply(myvars, function(x) { # Use the lapptly function to obtain summary results for each variable.
#     dplyr::summarise(myobject,
#   var = x, # Insert a variable name in the var column.
#   size = length(myobject[[x]]), # Insert the length of each variable in the size column.
#   MEAN = mean(myobject[[x]]), # Put the mean of each variable in the MEAN column.
#   SD = sd(myobject[[x]]), # Inserts the standard deviation of each variable in a column.
#   MIN = min(myobject[[x]]), #Minimize each variable in the MIN column.
#   MAX = max(myobject[[x]])) # Insert the maximum value of each variable in the MAX column.
#   })
#   myresult <- dplyr::bind_rows(myresult) # Use the bind_rows function to group summary results of each variable into rows.
#   myresult
# }
#



