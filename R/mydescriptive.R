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
#' ## all variable
#'  jjstat::mysummary(mtcars, colnames(mtcars))
#'
#' }
mysummary <- function(myobject, ..., all=T, digits= 4){
  #  Returning more (or less) than 1 row per `summarise()` group was deprecated in dplyr
  # 1.1.0.
  # ℹ Please use `reframe()` instead.
  # ℹ When switching from `summarise()` to `reframe()`, remember that `reframe()` always
  #   returns an ungrouped data frame and adjust accordingly.

  #Extracting and cleaning only the numeirc variable from the data
  if(all){
    myobject <- myobject %>% purrr::keep(is.numeric)
    myvars <- colnames(myobject)


    myresult <- dplyr::reframe(myobject %>% purrr::keep(is.numeric) ,
                               var = myvars,
                               N = sapply(myobject[myvars], length),
                               MEAN = sapply(myobject[myvars], mean),
                               SD = sapply(myobject[myvars], sd),
                               MIN = sapply(myobject[myvars], min),
                               MAX = sapply(myobject[myvars], max),
                               Skew = sapply(myobject[myvars], SKEW),
                               Kurt = sapply(myobject[myvars], KURT))

  }else{
    myvars <- c(...)
    myresult <- dplyr::reframe(myobject,
                               var = myvars,
                               N = sapply(myobject[myvars], length),
                               MEAN = sapply(myobject[myvars], mean),
                               SD = sapply(myobject[myvars], sd),
                               MIN = sapply(myobject[myvars], min),
                               MAX = sapply(myobject[myvars], max),
                               Skew = sapply(myobject[myvars], SKEW),
                               Kurt = sapply(myobject[myvars], KURT))
        }

  res =   myresult

  options(pillar.sigfig = digits)
  return(res)
  # on.exit(options(current_options))
}




#' add_normality
#'
#' @param data mysummarydata
#'
#' @return add normality
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' jjstat::mysummary(mtcars, colnames(mtcars)) %>% add_nomality()
#'
#' }
#'
#'
add_normality = function(data,  skew ="Skew", kurt="Kurt"){
  Skew = all_of(c(skew))
  Kurt = all_of(c(kurt))

  res= data %>% mutate(
    Skew_z = Skew/sqrt((6*N*((N-1))/((N-2)*(N+1)*(N+3)))),
    Kurt_z = Kurt/sqrt((24*N*(N-1)*(N-1))/((N-3)*(N-2)*(N+3)*(N-5))),
    skew_TF = ifelse( abs(Skew_z) < 1.96, "Good",
                      ifelse( abs(Skew_z) < 3, "Fair","NO")),
    kurt_TF = ifelse( abs(Kurt_z) < 1.96, "Good",
                      ifelse( abs(Kurt_z) < 3, "Fair","NO"))
  )
  res%>%select(-MIN,-MAX) %>% tibble::tibble()
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
Describe <- function(myobject, ...,  digits= 6){
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
                             Kurt = sapply(myobject[myvars], KURT),
                             SE = sapply(myobject[myvars], sd)/sqrt(sapply(myobject[myvars], length)),
                             Lci =  sapply(myobject[myvars], mean) - 1.96*SE,
                             Uci =  sapply(myobject[myvars], mean) + 1.96*SE

  )

  res = myresult

  options(pillar.sigfig = digits)
  print(res)
  # on.exit(options(current_options))


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



