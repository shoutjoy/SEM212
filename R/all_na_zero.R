#' NA to zero
#'
#' @param data data.frame
#' @param fun 'extend', 'base' are equal method
#'
#' @return Na data are changed zero
#' @export
#'

all_na_zero1 <- function(data, fun="extend"){
  data <- dplyr::mutate(data, across(everything(), ~replace_na(.x, 0)))  # base function
  data_base <- replace(data, is.na(data), 0)

  # replace(data, is.na(data), 0)   #
  switch(fun,
         extend = data,
         base = data_base
  )
}





#' all_na_zero
#'
#' @param df data.frame
#' @param imp implement default = 0
#'
#' @return NA to 0
#' @export
#'
#' @examples
#' \dontrun{
#'
#'
#' }
all_na_zero <- function(df, imp = 0 ) {
  # NA 값을 0으로 바꾸기
  df = as.data.frame(df)
  df[is.na(df)] <- imp
  return(df)
}
