#' NA to zero
#'
#' @param data data.frame
#' @param fun 'extend', 'base' are equal method
#'
#' @return Na data are changed zero
#' @export
#'

all_na_zero <- function(data, fun="extend"){
  data <- mutate(data, across(everything(), ~replace_na(.x, 0)))  # base function
  data_base <- replace(data, is.na(data), 0)

  # replace(data, is.na(data), 0)   #
  switch(fun,
         extend = data,
         base = data_base
  )
}
