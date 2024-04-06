#' Treat selected variables as factors
#'
#' @param data data.frame
#' @param ...  variables
#' @param fun funtion default as.factor
#'
#' @return data treatment
#' @export
#'
#' @examples
#' \dontrun{
#'  str(mrcars)
#'
#'  mtcars1 = mtcars %>% sel_as_trt("cyl", "am","vs", "gear", "carb")
#'  str(mtcars1)
#' }
#'
as_trt <- function(data, ..., fun = factor) {
  vars <- c(...)
  for (var in vars) {
    if (var %in% names(data)) {
      data[[var]] <- fun(data[[var]])
    } else {
      warning(paste("Variable", var, "not found in the dataset"))
    }
  }
  return(data)
}
