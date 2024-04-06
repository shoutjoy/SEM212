
#' Create a function to replace NA with an empty string in an R vector data
#'
#' @param vector input data
#' @param pattern dectect pattern
#' @param imp imputation
#'
#' @return imputation data
#' @export
#'
#' @examples
#'
#' \dontrun{
#' # Replace(c("A", NA, "B", "", "C", "C"), "C", imp="T")
#' }
#'
Replace <- function(vector, pattern=NA, imp="") {
  # Use the replace function to replace NA with an empty string
  return(replace(vector, vector == pattern, imp))
}
