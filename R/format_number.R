#' Define a function: Convert numbers to scientific notation or common number
#'
#' @param number number
#' @param n1 length
#' @param n2 scientfic
#' @param n3 scientfic
#'
#' @return  formatting
#' @export
#'
#' @examples
#' \dontrun{
#' ## number of data
#' nchar(sub("\\d+\\.", "", as.character(0.123456)))
#'
#' numbers <- c(0.123456, 123.456789, 0.0000123456)
#' sapply(numbers, format_number)
#'
#' for (i in seq_along(numbers)) {
#'    cat("Original:", numbers[i], "Formatted:", formatted_numbers[i], "\n")}
#'
#' format_number(0.123456)
#'
#' }
#'
format_number <- function(number, n1=8, n2=3, n3=5) {
  # Use scientific notation when there are more than 18 decimal places
  if(is.numeric(number)){

    return(format(number, scientific = TRUE, digits = n3)) %>%
      as.numeric()

  }else{

    if (nchar(sub("\\d+\\.", "", as.character(number))) >= n1) {
      return(format(number, scientific = TRUE, digits = n2))
    } else {
      return(format(number, scientific = FALSE, digits = n3))
    }

  }

}
