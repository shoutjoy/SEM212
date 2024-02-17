#' exceldata to dataframe
#'
#' @param x "clipboard"
#'
#' @return paste data.frame
#' @export
#'
#' @examples
#' #first you must copy data (Ctrl+C)
#' ## then excute below ctrl+Enter
#' df = datapaste()
#' df
#'
datapaste <- function(x="clipboard"){

  read.table(file = x, sep = "\t", header = TRUE)
}
