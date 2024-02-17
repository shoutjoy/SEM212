#' exceldata to dataframe
#'
#' @param tpye "clipboard" to table data. dataframe, tibble
#' @param x "clipboard"
#'
#' @return paste data.frame
#' @export
#'
#' @examples
#' #first you must copy data (Ctrl+C)
#' ## then excute below ctrl+Enter
#' df = datapaste()
#' df = datapaste("tibble")
#' df = datapaste("table")
#' df = datapaste("data.frame")
#'
datapaste <- function(type="data.frame", x="clipboard"){

table = read.table(file = x, sep = "\t", header = TRUE)
data.frame = read.table(file = x, sep = "\t", header = TRUE)|>data.frame()
tibble = read.table(file = x, sep = "\t", header = TRUE)|> tibble::tibble()

switch(type,
       table= table,
       data.frame = data.frame,
       tibble =  tibble
       )

}
