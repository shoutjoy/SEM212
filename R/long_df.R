
#' longdata transfomation
#'
#' @param data widedata
#' @param names_to name
#' @param values_to value
#' @param cols colrange
#' @param rowname defai;t acce
#'
#' @return longdata
#' @export
#'
#' @examples
#' \dontrun{
#'
#' mtcars[,1:4] %>%long_df("car", "value")
#' }
long_df = function(data,
                   names_to = "speaker",
                   values_to = "freq",
                   cols = 2:ncol(data1),
                   rowname ="accent"){

  colName = colnames(data)
  rowName = rownames(data) #accent
  # colnames0 = colnames(data)
  data1 = data %>% data.frame() %>%
    rownames_to_column(rowname)

  colnames(data1)= c(rowname, colName)

  data2 <- data1%>%
    pivot_longer(names_to = names_to,
                 values_to = values_to,
                 cols = cols)
  data2

}
