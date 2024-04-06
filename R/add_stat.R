
#' 데이터에서변수를 택하면 함수에 맞는 새로운 열을 추가하는 함수
#'
#' @param data data.frame
#' @param ... cols
#' @param fun mean, ad, ....
#' @param col_name New variable
#' @param na.rm na.rm=TRUE
#' @param margin margin 1, 2
#' @param type data, col 2type
#'
#' @return data.fram or vector
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' df = data.frame(A = c(1, 2, 3, 4, 5), B = c(6, 7, NA, 9, 10), C = c(11, 12, 13, 14, 15) )
#'
#' df%>% add_stat( "AC_MEAN","A", "C", fun = mean)
#' df%>% add_stat( "AC_MEAN","A", "C", fun = mean, type="col")
#' df %>%add_stat( "A", "C", type="col")
#' df %>%add_stat( "A", "C")
#' df %>%add_stat( "A", "C", fun = mean, "AC_MEAN")
#'
#' df %>%add_stat( "A", "C", "AC_MEAN",fun = sd)
#'
#' df %>%add_stat(col_name = "AC_MEAN","A", "C", fun = mean)
#' df %>%add_stat("A", "C", fun = mean, col_name = "AC_MEAN")
#' df %>%add_stat("A", "C", fun = mean)
#' df %>%add_stat(c("A", "C"))
#' df %>%add_stat("ROWs", c("A", "C"))
#'
#' mtcars %>% add_stat("MEAN", "am","vs")
#' mtcars %>% add_stat("am","vs")
#' mtcars %>% add_stat("MEAN", c("am","vs"))
#' mtcars %>% add_stat("MEAN", c("mpg","qsec"))
#' mtcars %>% add_stat("MEAN", c("mpg","qsec"), col_name="ACME")
#' df
#'
#' }
#'
#'
add_stat <- function(data,
                     ...,
                     fun = mean,
                     col_name = NULL,
                     na.rm = TRUE,
                     margin = 1,
                     type="data") {


  # 선택한 열 중에 존재하지 않는 열이 있을 경우, 해당 열을 col_name으로 지정
  if (any(is.na(match(c(...), colnames(data))))) {
    cat("\n\n Undefined columns selected\n\n")

    vars = c(...)
    index = match(vars, colnames(data))%>%is.na()  %>% which()
    col_name = vars[index]
    selected_cols = vars[-index]

    data[[col_name]] <- apply(data[,selected_cols],
                              margin, fun, na.rm = na.rm)
    # return(data)
  }else{
    if (!is.null(col_name)) {
      selected_cols <- data[, c(...), drop = FALSE]
      data[[col_name]] <- apply(selected_cols,
                                margin, fun, na.rm = na.rm)
      # return(data)
    }else{
      selected_cols <- data[, c(...), drop = FALSE]
      col_name <- paste0("V_",paste0(..., collapse = "_"))
      data[[col_name]] <- apply(selected_cols, margin, fun, na.rm = na.rm)
    }
  }

  #  return(data)
  switch(type,
         data = data,
         col = data[[col_name]])


}
