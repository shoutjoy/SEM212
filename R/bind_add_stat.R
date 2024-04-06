
#' Functions that generate multiple pieces of data
#'
#' @param data data.fram
#' @param ...  colname, cols
#' @param fun fun  mean sd
#' @param type data, res --data.fram , res, cols_check
#'
#'
#' @return data.fram
#' @export
#'
#' @examples
#' \dontrun{
#'
#'
#' mtcars %>% mutate(
#' Mean_col1 = (mpg + cyl)/2 ,
#' Mean_col2 = (disp + hp)/2 ,
#' Mean_col3 = (drat + wt )/2 ,
#' Mean_col4 = (qsec + vs + am)/3 )
#'
#'
#' bind_add_stat(mtcars,
#' c("Mean_col1","mpg", "cyl"),
#' c("Mean_col2", "disp", "hp"),
#' c("Mean_col3", "drat", "wt"),
#' c("Mean_col4", "qsec", "vs", "am") )
#'
#'
#'
#' bind_add_stat(mtcars,
#'    c("Mean_col1", 1:2),
#'    c("Mean_col2", 3:4),
#'    c("Mean_col3", 5:6),
#'    c("Mean_col4", 7:9))
#'  ##col_check
#' }
#'
bind_add_stat <- function(data, ..., fun = mean, type="res") {
  term <- list(...)
  col_names <- vector("list", length(term))  # 리스트 초기화
  cols <- vector("list", length(term))       # 리스트 초기화
  cols_check <- vector("list", length(term))       # 리스트 초기화
  # colnas <- vector("list", length(term))       # 리스트 초기화
  # Colsnames = colnames(data)

  for (i in seq_along(term)) {
    cols_check[[i]] <- term[[i]][-1]
  }

  if( unique(is.na(cols_check %>% unlist())) ){
    cols_check  = cols_check%>%unlist()
  }else{

    #  cols_check <- cols_check%>%unlist() %>% as.numeric()
    #suppres sWarnings  message
    cols_check <- suppressWarnings(cols_check%>%unlist() %>% as.numeric())
  }

  #   if (any(length(cols_check) <= 1 )) {
  #   cols_check <- NULL
  # } else if (length(cols_check) > 1) {
  #  cols_check <- unlist(cols_check) %>% as.numeric()
  #  }

  for (i in seq_along(term)) {
    col_names[[i]] <- term[[i]][1]  # 리스트에 값 할당
    cols[[i]] <- term[[i]][-1]      # 리스트에 값 할당

    if(unique(is.na(unlist(cols_check)))){
      cols[[i]] <- term[[i]][-1]      # 리스트에 값 할당

    }else{

      #  cols[[i]] <- colnames( data[ suppressWarnings(as.numeric(cols[[i]])) ] )
      cols[[i]] <- colnames( data[ as.numeric(cols[[i]]) ] )
    }

    data[[col_names[[i]]]] = add_stat(data,
                                      cols[[i]] %>% unlist(),
                                      col_name = col_names[[i]] %>% unlist(),
                                      fun = fun,
                                      type = "col")
  }

  # return(data)
  res= list(data, cols_check)

  switch(type,
         data= data ,
         all= res,
         res = data,
         cols_check= cols_check
         )

}
