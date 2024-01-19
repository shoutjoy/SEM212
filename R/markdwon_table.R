#' A tool that outputs data to the viewer
#' @export
#' @param data data.frame or etc data
#' @param caption table title
#' @param full_width table wide and fit values
#' @param font_size table font size
#' @param row.names row names in table(Set if necessary)
#' @param col.names column names in table (Set if necessary)
#' @param centering Align cener in Table
#' @param digits table values roundings
#' @param show 'markdown' is output to the viewer, and 'kbl' is output to the console.




#quick markdown_table , kable(format="html) using cfa2()-----------
markdown_table_s <- function(data,
                             caption="html to markdown",
                             full_width=FALSE,
                             font_size=20,
                             row.names = NA,
                             col.names = NA,
                             centering = TRUE,
                             digits=3,
                             show="markdown"
){

  library(kableExtra)

  if(show=="kbl"){
    data %>%as.data.frame() %>%
      kbl(digits = digits,
          caption =  caption,
          row.names=row.names,col.names=col.names,
          centering=centering
      ) %>%
      kable_classic(full_width=full_width,font_size= font_size)
  }else if(show=="markdown"){
    data %>%
      kable_classic(full_width=full_width,font_size= font_size)
  }
}
