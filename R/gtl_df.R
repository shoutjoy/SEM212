#' Google translate data.frame
#' #gtl_df() dataframe 번역 ----------
#' #영어논문을 만들 때, 표에 들어가는 단어를 번역하여 넣기
#' #너무 많은 수는 오래걸림--> 웹페이지를 갔다와야 하므로...
#' Title
#'
#' @param df data.frame
#' @param col Select the column you want to translate
#' @export
#'
#' @examples
#' \dontrun{
#'  df0 <-  data.frame(word=c("온라인","진행","교육"),
#'                    n=c(100, 200, 300))
#'  df0 %>% gtl_df()
#' }
#'
#'
gtl_df <- function(df, col=1){

  df <-  df %>% as.data.frame()   #tibble to dataframe
  df[,col]<- as.character(df[,col])  #factor to character

  for( i in 1:nrow(df)){
    df[i,col]<- gtl(df[i,col])
  }
  df
}
