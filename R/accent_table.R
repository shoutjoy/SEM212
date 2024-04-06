#' aaccent_table: table() analytics as data frame, mosaicplot, usign add_ratio
#'
#' @param data table data
#' @param cols column names in data
#' @param rows row names in data
#' @param title table main
#' @param trans transpose
#' @param cex  graph text size
#' @param color graph color 1:2, mosaicplot
#' @param ylab ylab
#' @param xlab xlab
#' @param sub subtitle
#' @param plot output option
#' @param type res, res_df, ratio, g, all
#' @param raw raw data, or table data
#'
#' @return matrix type data.frame
#' @export
#'
#' @examples
#'
#' \dontrun{
#' mtcars %>% select(am, vs) %>% table() %>% accent_table("am","vs")
#'
#' mtcars %>% select(am, vs) %>% table() %>% accent_table("am","vs", plot=TRUE)
#'
#'
#' mtcars %>% select(am, vs) %>% table() %>% accent_table("am","vs", plot=TRUE, title = "Mtcars")
#'
#' }
#'
#'
#'
accent_table = function(data, #table data
                        cols = "a1",
                        rows = "성조",
                        title = "",
                        trans = TRUE,
                        cex = 1.3,
                        color = TRUE,
                        ylab = "onset",
                        xlab="accent",
                        sub = NULL,
                        plot= FALSE,
                        type = "res",
                        raw=FALSE){

  if(raw){
    data =  data %>%
      dplyr::select(all_of(c(cols)), all_of(c(rows))) %>%
      table()

  }else{
    data =  data
  }




  res = data  %>% as.matrix() %>%data.frame() %>%
    pivot_wider(names_from = rows, values_from = "Freq") %>%
    rename(accent = cols) %>% tibble::column_to_rownames("accent")



  # 데이터를 데이터 프레임으로 반들기
  res_df = data  %>% as.matrix() %>%data.frame()


  #비율을 생성하여 행렬화
  res_ratio = add_ratio(res)

  #퍼센트 붙이기
  res_df_RES =  combine_data(res, add_ratio(res_ratio),"(", "%)")

  if(plot){
    res_mosaicplot = res %>%
      mosaicplot(color = color, ylab = ylab, xlab=xlab,
                 cex.axis = cex,
                 main = paste("Contigency Table of var(", title,")"),
                 sub= sub)
  }else{
    res_mosaicplot=NULL
  }
  # sub="Friendly, M. (1994). Mosaic displays for multi-way contingency tables"

  # res_all = list(res, res_df_RES, res_mosaicplot)

  if(trans){res= res %>% t()

  }else{res}


  switch(type,
         res = res,
         res_df = res_df,
         ratio = res_df_RES,
         g = res_mosaicplot,
         all = res_all )
}
