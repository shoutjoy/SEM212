
#' Convert matrix data to a ratio by using the sum of columns to get a ratio
#' @param data matrix
#' @param type res2: *100, res1
#' @param digits defalut 3
#'
#' @return ratio tabl e
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#'
#' #'mat = matrix(c(36, 67, 11,
#'           0, 35, 44,
#'          41, 108, 1,
#'           56, 87, 54),
#'            nrow = 4, byrow = TRUE.
#'    dimnames = list(c("공명음", "마찰음",
#'      "유기음_경음", "평파열음_평파찰음"),  c("H", "H(H)", "L")))
#'
#'  mat %>% add_ratio()
#'
#' }
#'
add_ratio <- function(data,
                      type="res2",
                      # rownames="syllabic",
                      digits=3

) {
  data = as.data.frame(data)
  # 각 열의 합을 구합니다.
  col_sums <- colSums(data)
  # 비율을 계산할 행렬을 생성합니다.
  ratios <- matrix(0, nrow = nrow(data), ncol = ncol(data))

  # 각 열의 합으로 나누어 비율을 계산합니다.
  for (i in 1:ncol(data)) {
    ratios[, i] <- data[, i] / col_sums[i]
  }
  #row and col names
  colnames(ratios) = colnames(data)
  rownames(ratios) = rownames(data)

  ratios = ratios %>% as.data.frame()

  # 결과를 반환합니다.
  res1 = round(ratios, digits)
  res2 = round(ratios, digits)*100


  switch(type,
         res1 = res1,
         res2 = res2
  )
}




#' Combine matrices and their ratios
#'
#' @param res matrix
#'
#' @return combine table /matrix
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' #' #'mat = matrix(c(36, 67, 11,
#'           0, 35, 44,
#'          41, 108, 1,
#'           56, 87, 54),
#'            nrow = 4, byrow = TRUE.
#'    dimnames = list(c("공명음", "마찰음",
#'      "유기음_경음", "평파열음_평파찰음"),  c("H", "H(H)", "L")))
#'
#'  mat %>% add_ratio_df()
#' }
#'
#'
add_ratio_df= function(res){
  res_df_RES =  combine_data(res, add_ratio(res),"(", "%)")
  res_df_RES
}







#' Functions to sum margins after adding ratios
#'
#' @param dataset data(rwa=T)or table
#' @param v1 col
#' @param v2 row
#' @param raw TRUE dataset. not table
#' @param add_ratio using add_ration function
#' @param plot mosicplot
#' @param color color 1:4, etc
#' @param ylab yalb
#' @param xlab xlab
#' @param cex  size
#' @param title title
#' @param type all, df, res
#'
#' @return matrix
#' @export
#'
#' @examples
#'
#' #'
#' \dontrun{
#'
#' #' #'mat = matrix(c(36, 67, 11,
#'           0, 35, 44,
#'          41, 108, 1,
#'           56, 87, 54),
#'            nrow = 4, byrow = TRUE.
#'    dimnames = list(c("공명음", "마찰음",
#'      "유기음_경음", "평파열음_평파찰음"),  c("H", "H(H)", "L")))
#'
#'  mat %>% add_sum()
#'
#'  #                           H H(H)+H(H)_1         L SUM
#'  #공명음             26(11.5%)   35(19.4%) 36(28.1%)  97
#'  #마찰음             49(21.6%)   62(34.4%) 35(27.3%) 146
#'  #유기음_경음       108(47.6%)   82(45.6%)  11(8.6%) 201
#'  #평파열음_평파찰음  44(19.4%)     1(0.6%) 46(35.9%)  91
#'  #SUM                      227         180       128 535
#'
#'
#'
#'
#'
#'
#' }
#'
add_sum = function(dataset,
                   v1 = "a1",
                   v2 = "성조",
                   raw = FALSE,
                   add_ratio = TRUE,
                   plot = FALSE,
                   color = TRUE,
                   ylab = "onset",
                   xlab = "speaker",
                   cex = 1.2,
                   title = NULL,
                   type="df"
){


  if(raw){
    data =  dataset %>%
      dplyr::select(all_of(c(v1)), all_of(c(v2))) %>%
      table()
  }else{
    data =  dataset
  }

  # 최종결과 에 포함을 margn sum
  data_margin0 = data %>% addmargins()

  data_rowsum0 = data %>%  apply(., MARGIN = 2 , FUN = sum)
  data_rowsum_df = data %>%  rbind(SUM=apply(., MARGIN = 2 , FUN = sum) )
  data_colsum = data_rowsum_df %>% apply(., MARGIN = 1 , FUN = sum)

  g = patternGraph(data, tolong = T, type= "g")

  #데이터에 비율을 넣는 경우와 안넣는 경우
  if(add_ratio){
    #데이터 게산
    data_margin = data  %>% add_ratio_df()
  }else{
    data_margin = data
  }

  #모자이크 픞롯을 넣기
  if(plot){
    res_mosaicplot = data %>% t() %>%
      mosaicplot(color = color, ylab = ylab, xlab=xlab,
                 cex.axis = cex,
                 main = paste("Contigency Table of var ", title,""))
  }else{
    res_mosaicplot=NULL
  }

  #비율계산과 데이터 margin sum
  data_margin = cbind(rbind(data_margin, SUM=data_rowsum0 ), SUM=data_colsum )
  res1 = data_margin

  res2 = list(data_margin, g)


  switch(type,
         a11 = res2,
         df = res1,
         res = res1
  )

}




