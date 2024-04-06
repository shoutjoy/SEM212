#' generative weight variable, kge(2024)
#'
#' @param df data.frame
#' @param type result res, res1, res2, res3, res4
#' @param pattern pattern = "", If you want something different, you can specify a different pattern.
#' @param remove ubite option
#'
#' @return  add weight
#' @export
#'
#' @examples
#'
#'
#' \dontrun{
#'  # first
#'  kge_bind0 %>% split_kw_df_match()
#' #데이터검증: 입력시 " "와 같은 빈칸들이 존재함.
#' #' kge_bind11 <- kge_bind0 %>% split_kw_df_match()
#' kge_bind11$A3 %>% unique()
#' kge_bind11$B3 %>% unique()
#' kge_bind11$C3 %>% unique()
#' kge_bind11$D3 %>% unique()
#' # kge_bind10$d3 %>% unique()
#' #데이터 전처리: 빈칸 규칙 체크하여 정리
#' kge_bind11$A3 <- kge_bind11$A3 %>% replace_df(imp = "", pattern = " ")
#' kge_bind11$B3 <- kge_bind11$B3 %>% replace_df(imp = "", pattern = " ")
#' kge_bind11$C3 <- kge_bind11$C3 %>% replace_df(imp = "", pattern = " ")
#' kge_bind11$D3 <- kge_bind11$D3 %>% replace_df(imp = "", pattern = " ")
#'
#' # result
#' kge_bind11 %>% data.frame()
#'
#' # same data
#' kge_bind10 %>% data.frame()
#'
#' # apply kge_weight
#' kge_bind10%>% kge_weigth() %>%  data.frame()
#'
#' kge_bind1<- kge_bind10%>% kge_weigth()
#'
#' kge_bind1 %>% data.frame()
#'  kge_bind1 %>% data.frame() %>% nrow()  #4924
#'
#' ##
#'  kge_bind1 %>% filter(성조 != "") %>% nrow() #4360
#'
#' kge_bind2 %>% data.frame() %>% nrow()#4360
#'
#' }

kge_weigth = function(df, type= "res", pattern ="", remove= FALSE){

  df0 = df %>% tibble::tibble()%>%
    mutate(
      w1 = ifelse(A3 == pattern, 0, 1),
      w2 = ifelse(B3 == pattern, 0, 1),
      w3 = ifelse(C3 == pattern, 0, 1),
      w4 = ifelse(D3 == pattern, 0, 1)
    ) %>% as.data.frame()

  df00 = df0 %>% tidyr::unite(Wz , w1:w4, sep="",remove = remove )%>%
    tibble::tibble()

  # df001 = df00 %>% tidyr::unite(Wz , w1:w4, sep="",remove = remove )%>%
  #   tibble::tibble()
  df001= df00 %>% mutate(
    W3 = substr(Wz, 1, 3),
    W2 = substr(Wz, 1, 2)
  )
  df002 = df001%>% tibble::tibble()%>%
    mutate(
      w1f = ifelse(w1 == 0, "light", "heavy"),
      w2f = ifelse(w2 == 0, "light", "heavy"),
      w3f = ifelse(w3 == 0, "light", "heavy"),
      w4f = ifelse(w4 == 0, "light", "heavy"),
    ) %>% as.data.frame() %>%
    mutate_at(c("w1f", "w2f","w3f","w4f"), factor)



  df1 = df002 %>%
    dplyr::mutate(
      weight = dplyr::case_when(
        W3 == "000" ~ 1,
        W3 == "001" ~ 2,
        W3 == "010" ~ 3,
        W3 == "011" ~ 4,
        W3 == "100" ~ 5,
        W3 == "101" ~ 6,
        W3 == "110" ~ 7,
        W3 == "111" ~ 8)
    )%>% tibble::tibble()

  df2 = df1%>%
    dplyr::mutate(
      weigth_comb = dplyr::case_when(
        W3 == "000" ~ "light-light-light",
        W3 == "001" ~ "light-light-heavy",
        W3 == "010" ~ "light-heavy-light",
        W3 == "011" ~ "light-heavy-heavy",
        W3 == "100" ~ "heavy-light-light",
        W3 == "101" ~ "heavy-light-heavy",
        W3 == "110" ~ "heavy-heavy-light",
        W3 == "111" ~ "heavy-heavy-heavy"),

      weigth_comb2 =  dplyr::case_when(
        W2 == "00" ~ "light-light",
        W2 == "01" ~ "light-heavy",
        W2 == "10" ~ "heavy-light",
        W2 == "11" ~ "heavy-heavy"),

      weigth_comb3 = dplyr::case_when(
        W3 == "000" ~ "X-light-light",
        W3 == "001" ~ "X-light-heavy",
        W3 == "010" ~ "X-heavy-light",
        W3 == "011" ~ "X-heavy-heavy",
        W3 == "100" ~ "X-light-light",
        W3 == "101" ~ "X-light-heavy",
        W3 == "110" ~ "X-heavy-light",
        W3 == "111" ~ "X-heavy-heavy"),


      weigth_comb4 = dplyr::case_when(
        Wz == "0000" ~ "light-light-light-light",
        Wz == "0001" ~ "light-light-light-heavy",
        Wz == "0010" ~ "light-light-heavy-light",
        Wz == "0011" ~ "light-light-heavy-heavy",
        Wz == "0100" ~ "light-heavy-light-light",
        Wz == "0101" ~ "light-heavy-light-heavy",
        Wz == "0110" ~ "light-heavy-heavy-light",
        Wz == "0111" ~ "light-heavy-heavy-heavy",
        Wz == "1000" ~ "heavy-light-light-light",
        Wz == "1001" ~ "heavy-light-light-heavy",
        Wz == "1010" ~ "heavy-light-heavy-light",
        Wz == "1011" ~ "heavy-light-heavy-heavy",
        Wz == "1100" ~ "heavy-heavy-light-light",
        Wz == "1101" ~ "heavy-heavy-light-heavy",
        Wz == "1110" ~ "heavy-heavy-heavy-light",
        Wz == "1111" ~ "heavy-heavy-heavy-heavy")


    )%>% tibble::tibble()

  df3 = df2 %>% dplyr::select(-weight) %>% tibble::tibble()


  switch(type,
         res1 = df0,
         res2 = df00,
         res3 = df1,
         res4 = df3,
         res = df2)
}








#' generative weight variable korean, kge(2024)
#'
#' @param df data.frame
#' @param type result res, res1, res2, res3, res4
#' @param pattern pattern = "", If you want something different, you can specify a different pattern.
#' @param remove ubite option
#'
#' @return  add weight
#' @export
#'
#' @examples
#'
#'
#' \dontrun{
#'  # first
#'  kge_bind0 %>% split_kw_df_match()
#' #데이터검증: 입력시 " "와 같은 빈칸들이 존재함.
#' #' kge_bind11 <- kge_bind0 %>% split_kw_df_match()
#' kge_bind11$A3 %>% unique()
#' kge_bind11$B3 %>% unique()
#' kge_bind11$C3 %>% unique()
#' kge_bind11$D3 %>% unique()
#' # kge_bind10$d3 %>% unique()
#' #데이터 전처리: 빈칸 규칙 체크하여 정리
#' kge_bind11$A3 <- kge_bind11$A3 %>% replace_df(imp = "", pattern = " ")
#' kge_bind11$B3 <- kge_bind11$B3 %>% replace_df(imp = "", pattern = " ")
#' kge_bind11$C3 <- kge_bind11$C3 %>% replace_df(imp = "", pattern = " ")
#' kge_bind11$D3 <- kge_bind11$D3 %>% replace_df(imp = "", pattern = " ")
#'
#' # result
#' kge_bind11 %>% data.frame()
#'
#' # same data
#' kge_bind10 %>% data.frame()
#'
#' # apply kge_weight
#' kge_bind10%>% kge_weigth() %>%  data.frame()
#'
#' kge_bind1<- kge_bind10%>% kge_weigth()
#'
#' kge_bind1 %>% data.frame()
#'  kge_bind1 %>% data.frame() %>% nrow()  #4924
#'
#' ##
#'  kge_bind1 %>% filter(성조 != "") %>% nrow() #4360
#'
#' kge_bind2 %>% data.frame() %>% nrow()#4360
#'
#' }
#'
#'
kge_weigth_ko = function(df, type= "res", pattern ="", remove= FALSE){

  df0 = df %>% tibble::tibble()%>%
    mutate(
      w1 = ifelse(A3 == pattern, 0, 1),
      w2 = ifelse(B3 == pattern, 0, 1),
      w3 = ifelse(C3 == pattern, 0, 1),
      w4 = ifelse(D3 == pattern, 0, 1)
    ) %>% as.data.frame()

  df00 = df0 %>% tidyr::unite(Wz , w1:w4, sep="",remove = remove )%>%
    tibble::tibble()

  # df001 = df00 %>% tidyr::unite(Wz , w1:w4, sep="",remove = remove )%>%
  #   tibble::tibble()
  df001= df00 %>% mutate(
    W3 = substr(Wz, 1, 3),
    W2 = substr(Wz, 1, 2)
  )
  df002 = df001%>% tibble::tibble()%>%
    mutate(
      w1f = ifelse(w1 == 0, "개음절", "폐음절"),
      w2f = ifelse(w2 == 0, "개음절", "폐음절"),
      w3f = ifelse(w3 == 0, "개음절", "폐음절"),
      w4f = ifelse(w4 == 0, "개음절", "폐음절"),
    ) %>% as.data.frame() %>%
    mutate_at(c("w1f", "w2f","w3f","w4f"), factor)



  df1 = df002 %>%
    dplyr::mutate(
      weight = dplyr::case_when(
        W3 == "000" ~ 1,
        W3 == "001" ~ 2,
        W3 == "010" ~ 3,
        W3 == "011" ~ 4,
        W3 == "100" ~ 5,
        W3 == "101" ~ 6,
        W3 == "110" ~ 7,
        W3 == "111" ~ 8)
    )%>% tibble::tibble()

  df2 = df1%>%
    dplyr::mutate(
      weigth_comb = dplyr::case_when(
        W3 == "000" ~ "개음절-개음절-개음절",
        W3 == "001" ~ "개음절-개음절-폐음절",
        W3 == "010" ~ "개음절-폐음절-개음절",
        W3 == "011" ~ "개음절-폐음절-폐음절",
        W3 == "100" ~ "폐음절-개음절-개음절",
        W3 == "101" ~ "폐음절-개음절-폐음절",
        W3 == "110" ~ "폐음절-폐음절-개음절",
        W3 == "111" ~ "폐음절-폐음절-폐음절"),

      weigth_comb2 =  dplyr::case_when(
        W2 == "00" ~ "개음절-개음절",
        W2 == "01" ~ "개음절-폐음절",
        W2 == "10" ~ "폐음절-개음절",
        W2 == "11" ~ "폐음절-폐음절"),

      weigth_comb3 = dplyr::case_when(
        W3 == "000" ~ "X-개음절-개음절",
        W3 == "001" ~ "X-개음절-폐음절",
        W3 == "010" ~ "X-폐음절-개음절",
        W3 == "011" ~ "X-폐음절-폐음절",
        W3 == "100" ~ "X-개음절-개음절",
        W3 == "101" ~ "X-개음절-폐음절",
        W3 == "110" ~ "X-폐음절-개음절",
        W3 == "111" ~ "X-폐음절-폐음절"),


      # weigth_comb3X = dplyr::case_when(
      #   W3 == "000" ~ "개음절-X-개음절",
      #   W3 == "001" ~ "개음절-X-폐음절",
      #   W3 == "010" ~ "개음절-X-개음절",
      #   W3 == "011" ~ "개음절-X-폐음절",
      #   W3 == "100" ~ "폐음절-X-개음절",
      #   W3 == "101" ~ "폐음절-X-폐음절",
      #   W3 == "110" ~ "폐음절-X-개음절",
      #   W3 == "111" ~ "폐음절-X-폐음절"),


      weigth_comb4 = dplyr::case_when(
        Wz == "0000" ~ "개음절-개음절-개음절-개음절",
        Wz == "0001" ~ "개음절-개음절-개음절-폐음절",
        Wz == "0010" ~ "개음절-개음절-폐음절-개음절",
        Wz == "0011" ~ "개음절-개음절-폐음절-폐음절",
        Wz == "0100" ~ "개음절-폐음절-개음절-개음절",
        Wz == "0101" ~ "개음절-폐음절-개음절-폐음절",
        Wz == "0110" ~ "개음절-폐음절-폐음절-개음절",
        Wz == "0111" ~ "개음절-폐음절-폐음절-폐음절",
        Wz == "1000" ~ "폐음절-개음절-개음절-개음절",
        Wz == "1001" ~ "폐음절-개음절-개음절-폐음절",
        Wz == "1010" ~ "폐음절-개음절-폐음절-개음절",
        Wz == "1011" ~ "폐음절-개음절-폐음절-폐음절",
        Wz == "1100" ~ "폐음절-폐음절-개음절-개음절",
        Wz == "1101" ~ "폐음절-폐음절-개음절-폐음절",
        Wz == "1110" ~ "폐음절-폐음절-폐음절-개음절",
        Wz == "1111" ~ "폐음절-폐음절-폐음절-폐음절")


    )%>% tibble::tibble()

  df3 = df2 %>% dplyr::select(-weight) %>% tibble::tibble()


  switch(type,
         res1 = df0,
         res2 = df00,
         res3 = df1,
         res4 = df3,
         res = df2)
}








#' k generative weight variable korean, kge(2024)
#'
#' @param df data.frame
#' @param type result res, res1, res2, res3, res4
#' @param pattern pattern = "", If you want something different, you can specify a different pattern.
#' @param remove ubite option
#'
#' @return  add weight
#' @export
#'
#' @examples
#'
#'
#' \dontrun{
#'  # first
#'  kge_bind0 %>% split_kw_df_match()
#' #데이터검증: 입력시 " "와 같은 빈칸들이 존재함.
#' #' kge_bind11 <- kge_bind0 %>% split_kw_df_match()
#' kge_bind11$A3 %>% unique()
#' kge_bind11$B3 %>% unique()
#' kge_bind11$C3 %>% unique()
#' kge_bind11$D3 %>% unique()
#' # kge_bind10$d3 %>% unique()
#' #데이터 전처리: 빈칸 규칙 체크하여 정리
#' kge_bind11$A3 <- kge_bind11$A3 %>% replace_df(imp = "", pattern = " ")
#' kge_bind11$B3 <- kge_bind11$B3 %>% replace_df(imp = "", pattern = " ")
#' kge_bind11$C3 <- kge_bind11$C3 %>% replace_df(imp = "", pattern = " ")
#' kge_bind11$D3 <- kge_bind11$D3 %>% replace_df(imp = "", pattern = " ")
#'
#' # result
#' kge_bind11 %>% data.frame()
#'
#' # same data
#' kge_bind10 %>% data.frame()
#'
#' # apply kge_weight
#' kge_bind10%>% kge_weigth() %>%  data.frame()
#'
#' kge_bind1<- kge_bind10%>% kge_weigth()
#'
#' kge_bind1 %>% data.frame()
#'  kge_bind1 %>% data.frame() %>% nrow()  #4924
#'
#' ##
#'  kge_bind1 %>% filter(성조 != "") %>% nrow() #4360
#'
#' kge_bind2 %>% data.frame() %>% nrow()#4360
#'
#' }
kge_weigth_add_ca = function(df001, type= "res", pattern ="", remove= FALSE){

  # df0 = df %>% tibble::tibble()%>%
  #   mutate(
  #     w1 = ifelse(A3 == pattern, 0, 1),
  #     w2 = ifelse(B3 == pattern, 0, 1),
  #     w3 = ifelse(C3 == pattern, 0, 1),
  #     w4 = ifelse(D3 == pattern, 0, 1)
  #   ) %>% as.data.frame()
  #
  # df00 = df0 %>% tidyr::unite(Wz , w1:w4, sep="",remove = remove )%>%
  #   tibble::tibble()
  #
  # # df001 = df00 %>% tidyr::unite(Wz , w1:w4, sep="",remove = remove )%>%
  # #   tibble::tibble()
  # df001= df00 %>% mutate(
  #   W3 = substr(Wz, 1, 3),
  #   W2 = substr(Wz, 1, 2)
  # )
  #

  df002 = df001%>% tibble::tibble()%>%
    mutate(
      w1fc = ifelse(w1 == 0, "개", "폐"),
      w2fc = ifelse(w2 == 0, "개", "폐"),
      w3fc = ifelse(w3 == 0, "개", "폐"),
      w4fc = ifelse(w4 == 0, "개", "폐"),
    ) %>% as.data.frame() %>%
    mutate_at(c("w1f", "w2f","w3f","w4f"), factor)


  #
  # df1 = df002 %>%
  #   dplyr::mutate(
  #     weight = dplyr::case_when(
  #       W3 == "000" ~ 1,
  #       W3 == "001" ~ 2,
  #       W3 == "010" ~ 3,
  #       W3 == "011" ~ 4,
  #       W3 == "100" ~ 5,
  #       W3 == "101" ~ 6,
  #       W3 == "110" ~ 7,
  #       W3 == "111" ~ 8)
  #   )%>% tibble::tibble()

  df2 = df002 %>%
    dplyr::mutate(
      weigth_c2 =  dplyr::case_when(
        W2 == "00" ~ "개-개",
        W2 == "01" ~ "개-폐",
        W2 == "10" ~ "폐-개",
        W2 == "11" ~ "폐-폐"),

      weigth_cx2 =  dplyr::case_when(
        W2 == "00" ~ "",
        W2 == "01" ~ "X-폐",
        W2 == "10" ~ "폐-X",
        W2 == "11" ~ "X-폐"),

      weigth_c3 = dplyr::case_when(
        W3 == "000" ~ "개-개-개",
        W3 == "001" ~ "개-개-폐",
        W3 == "010" ~ "개-폐-개",
        W3 == "011" ~ "개-폐-폐",
        W3 == "100" ~ "폐-개-개",
        W3 == "101" ~ "폐-개-폐",
        W3 == "110" ~ "폐-폐-개",
        W3 == "111" ~ "폐-폐-폐"),

      weigth_cx3 = dplyr::case_when(
        W3 == "000" ~ "개-X-개",
        W3 == "001" ~ "개-X-폐",
        W3 == "010" ~ "개-X-개",
        W3 == "011" ~ "개-X-폐",
        W3 == "100" ~ "폐-X-개",
        W3 == "101" ~ "폐-X-폐",
        W3 == "110" ~ "폐-X-개",
        W3 == "111" ~ "폐-X-폐"),


      weigth_cy3 = dplyr::case_when(
        W3 == "000" ~ "X-개-개",
        W3 == "001" ~ "X-개-폐",
        W3 == "010" ~ "X-폐-개",
        W3 == "011" ~ "X-폐-폐",
        W3 == "100" ~ "X-개-개",
        W3 == "101" ~ "X-개-폐",
        W3 == "110" ~ "X-폐-개",
        W3 == "111" ~ "X-폐-폐"),

      weigth_comb3X = dplyr::case_when(
        W3 == "000" ~ "개음절-X-개음절",
        W3 == "001" ~ "개음절-X-폐음절",
        W3 == "010" ~ "개음절-X-개음절",
        W3 == "011" ~ "개음절-X-폐음절",
        W3 == "100" ~ "폐음절-X-개음절",
        W3 == "101" ~ "폐음절-X-폐음절",
        W3 == "110" ~ "폐음절-X-개음절",
        W3 == "111" ~ "폐음절-X-폐음절"),


      weigth_c4 = dplyr::case_when(
        Wz == "0000" ~ "개-개-개-개",
        Wz == "0001" ~ "개-개-개-폐",
        Wz == "0010" ~ "개-개-폐-개",
        Wz == "0011" ~ "개-개-폐-폐",
        Wz == "0100" ~ "개-폐-개-개",
        Wz == "0101" ~ "개-폐-개-폐",
        Wz == "0110" ~ "개-폐-폐-개",
        Wz == "0111" ~ "개-폐-폐-폐",
        Wz == "1000" ~ "폐-개-개-개",
        Wz == "1001" ~ "폐-개-개-폐",
        Wz == "1010" ~ "폐-개-폐-개",
        Wz == "1011" ~ "폐-개-폐-폐",
        Wz == "1100" ~ "폐-폐-개-개",
        Wz == "1101" ~ "폐-폐-개-폐",
        Wz == "1110" ~ "폐-폐-폐-개",
        Wz == "1111" ~ "폐-폐-폐-폐"),

      weigth_cx4 = dplyr::case_when(
        Wz == "0000" ~ "개-X-X-개",
        Wz == "0001" ~ "개-X-X-폐",
        Wz == "0010" ~ "개-X-X-개",
        Wz == "0011" ~ "개-X-X-폐",
        Wz == "0100" ~ "개-X-X-개",
        Wz == "0101" ~ "개-X-X-폐",
        Wz == "0110" ~ "개-X-X-개",
        Wz == "0111" ~ "개-X-X-폐",
        Wz == "1000" ~ "폐-X-X-개",
        Wz == "1001" ~ "폐-X-X-폐",
        Wz == "1010" ~ "폐-X-X-개",
        Wz == "1011" ~ "폐-X-X-폐",
        Wz == "1100" ~ "폐-X-X-개",
        Wz == "1101" ~ "폐-X-X-폐",
        Wz == "1110" ~ "폐-X-X-개",
        Wz == "1111" ~ "폐-X-X-폐"),

      weigth_comb4x = dplyr::case_when(
        Wz == "0000" ~ "개음절-X-X-개음절",
        Wz == "0001" ~ "개음절-X-X-폐음절",
        Wz == "0010" ~ "개음절-X-X-개음절",
        Wz == "0011" ~ "개음절-X-X-폐음절",
        Wz == "0100" ~ "개음절-X-X-개음절",
        Wz == "0101" ~ "개음절-X-X-폐음절",
        Wz == "0110" ~ "개음절-X-X-개음절",
        Wz == "0111" ~ "개음절-X-X-폐음절",
        Wz == "1000" ~ "폐음절-X-X-개음절",
        Wz == "1001" ~ "폐음절-X-X-폐음절",
        Wz == "1010" ~ "폐음절-X-X-개음절",
        Wz == "1011" ~ "폐음절-X-X-폐음절",
        Wz == "1100" ~ "폐음절-X-X-개음절",
        Wz == "1101" ~ "폐음절-X-X-폐음절",
        Wz == "1110" ~ "폐음절-X-X-개음절",
        Wz == "1111" ~ "폐음절-X-X-폐음절"),

      weigth_comb4y = dplyr::case_when(
        Wz == "0000" ~ "X-X-개음절-개음절",
        Wz == "0001" ~ "X-X-개음절-폐음절",
        Wz == "0010" ~ "X-X-폐음절-개음절",
        Wz == "0011" ~ "X-X-폐음절-폐음절",
        Wz == "0100" ~ "X-X-개음절-개음절",
        Wz == "0101" ~ "X-X-개음절-폐음절",
        Wz == "0110" ~ "X-X-폐음절-개음절",
        Wz == "0111" ~ "X-X-폐음절-폐음절",
        Wz == "1000" ~ "X-X-개음절-개음절",
        Wz == "1001" ~ "X-X-개음절-폐음절",
        Wz == "1010" ~ "X-X-폐음절-개음절",
        Wz == "1011" ~ "X-X-폐음절-폐음절",
        Wz == "1100" ~ "X-X-개음절-개음절",
        Wz == "1101" ~ "X-X-개음절-폐음절",
        Wz == "1110" ~ "X-X-폐음절-개음절",
        Wz == "1111" ~ "X-X-폐음절-폐음절")



    )%>% tibble::tibble()


  switch(type,
         res1 = df0,
         res2 = df00,
         res3 = df1,
         res4 = df3,
         res = df2)
}



