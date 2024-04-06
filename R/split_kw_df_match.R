#' split_kw_df_match, using Goeun Kim, PhD (Seoul National University 2024)
#'
#' @param df data.frame
#' @param sel_col word column
#' @param remove TRUE is newdata add
#' @param type type is res and all
#'
#' @return data.frame
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' #' wd <- data.frame(
#' id =  paste0("BY", 1:5),
#' word = c("토끼", "휘파람", "뻐스", "우리냥","우리낙"),
#' tone = c("HH","LHH","HL","LHH","LHL" ),
#' gender =c("F","F","M","M","M"),
#' age = c(20,60,20,60, 30),
#' area= c("n","s","n","s","s"),
#' mix= c(FALSE,FALSE,FALSE,TRUE,TRUE),
#' stringsAsFactors = FALSE)
#'
#'# Create new data
#' wd%>%split_kw_df_match("word", type="res")
#'
#' wd%>%split_kw_df_match("word", type="all")
#'
#' wd2 <- data.frame(id =  paste0("BY", 1:5),
#' word = c("토끼", "휘파람", "뻐스", "우리냥","우리낙망"))
#'
#' wd2%>%split_kw_df_match("word", type="res")
#'
#' wd2%>%split_kw_df_match("word", type="all")
#'
#'
#' }
#'
#'
#'
split_kw_df_match <- function(df,
                              sel_col="word",
                              remove = TRUE,
                              type="res" ){
  # Separate each word in the word column into initial, neutral, and final.
  if(!is.data.frame(df)){
    # stop("You Must Enter as a [data frame], you need check your data ")
    df <- as.data.frame(df)
  }

  df = df %>% as.data.frame()%>% dplyr::mutate(N = nchar(word))
  n_col =  ncol(df)  #select


  spl = function(separated_word) {

    if(nchar(separated_word)==1){
      separated_word = paste0(separated_word,"      ")

    }else if(nchar(separated_word)==2){
      separated_word = paste0(separated_word,"   ")

    }else{
      separated_word  }



    separated_korean <- split_korean_word(separated_word)

    paste(separated_korean, collapse = '')
  }

  df$syllabic <- sapply(as.vector(df[, sel_col]), spl)

  df = separate(df, syllabic, c(
    paste0(rep(LETTERS[1:max(df$N)], each = 3),
           rep(1:3, times = 3))
  ), sep=",",
  remove = remove )


  # tonecheck
  tonecheck <- data.frame(
    tone = c("ㄱ", "ㄷ", "ㅂ", "ㅈ", "ㄱ", "ㄴ", "ㄹ", "ㅁ", "ㅇ", "ㄲ", "ㄸ", "ㅃ", "ㅉ", "ㅋ", "ㅌ", "ㅍ", "ㅊ", "ㅎ", "ㅅ", "ㅆ"),
    code = c("lax", "lax", "lax", "lax", "lax", "son", "son", "son", "son", "tsasp", "tsasp", "tsasp", "tsasp", "tsasp", "tsasp", "tsasp", "tsasp", "fri", "fri", "fri")
  )

  # Find variables with a letter followed by a number
  df1 <- df
  target_columns <- grep("[A-Z]1$", names(df1), value = TRUE)


  # Create a1, b1, c1 by matching their values with tonecheck data
  for (col in target_columns) {
    df1[[col]] <- tonecheck$code[match(df1[[col]], tonecheck$tone)]

  }
  colnames(df1) = tolower(colnames(df1))


  tonecheck2 <- data.frame(
    tone = c("ㅏ", "ㅐ", "ㅑ", "ㅒ", "ㅓ", "ㅔ", "ㅕ", "ㅖ", "ㅗ",
             "ㅘ", "ㅙ", "ㅚ", "ㅛ", "ㅜ", "ㅝ", "ㅞ", "ㅟ", "ㅠ", "ㅡ", "ㅢ"),
    code = c("sg", "sg", "dp", "dp", "sg", "sg", "dp", "dp", "sg",
             "dp", "dp", "dp", "dp", "sg", "dp", "dp", "dp", "dp", "sg", "dp")
  )

  df2 <- df

  target_columns2 <- grep("[A-Z]2$", names(df2), value = TRUE)
  for (col2 in target_columns2) {
    df2[[col2]] <- tonecheck2$code[match(df2[[col2]], tonecheck2$tone)]
  }
  colnames(df2) = tolower(colnames(df2))

  tonecheck3 <- data.frame(
    tone = c("ㄱ", "ㄲ", "ㄳ", "ㄴ", "ㄵ", "ㄶ", "ㄷ", "ㄹ", "ㄺ", "ㄻ", "ㄼ", "ㄽ", "ㄾ", "ㄿ", "ㅀ", "ㅁ", "ㅂ", "ㅄ", "ㅅ", "ㅆ", "ㅇ", "ㅈ", "ㅊ", "ㅋ", "ㅌ", "ㅍ", "ㅎ"),
    code = c("obs", "obs", "obs", "son", "obs", "obs", "obs", "son", "obs", "obs", "obs", "obs", "obs", "obs", "obs", "son", "obs", "obs", "obs", "obs", "son", "obs", "obs", "obs", "obs", "obs", "obs")
  )

  df3 <- df
  target_columns3 <- grep("[A-Z]3$", names(df3), value = TRUE)
  for (col3 in target_columns3) {
    df3[[col3]] <- tonecheck3$code[match(df3[[col3]], tonecheck3$tone)]
  }
  colnames(df3) = tolower(colnames(df3))



  if(max(df$N) < 4 ){
    df = bind_cols(df,
                   df1[,-c(1: n_col)][c(1)],
                   df2[,-c(1: n_col)][c(2)],
                   df3[,-c(1: n_col)][c(3)],
                   df1[,-c(1: n_col)][c(4)],
                   df2[,-c(1: n_col)][c(5)],
                   df3[,-c(1: n_col)][c(6)],
                   df1[,-c(1: n_col)][c(7)],
                   df2[,-c(1: n_col)][c(8)],
                   df3[,-c(1: n_col)][c(9)]
    )
  }else if(max(df$N) == 4){
    df = bind_cols(df,
                   df1[,-c(1: n_col)][c(1)],
                   df2[,-c(1: n_col)][c(2)],
                   df3[,-c(1: n_col)][c(3)],
                   df1[,-c(1: n_col)][c(4)],
                   df2[,-c(1: n_col)][c(5)],
                   df3[,-c(1: n_col)][c(6)],
                   df1[,-c(1: n_col)][c(7)],
                   df2[,-c(1: n_col)][c(8)],
                   df3[,-c(1: n_col)][c(9)],
                   df1[,-c(1: n_col)][c(10)],
                   df2[,-c(1: n_col)][c(11)],
                   df3[,-c(1: n_col)][c(12)]
    )
  }else if(max(df$N) == 5){
    df = bind_cols(df,
                   df1[,-c(1: n_col)][c(1)],
                   df2[,-c(1: n_col)][c(2)],
                   df3[,-c(1: n_col)][c(3)],
                   df1[,-c(1: n_col)][c(4)],
                   df2[,-c(1: n_col)][c(5)],
                   df3[,-c(1: n_col)][c(6)],
                   df1[,-c(1: n_col)][c(7)],
                   df2[,-c(1: n_col)][c(8)],
                   df3[,-c(1: n_col)][c(9)],
                   df1[,-c(1: n_col)][c(10)],
                   df2[,-c(1: n_col)][c(11)],
                   df3[,-c(1: n_col)][c(12)],
                   df1[,-c(1: n_col)][c(13)],
                   df2[,-c(1: n_col)][c(14)],
                   df3[,-c(1: n_col)][c(15)])
  }
  ### REMOVe <NA>
  df[is.na(df)] <- ""
  df1[is.na(df1)] <- ""
  df2[is.na(df2)] <- ""
  df3[is.na(df3)] <- ""

  resall = list(raw = df,
                초성 = df1,
                중성 = df2,
                종성 = df3)
  res = df #%>% tibble::tibble()
  options(warn = -1)
  # return(df)
  switch(type,
         # res = tibble::tibble(res),
         res = res,
         all = resall)
}
