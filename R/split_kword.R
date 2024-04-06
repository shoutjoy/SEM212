

#' split_korean_word- Separating vowels and consonants in Korean
#'
#' @param word input word, one or more
#' @param paste Whether you want the outputs to be separate or combined into one
#'
#' @return text data
#' @export
#'
#' @examples
#' \dontrun{
#' split_korean_word("책")
#' ## [1] "ㅊ,ㅐ,ㄱ"
#' split_korean_word("채")
#' ## [1] "ㅊ,ㅐ,"
#' split_korean_word("고양이")
#' #' ##[1] "ㄱ,ㅗ,,ㅇ,ㅑ,ㅇ,ㅇ,ㅣ,"
#' split_korean_word("곱창밥")
#' ## [1] "ㄱ,ㅗ,ㅂ,ㅊ,ㅏ,ㅇ,ㅂ,ㅏ,ㅂ"
#' }
#'
split_korean_word <- function(word, paste=TRUE) {
  # Hangul Unicode Range
  HANGUL_START <- 44032
  HANGUL_END <- 55203

  # Initialization list
  CHOSUNG_LIST <- c(
    'ㄱ', 'ㄲ', 'ㄴ', 'ㄷ', 'ㄸ', 'ㄹ', 'ㅁ', 'ㅂ', 'ㅃ', 'ㅅ',
    'ㅆ', 'ㅇ', 'ㅈ', 'ㅉ', 'ㅊ', 'ㅋ', 'ㅌ', 'ㅍ', 'ㅎ'
  )

  # Neutral list
  JUNGSUNG_LIST <- c(
    'ㅏ', 'ㅐ', 'ㅑ', 'ㅒ', 'ㅓ', 'ㅔ', 'ㅕ', 'ㅖ', 'ㅗ', 'ㅘ',
    'ㅙ', 'ㅚ', 'ㅛ', 'ㅜ', 'ㅝ', 'ㅞ', 'ㅟ', 'ㅠ', 'ㅡ', 'ㅢ', 'ㅣ'
  )

  # Species List
  JONGSUNG_LIST <- c(
    '', 'ㄱ', 'ㄲ', 'ㄳ', 'ㄴ', 'ㄵ', 'ㄶ', 'ㄷ', 'ㄹ', 'ㄺ',
    'ㄻ', 'ㄼ', 'ㄽ', 'ㄾ', 'ㄿ', 'ㅀ', 'ㅁ', 'ㅂ', 'ㅄ', 'ㅅ',
    'ㅆ', 'ㅇ', 'ㅈ', 'ㅊ', 'ㅋ', 'ㅌ', 'ㅍ', 'ㅎ'
  )

  result <- character(0)
  for (char in strsplit(word, '')[[1]]) {
    if (char %in% letters) {
      result <- append(result, char)

    } else if (HANGUL_START <= utf8ToInt(char) && utf8ToInt(char) <= HANGUL_END) {
      ## Locate Korean consonants and vowels
      char_code <- utf8ToInt(char) - HANGUL_START
      chosung_index <- char_code %/% 21 %/% 28
      jungsung_index <- char_code %/% 28 %% 21
      jongsung_index <- char_code %% 28

      result <- append(result, CHOSUNG_LIST[chosung_index + 1])
      result <- append(result, JUNGSUNG_LIST[jungsung_index + 1])

      if (jongsung_index > 0) {
        result <- append(result, JONGSUNG_LIST[jongsung_index + 1])

      } else {
        result <- append(result, '')  # Insert an empty string
      }
    } else {
      result <- append(result, char)
    }
  }
  if(paste){
    return(result %>% paste0(collapse=","))
  }else{
    return(result)
  }
}



#' Tsplit_korean_word_check is datacheck
#'
#' @param worddata worddata
#' @param type type defaul all, text, df , all is input and output, text is result , df is data.frame
#'
#' @return split word
#' @export
#'
#' @examples
#' \dontrun{
#' split_korean_word_check("호랑이")
#' }
#'
split_korean_word_check <- function(worddata, type = "all") {

  if(nchar(worddata)==2){
    worddata =  paste0(worddata,"   ")
  }else{
    worddata
  }

  # Isolate initial, neutral, and final gender
  separated_korean <- split_korean_word(worddata)

  res = separated_korean %>% paste0(collapse=",")
  res0 = list(worddata, separated_korean)
  res2 = data.frame(res)%>%
    t() %>% as.data.frame() %>%
    tibble::rownames_to_column("word")


  switch(type,
         text = res,
         all = res0,
         df = res2
  )
}




#' split_kw_df Create a data column by separating the initial, neutral, and final Korean characters.
#'
#' @param df data.frame
#' @param sel_col select word column
#' @param remove remove False is overlab, TRUE is generate new data
#'
#' @return data.frame
#' @export
#'
#' @examples
#' \dontrun{
#' wd <- data.frame(
#' id =  paste0("BY", 1:5),
#' word = c("토끼", "휘파람", "뻐스", "우리냥","우리낙"),
#' tone = c("HH","LHH","HL","LHH","LHL" ),
#' gender =c("F","F","M","M","M"),
#' age = c(20,60,20,60, 30),
#' area= c("n","s","n","s","s"),
#' mix= c(FALSE,FALSE,FALSE,TRUE,TRUE),
#' stringsAsFactors = FALSE)
#'
#' wd%>%split_kw_df("word")
#' wd%>%split_kw_df()
#'
#' }
#'
split_kw_df <- function(df, sel_col="word", remove = TRUE) {
  # Separate each word in the word column into initial, neutral, and final.
  if(!is.data.frame(df)){
    stop("You Must Enter as a [data frame], you need check your data ")
  }

  df = df %>% dplyr::mutate(N = nchar(word))

  spl = function(separated_word) {
    # separated_word <- paste(unlist(strsplit(word, '')), collapse = ' ')
    if(nchar(separated_word)==2){
      separated_word = paste0(separated_word,"   ")
    }else{
      separated_word
    }

    separated_korean <- split_korean_word(separated_word)
    paste(separated_korean, collapse = '')
  }

  df$syllabic <- sapply(df[, sel_col], spl)

  df = separate(df, syllabic, c(
    # paste0("init", 1:3),
    # paste0("mid", 1:3),
    # paste0("final", 1:3)
    paste0(rep(LETTERS[1:max(df$N)], each = 3),
           rep(1:3, times = 3))
  ), sep=",",
  remove = remove )

  ### REMOVe <NA>
  df[is.na(df)] <- ""

  # return(list(df, df1))
  return(df)

  ## suppres warning message
  options(warn = -1)

}




