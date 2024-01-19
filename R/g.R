

#' Googole Translate korean to english
#'
#' @param text input your text
#' @param slang start language. default Korean
#' @param elang translated language. default English
#' @param show show Methode. 'translate' is console, 'browser'  is Print to your computer's browser, 'all' are There are two ways to output it.
#' @examples
#' # example code
#' \dontrun{
#' #korean to english : default
#' g("안녕하세요 번역을 시작합니다!(by 박중희).", slang="ko", elang= "en")
#' g("안녕하세요 번역을 시작합니다!(by 박중희).", "ko", "en")
#' # english to korean
#' g("Hello, let’s start translating! (by Park Joong-hee", slang="ko", elang= "en")
#' g("Hello, let’s start translating! (by Park Joong-hee","ko","en")
#' #japan to korean
#' g("こんにちは翻訳を始めます！（byパク・ジュンヒ", slang="ja", elang= "ko")
#'
#' # ko: korean , en: English , ja: Japan, china : zh
#' # https://cloud.google.com/translate/docs/languages?hl=ko
#' }
#'
#'
g = function (text="안녕하세요 번역을 시작합니다!(by 박중희).",
              slang="ko",
              elang="en",
              show="translate") {
  library(tidyverse)
  library(rvest)
  library(httpuv)

  url = sprintf("https://translate.google.co.kr/m?sl=%s&hl=%s&q=%s",
                slang,
                elang,
                encodeURIComponent(text))

  node = html_nodes(read_html(url), ".result-container")

  result= html_text(node)


  if(show=="all"){
    res = list(input_text= text,
               translate= result,
               View_browse=browseURL(url)
    )
    res
  }else if(show=="translate"){
    res = result
    cat(res)

  }else if(show== "browser"){
    res = paste("Input text:\n\n" ,input_text = text,
                "\n",
                "\n",
                "Translate result:\n\n",
                translate= result)
    cat(res)
    # res
  }

}
