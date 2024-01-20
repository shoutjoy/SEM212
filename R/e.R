

#' Google translaste english  to koean
#'
#' @param text  Input text
#' @param slang  english
#' @param elang korean
#' @param show 'translate', 'browser', 'all' select 3 opttions
#'

#' @export
#'
#' @examples
#' #eexamples code
#' \dontrun{
#'
#' e("Translating English to Korean is the default option.")
#' }
#'
#'
#'
e = function (text="hello let's start translate english to korean.",
              slang="en",
              elang="ko",
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
