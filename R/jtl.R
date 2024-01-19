
#' @param text text to translate
#' @param slang starting language
#' @param elang ending language
#' @param translator translator type. lists are above.
#' @param print_url Debugging Setting
#' @export
jtl <-  (function() {
  library("curl")
  host = nslookup("jaehyung101.synology.me");
  host = sprintf("http://%s:10147", host);

  #' 사용할 수 있는 translator 목록
  #'
  #' [
  #'   "alibaba", "apertium", "argos", "baidu", "bing",
  #'   "caiyun", "cloudTranslation", "deepl", "elia", "google",
  #'   "iciba", "iflytek", "iflyrec", "itranslate", "judic", "languageWire",
  #'   "lingvanex", "niutrans", "mglip", "mirai", "modernMt", "myMemory", "papago",
  #'   "qqFanyi", "qqTranSmart", "reverso", "sogou", "sysTran", "tilde", "translateCom",
  #'   "translateMe", "utibet", "volcEngine", "yandex", "yeekit", "youdao"
  #' ]
  #'

  function(text="", slang='ko', elang='en', translator='deepl', print_url=F) {
    library("httr");
    library("httpuv");
    url = sprintf("%s/%s/%s/%s?text=%s", host, translator, slang, elang, encodeURIComponent(text))
    response <- GET(url)
    if(print_url) print(url);
    content(response, "text", encoding="UTF-8")
  }
})#();
