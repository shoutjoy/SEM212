#' Separate words into syllables
#'
#' @param word Words being typed
#'
#' @return Syllable-separated data
#' @export
#'
#' @examples
#'
#' \dontrun{
#' split_word("HLL")
#' split_word("호랑이")
#'
#' }
#'
split_word <- function(word) {
  # 입력된 단어를 음절 단위로 분리
  return(strsplit(word, split = "")[[1]])
}
