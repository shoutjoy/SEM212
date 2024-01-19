#' Fill in missing information at once
#'
#' @param data data,frame
#' @param fn mean, median
#'
#' @return result fill data
#' @export
#'
#' @examples
#' \dontrun{
#' # sleep %>% str()
#' sleep %>% ncol()

#' sleep %>% missfillinFor()
#' sleep %>% missfillinFor(fn=median)
#' }
missfillinFor <- function(data, fn=mean){

  for(i in 1:ncol(data)){
    data[i] = data[i] %>% missfillin(fn=fn)
  }
  # colSums(!is.na(data))
  data
}
