#' number comma
#'
#' @param x  vector
#' @param digits  digits 2
#' @param big_mark  "," is default
#'
#' @return include comma
#' @export
#'
#' @examples
#'
#' \dontrun{
#' comma(3452345)
#' comma(.12358124331)
#' }
comma <- function(x, digits = 2, big_mark = ",") {
  format(x,
         digits = digits ,
         big.mark = big_mark)
}
