#'skew계산 함수
#'
#'
#' @param x data  vector
#'
#' @return skewness
#' @export
#'
#'
SKEW <- function(x){

  m3 <- mean((x - mean(x))^3)
  skewness <- m3/(sd(x)^3)
  skewness
}
