#' kurtosis 계산 함수
#'
#' @param x vector
#'
#' @return kurtosis
#' @export

KURT <- function(x){

  m4 <- mean((x - mean(x))^4)
  kurtosis <- m4/(sd(x)^4) - 3
  kurtosis
}
