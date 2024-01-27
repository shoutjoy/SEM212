
#' lower pull matrix
#'
#' @param mat matrix data
#'
#' @return lowermatrix, upper 0
#' @export
#'
#'
lowerMat <- function(mat) {
  num_rows <- nrow(mat)
  num_cols <- ncol(mat)

  if (num_rows != num_cols) {
    stop("Input matrix is not square.")
  }

  for (i in 1:num_rows) {
    for (j in 1:num_cols) {
      if (i == j) {
        mat[i, j] <- 0
      } else if (i < j) {
        mat[i, j] <- 0
      }
    }
  }

  return(mat)
}
