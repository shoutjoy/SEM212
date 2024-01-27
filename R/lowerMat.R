
#' lower pull matrix
#'
#' @param mat matrix data
#' @param fill matrix data fill upper 0 or if that's what you want , enter it
#' @param diag matrix data diag enter 0
#'
#' @return lowermatrix, upper 0
#' @export
#'
#'
lowerMat <- function(mat, fill = 0, diag = 0  ) {
  num_rows <- nrow(mat)
  num_cols <- ncol(mat)

  if (num_rows != num_cols) {
    stop("Input matrix is not square.")
  }

  for (i in 1:num_rows) {
    for (j in 1:num_cols) {
      if (i == j) {
        mat[i, j] <- fill
      } else if (i < j) {
        mat[i, j] <- diag
      }
    }
  }

  return(mat)
}


#' lower pull matrix fill NA
#'
#' @param mat matrix data
#' @param fill matrix data NA or .
#'
#' @return lowermatrix, upper 0
#' @export
#'
#'
lowerMat_na <- function(mat, fill=".") {
  num_rows <- nrow(mat)
  num_cols <- ncol(mat)

  if (num_rows != num_cols) {
    stop("Input matrix is not square.")
  }

  for (i in 1:num_rows) {
    for (j in 1:num_cols) {
      if (i == j) {
        mat[i, j] <- fill
      } else if (i < j) {
        mat[i, j] <- fill
      }
    }
  }

  return(mat)
}
