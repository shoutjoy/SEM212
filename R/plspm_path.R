
#' path create
#' @param from var1
#' @param to var2
#' @export
path <- function(from, to) {
  return(list(from = from,
              to = to))
}



#' plspm path create
#'
#' @param row_names rowname
#' @param col_names colname
#' @param relationship input path
#'
#' @return matrix
#' @export
#'
#' @examples
#'
#' \dontrun{
#' edu_path <- plspm_path(
#'   row_names = c("Support", "Advising", "Tutoring", "Value", "Satisfaction", "Loyalty"),
#'   col_names = c("Support", "Advising", "Tutoring", "Value", "Satisfaction", "Loyalty"),
#'   relationship = list(
#'     list(from = "Value",
#'          to = c("Support", "Advising", "Tutoring")),
#'     list(from = "Satisfaction",
#'          to = c("Support", "Advising", "Tutoring","Value")),
#'     list(from = "Loyalty",
#'          to =c("Satisfaction"))
#'   )
#' )
#' edu_path
#'
#'
#' edu_path <- plspm_path(
#'   row_names = c("Support", "Advising", "Tutoring", "Value", "Satisfaction", "Loyalty"),
#'   col_names = c("Support", "Advising", "Tutoring", "Value", "Satisfaction", "Loyalty"),
#'   relationship = list(
#'
#'     path(form = "Support", to ="Advising"),
#'     path(form = "Tutoring", to ="Advising"),
#'     path(from = "Value",
#'          to = c("Support", "Advising", "Tutoring")),
#'     path(from = "Satisfaction",
#'          to = c("Support", "Advising", "Tutoring","Value")),
#'     path (from = "Loyalty",
#'          to =c("Satisfaction"))
#'   )
#' )
#' edu_path
#'
#' }
plspm_path <- function(row_names = c("r1","r2","r3"),
                        col_names = NULL,
                        relationship = NULL) {

  if(is.null(col_names)){
    col_names <- row_names
    num_rows <- length(row_names)
    num_cols <- length(col_names)

  }else{
    num_rows <- length(row_names)
    num_cols <- length(col_names)
  }

  path_matrix <- matrix(0,
                        nrow = num_rows,
                        ncol = num_cols,
                        dimnames = list(row_names, col_names))

  if (!is.null(relationship)) {
    for (path in relationship) {
      from <- path$from
      to <- path$to
      path_matrix[from, to] <- 1
    }
  }

  return(path_matrix)
}


