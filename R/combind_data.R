
#' A function that combines two matrices of data, for example, to combine observation/expectation tables and p-values.
#'
#' @param mat_observed_expected  matrix  observed/expected
#' @param mat_p_value  matirx pvalue
#' @param left  "(" etc input
#' @param right ")"
#' @return matrix
#'

#'
#'  \dontrun{
#'
#' # Example data
#' observed_expected <- matrix(c(1.0587162, 1.1477347, 0.50877193,
#'                               1.1269248, 0.5743712, 1.94957983,
#'                               0.9163776, 1.4060606, 0.03515152,
#'                               0.9530237, 0.8624314, 1.44531611),
#'                             nrow = 4, byrow = TRUE,
#'                             dimnames = list(c("공명음", "마찰음", "유기음_경음", "평파열음_평파찰음"),
#'                                            c("H", "H(H)", "L")))
#'
#' p_value <- matrix(c(0, 0.00001, 0.00000,
#'                     0, 0.00000, 0.01497,
#'                     0, 0.00013, 0.00000,
#'                     0, 0.00000, 0.00102),
#'                   nrow = 4, byrow = TRUE,
#'                   dimnames = list(c("공명음", "마찰음", "유기음_경음", "평파열음_평파찰음"),
#'                                   c("H", "H(H)", "L")))
#'
#' # Combine the data
#' combined_data <- combine_data(observed_expected, p_value)
#'  print(combined_data)
#'
#'}
#'
#'
#' @export
#'
combine_data <- function(mat_observed_expected,
                         mat_p_value,
                         left=" ",
                         right="") {
  # Extract row and column names
  row_names <- rownames(mat_observed_expected)
  col_names <- colnames(mat_observed_expected)

  # Create a combined data frame
  combined <- data.frame(matrix("",
                                nrow = nrow(mat_observed_expected),
                                ncol = ncol(mat_observed_expected)))
  row.names(combined) <- row_names

  # Fill in combined data frame with observed/expected values and p-values
  for (i in 1:nrow(mat_observed_expected)) {
    for (j in 1:ncol(mat_observed_expected)) {
      value <- mat_observed_expected[i, j]
      mat_p_value_cell <- mat_p_value[i, j]
      combined[i, j] <- paste0(value, left,
                               format(mat_p_value_cell,
                                      scientific = FALSE,
                                      digits = 5),
                               right)
    }
  }

  # Assign column names
  colnames(combined) <- col_names

  return(combined)
}
#'
#'
#'
#'
#'
#'
#' Functions that output statistical significance for matrices and dataframes Simple and exact expressions
#'
#' @param p_value_table A matrix of p-values, or other numerical matrix
#' @param simple Whether to represent the * mark simply or precisely with ***.
#'
#' @return Matrices marked with a *.
#' @export
#'
#' @examples
#'
#'  \dontrun{
#'
#'  #' p_value <- matrix(c(0, 0.00001, 0.00000,
#'                     0, 0.00000, 0.01497,
#'                     0, 0.00013, 0.00000,
#'                     0, 0.00000, 0.00102),
#'                   nrow = 4, byrow = TRUE,
#'                   dimnames = list(c("공명음", "마찰음", "유기음_경음", "평파열음_평파찰음"),
#'                                   c("H", "H(H)", "L")))
#'      # Add significance symbols
#'      # p_value
#'
#'      add_significance_symbols(p_value)
#'
#'  ##                  H     H(H)  L
#'  ##공명음            "***" "***" "***"
#'  ##마찰음            "***" "***" "*"
#'  ##유기음_경음       "***" "***" "***"
#'  ##평파열음_평파찰음 "***" "***" "**"
#'
#'
#'
#'
#'  }
#'
#'
add_significance_symbols <- function(p_value_table, simple=FALSE) {
  # Define significance levels
  significance_levels <- c(0.001, 0.01, 0.05)


  if(simple){
    # Convert p-values to significance symbols
    symbols <- matrix("", nrow = nrow(p_value_table), ncol = ncol(p_value_table))
    for (i in 1:nrow(p_value_table)) {
      for (j in 1:ncol(p_value_table)) {
        if (p_value_table[i, j] < significance_levels[1]) {
          symbols[i, j] <- "*"
        } else if (p_value_table[i, j] < significance_levels[2]) {
          symbols[i, j] <- "*"
        } else if (p_value_table[i, j] < significance_levels[3]) {
          symbols[i, j] <- "*"
        }
      }
    }
  }else{
    # Convert p-values to significance symbols
    symbols <- matrix("", nrow = nrow(p_value_table), ncol = ncol(p_value_table))
    for (i in 1:nrow(p_value_table)) {
      for (j in 1:ncol(p_value_table)) {
        if (p_value_table[i, j] < significance_levels[1]) {
          symbols[i, j] <- "***"
        } else if (p_value_table[i, j] < significance_levels[2]) {
          symbols[i, j] <- "**"
        } else if (p_value_table[i, j] < significance_levels[3]) {
          symbols[i, j] <- "*"
        }
      }
    }
  }


  # Combine symbols with p-values
  result <- p_value_table
  for (i in 1:nrow(p_value_table)) {
    for (j in 1:ncol(p_value_table)) {
      # result[i, j] <- paste0(format(p_value_table[i, j], scientific = FALSE, digits = 5), symbols[i, j])

      result[i, j] <- symbols[i, j]

    }
  }

  return(result)
}


