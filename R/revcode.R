#' Custom function to perform reverse coding for multiple variables with custom n values
#'
#' @param data Data frame containing the variables
#' @param n Vector of maximum scale values (one for each variable), c("v1","v2",...)
#' @param ... Variable names to be reverse coded
#' @param postfix postfix is reverse variable name default '_r'
#'
#' @return add reverse data in data.frame
#' @export
#'
#' @examples
#' \dontrun{
#'
#' ### How to decode all variables by applying only one n
#' revcode(stat_onl, 8, "upgrade", "satisfy", "fatigue" )|>
#' select(upgrade, satisfy, fatigue, upgrade_r, satisfy_r, fatigue_r)
#'
#' ### the method for the reverse coding variable being applied separately
#' ### to each variable and decoding
#' revcode(stat_onl, n= c(8,8,8), "upgrade", "satisfy", "fatigue" )|>
#' select(upgrade, satisfy, fatigue, upgrade_r, satisfy_r, fatigue_r)
#'
#' ## mtcars data
#' ## Since the inverse coding value of the cyl variable is 4,6,8,
#' ## n enters 4 + 8 = 12; the minimum + maximum = n value.
#' revcode(mtcars[,c("cyl", "gear","carb")], 12, "cyl")
#'
#'
#' revcode(mtcars[,c("cyl", "gear","carb")],
#'             n = c(12, 6, 9),"cyl","gear","carb")
#' }
revcode <- function(data, n = 8, ..., postfix="_r") {
  # Arguments:
  #   data: Data frame containing the variables
  #   n: Vector of maximum scale values (one for each variable)
  #   ...: Variable names to be reverse coded
  if(length(n)==1){
    # Loop through each variable
    for (var in c(...)) {
      # Reverse code the variable
      data[[paste0(var, postfix)]] <- n - data[[var]]
    }
  }else{
    # Loop through each variable
    for (i in seq_along(c(...))) {
      var <- c(...)[i]
      n_val <- n[i]

      # Reverse code the variable
      data[[paste0(var, postfix)]] <- n_val - data[[var]]
    }

  }
  return(data)
}


