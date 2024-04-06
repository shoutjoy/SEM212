#' Search for and fill in empty data
#'
#' @param df data.frame
#' @param pattern default ""
#' @param imp iimputation , defalut NA
#'
#' @return data.frame
#' @export
#'
replace_df = function(df, imp = NA, pattern = "" ){
  df[df == pattern ] <- imp

  df
}
