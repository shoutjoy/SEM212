#' meancentering mixed model
#' @param data data.frame
#' @param grp_id group id
#' @param gv  group variable, Group Variables That Separate Groups
#' @param ...  individual measurement independent variables evaluated at individual level
#' @export
#' @examples
#' \dontrun{
#' meanCentering_df(mtcars[,c("cyl","mpg","wt","hp")],"cyl","mpg","wt","hp")|> data.frame()
#' }
#'
#'
#'
meanCentering_df<- function(data, grp_id,
                            gv, ... ){
  # ... means the variables you want to add.
  #For example, if you want to add v1, v2, v3, v4, enter gv, v1, v2, v3, v4.

  #Determines the mean and number for gv.
  data_df = aggregate(formula(paste(gv, "~", grp_id)), data, mean)
  data_df$gsize = aggregate(formula(paste(gv, "~", grp_id)), data,length)[,2]

  # Mean the variables corresponding to ... and add them as new variables.

  for (var in list(...)) {
    data_df[[paste0("gm", var)]] = aggregate(formula(paste(var, "~", grp_id)), data, mean)[,2]
  }

  #mean centering
  data_df[[paste0("am.", gv) ]] = data_df[,gv] - mean(data_df[,gv] )
  data_df$am.gsize = data_df$gsize - mean(data_df$gsize)


  #  Do the mean centering on the variables corresponding to # ... and add them as new variables.
  for (var in list(...)) {
    data_df[[paste0("am.gm", var)]] = data_df[[paste0("gm", var)]] - mean(data_df[[paste0("gm", var)]])
  }

  ## result
  # tibble::tibble(data_df )
  res = dplyr::inner_join(dplyr::select(data,-all_of(gv)),
                          # dplyr::select(data_person,-all_of(gv))
                          data_df,
                          by = grp_id)

  #personal mean centering
  for (var in list(...)) {
    res[[paste0("gm.", var)]] = res[[var]] - res[[paste0("gm", var)]]
  }
  #result
  return(tibble::tibble(res))
}
#'
#'
# meanCentering_df<- function(data, grp_id,
#                             gv, ... ){
#
#   # ... means the variables you want to add.
#   #For example, if you want to add v1, v2, v3, v4, enter gv, v1, v2, v3, v4.
#
#   #Determines the mean and number for gv.
#   data_df = aggregate(formula(paste(gv, "~", grp_id)), data, mean)
#   data_df$gsize = aggregate(formula(paste(gv, "~", grp_id)), data,length)[,2]
#
#   # Mean the variables corresponding to ... and add them as new variables.
#
#   for (var in list(...)) {
#     data_df[[paste0("gm", var)]] = aggregate(formula(paste(var, "~", grp_id)),
#                                              data, mean)[,2]
#   }
#
#   #mean centering
#   data_df[[paste0("am.", gv) ]] = data_df[,gv] - mean(data_df[,gv] )
#   data_df$am.gsize = data_df$gsize - mean(data_df$gsize)
#
#
#   #  Do the mean centering on the variables corresponding to # ... and add them as new variables.
#   for (var in list(...)) {
#     data_df[[paste0("am.gm", var)]] = data_df[[paste0("gm", var)]] - mean(data_df[[paste0("gm", var)]])
#   }
#
#   ## result
#   # tibble::tibble(data_df )
#   res = dplyr::inner_join(dplyr::select(data,-all_of(gv) ),
#                           data_df,
#                           by = grp_id)
#   # Result to tibble data
#   tibble::tibble(res)
# }
