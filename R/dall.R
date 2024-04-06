
#' view data.frame
#'
#' @param data tibble
#'
#' @return data.frame
#' @export
#'

dall= function(data){
  data %>% data.frame()
}



#' Print all
#'
#' @param data tibble
#'
#' @return view all
#' @export
#'

pall= function(data){
  data %>%print(n=Inf)
}
