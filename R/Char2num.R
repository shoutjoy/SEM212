#' Functions to convert character to numeric
#'
#' @param data data.frame
#' @param auto If TRUE, converts all variables to
#' @param iter Select the range of variables to convert
#'
#' @return  data.frame
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' ## Replacing a character variable with numeric
#' mysummary(sats, "sux1") %>%
#'     mutate(across(everything(), ~ format(., digits = 4))) %>%
#'     tibble() %>% Char2num()
#'
#'
#' ## Set remove=TRUE if you only want to process the selected variable and output only that.
#'   mysummary(sats, "sux1") %>%
#'   mutate(across(everything(), ~ format(., digits = 5))) %>%
#'     tibble() %>%Char2num(auto=FALSE, iter= 4:ncol(.), remove= TRUE)
#'
#' ##
#' mysummary(sats, "sux1") %>%
#'   mutate(across(everything(), ~ format(., digits = 5))) %>%
#'     tibble() %>%Char2num(auto=FALSE, iter= 4:ncol(.), remove= FALSE)
#' ## same
#'   mysummary(sats, "sux1") %>%
#'      mutate(across(everything(), ~ format(., digits = 5))) %>%
#'      tibble() %>%Char2num(auto=FALSE, iter= 4:ncol(.))
#'
#'
#' }
#'
#'
Char2num <- function(data, auto=TRUE, iter = 1:ncol(data), remove = FALSE){
  data = data

  #first col check

  # data processing
  if(auto){
    #  iter = iter
    if(is.numeric(data[1])){
      iter = 1:ncol(data)
    }else{
      iter = 2:ncol(data)
    }


  }else{

    if(remove){
      #data select
      data = data[,c(iter)]
      iter = seq_along(data)
    }else{
      iter = iter  # data not remove
    }

  }
  #data treatment
  for( i in iter){
    data[[i]] <- suppressWarnings(as.numeric(data[[i]]))
    data[[i]][is.na(data[[i]])  ] <- 0
  }
  #final data
  data
}
