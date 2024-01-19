#
#' Check and resolve missing data-
#'
#' @param data data.frame
#' @param plot missing data plot
#' @description
#' If there are missing values, print how many there are
#'
#' @return Results of checking for missingness
#' @export
#'
#' @examples
#' \dontrun{
#'  sleep %>% str()
#' sleep %>% missCheck()
#' }
#'
#'
missCheck <- function(data, plot=FALSE){
  library(dplyr)
  library(VIM)
  #
  # aggr_data_total  <- aggr(data, prop=FALSE, numbers= TRUE, plot = plot)$missings
  # aggr_data  <-aggr(data, prop=FALSE, numbers= TRUE, plot=plot)$missings%>%
  #   filter(Count !=0)

  library(Hmisc)
  #impute
  datacount_total <- colSums(!is.na(data))
  datacount <- colSums(!is.na(data))


  table_data<-cbind.data.frame(aggr(data, prop=FALSE, numbers= TRUE,
                                    plot = plot)$missings[1],
                               V_count=colSums(!is.na(data)) %>% as.data.frame(),
                               aggr(data, prop=FALSE, numbers= TRUE,plot = plot)$missings[2]
  )

  table_data_miss <- table_data %>% dplyr::filter(Count != 0)
  res <- table_data_miss
  # res<- list(    # aggr_data,
  #            # datacount,
  #            table_data
  #            # table_data_miss
  #            )
  res
}

