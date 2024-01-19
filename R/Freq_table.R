
#' Freq_table  Frequency output function------------
#'
#' @param data data.frame
#'
#' @export
#'
#'
#'
#'
Freq_table <- function(data){
  res <-  data%>% table() %>% as.data.frame() %>%
    mutate("Proportion(%)" = paste(round(Freq/length(data)*100,2),"%"))%>%
    arrange(desc(Freq))%>%`colnames<-`(c("Variable","Freq","Prop"))
  res
}



# Freq_table <- function(data,
#                        format="markdown",  #df: dataframe
#                        title="Frequency output function",
#                        sort=FALSE,
#                        prop=TRUE,
#                        arrange=""   # desc
# ){
#   library(tidyverse)
#   library(knitr)
#
#   if(prop==TRUE){
#     if(sort==TRUE){
#       if(arrange=="desc"){
#         res <-  data%>% table() %>% as.data.frame() %>%
#           mutate("Proportion(%)" = paste(round(Freq/length(data)*100,2),"%"))%>%
#           arrange(desc(Freq)) %>%
#           `colnames<-`(c("Variable","Freq","Prop"))
#
#
#       }else if(arrange==""){
#         res <-   data%>% table() %>% as.data.frame() %>%
#           mutate("Proportion(%)" = paste(round(Freq/length(data)*100,2),"%"))%>%
#           arrange(Freq) %>%
#           `colnames<-`(c("Variable","Freq","Prop"))
#
#       }
#
#     }else if(sort==FALSE){
#       res <-     data%>% table() %>% as.data.frame() %>%
#         mutate("Proportion(%)" = paste(round(Freq/length(data)*100,2),"%")) %>%
#         `colnames<-`(c("Variable","Freq","Prop"))
#
#     }
#   }else if(prop==FALSE){
#     res <-  data%>% table() %>% as.data.frame()
#   }
#
#
#
#
#   #result summary
#   if(format=="markdown"){
#     res %>%
#       kable(caption = title, format = format)
#   }else if(format=="df"){
#     res
#   }
# }
