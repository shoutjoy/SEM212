#' Title
#'
#' @param data data.frame
#' @param format markdown, html
#' @param title your name
#' @param sort sorting default FALSE
#' @param prop default TRUE
#' @param arrange input 'desc' . default none ""
#'
#' @return table
#' @export
#'
#' @examples
#' \dontrun{
#' mtcars$am%>% SEM212::Freq_table()
#' }
#'
#'
Freq_table <- function(data,
                       format="markdown",  #df: dataframe
                       title="Frequency output function",
                       sort=FALSE,
                       prop=TRUE,
                       arrange=""){
  library(tidyverse)
  library(knitr)

  if(prop==TRUE){
    if(sort==TRUE){
      if(arrange=="desc"){
        res <-  data%>% table() %>% as.data.frame() %>%
          mutate("Proportion(%)" = paste(round(Freq/length(data)*100,2),"%"))%>%
          arrange(desc(Freq)) %>%
          `colnames<-`(c("Variable","Freq","Prop"))


      }else if(arrange==""){
        res <-   data%>% table() %>% as.data.frame() %>%
          mutate("Proportion(%)" = paste(round(Freq/length(data)*100,2),"%"))%>%
          arrange(Freq) %>%
          `colnames<-`(c("Variable","Freq","Prop"))

      }

    }else if(sort==FALSE){
      res <-     data%>% table() %>% as.data.frame() %>%
        mutate("Proportion(%)" = paste(round(Freq/length(data)*100,2),"%")) %>%
        `colnames<-`(c("Variable","Freq","Prop"))

    }
  }else if(prop==FALSE){
    res <-  data%>% table() %>% as.data.frame()
   }
}



#' Freq_table <- function(data){
#'   res <-  data%>% table() %>% as.data.frame() %>%
#'     mutate("Proportion(%)" = paste(round(Freq/length(data)*100,2),"%"))%>%
#'     arrange(desc(Freq))%>%`colnames<-`(c("Variable","Freq","Prop"))
#'   res
#' }
