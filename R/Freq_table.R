
#' Freq_table  Frequency output function------------
#'
#' @param data data.frame
#' @param format markdown
#' @param title Frequency output function
#' @param sort data sort
#' @param prop add proportion
#' @param arrange desc
#'
#' @export
Freq_table <- function(data,
                       format="markdown",  #df: dataframe
                       title="Frequency output function",
                       sort=FALSE,
                       prop=TRUE,
                       arrange=""   # desc
){
  library(tidyverse)
  library(knitr)

  if(prop==TRUE){
    if(sort==TRUE){
      if(arrange=="desc"){
        res <-  data%>% table() %>% as.data.frame() %>%
          mutate("Proportion(%)" = paste(round(Freq/length(data)*100,2),"%"))%>%
          arrange(desc(Freq))

      }else if(arrange==""){
        res <-   data%>% table() %>% as.data.frame() %>%
          mutate("Proportion(%)" = paste(round(Freq/length(data)*100,2),"%"))%>%
          arrange(Freq)
      }

    }else if(sort==FALSE){
      res <-     data%>% table() %>% as.data.frame() %>%
        mutate("Proportion(%)" = paste(round(Freq/length(data)*100,2),"%"))
    }
  }else if(prop==FALSE){
    res <-  data%>% table() %>% as.data.frame()
  }


  colnmaes(res)=c("Variable","Freq","Prop")
  #result summary
  if(format=="markdown"){
    res %>%
      kable(caption = title, format = format)
  }else if(format=="df"){
    res
  }
}
