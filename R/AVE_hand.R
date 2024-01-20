
#' AVE by hnad
#' @param x lavaan result
#' @param digit rounding
#' @param markdown data.frame or markdown. default FALSE
#' @export


AVE <-function(x, digit=3, markdown = FALSE){
  library(knitr)
  l.matrix <- lavaan::lavInspect(x,"std")$lambda
  l.matrix[l.matrix==0] <- NA   #불필요한 것은 삭제
  AVE.1<-apply(l.matrix^2, 2, mean, na.rm=T)

  if(markdown==TRUE){
    AVE<- AVE.1 %>% knitr::kable(digits=digit, format="pandoc",
                          caption="AVE(average variance extracted)>0.5")
  }else{
    AVE.1<-apply(l.matrix^2, 2, mean, na.rm=T)
  }

  AVE.1 %>% graphics::barplot(ylim=c(0,1))
  graphics::text(x= AVE.1 %>%
                   graphics::barplot(ylim=c(0,1)),
                y = AVE.1+0.03,
                label= round(AVE.1,2))
  graphics::abline(h=0.5, lty=2, col="red")

  res = list(AVE, AVE.1) #result
  res

}
