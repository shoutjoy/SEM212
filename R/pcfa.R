
#' principal component factor analysis function
#'
#' @param R correlation matrix
#' @param nfactor number of factor
#' @param rowname matrix rowname
#' @param name 'auto', 'manual'
#' @param digits digits = 3
#' @param xlim  c(-1,1)
#' @param ylim c(-1,1)
#' @param cex  size =1
#' @param pos  positon  1
#'
#' @return result table, biplot, dimension
#' @export
#'
#' @examples
#' \dontrun{
#' R= matrix(
#' c(1.00, 0.83, 0.78, 0.70, 0.66, 0.63,
#'   0.83, 1.00, 0.67, 0.67, 0.65, 0.57,
#'   0.78, 0.67, 1.00, 0.64, 0.54, 0.51,
#'   0.70, 0.67, 0.64, 1.00, 0.45, 0.51,
#'   0.66, 0.65, 0.54, 0.45, 1.00, 0.40,
#'   0.63, 0.57, 0.51, 0.51, 0.40,1.00),
#'  byrow=T, nrow=6)
#' subject=c("classic","france","english","math","pitch","music")
#'
#' colnames(R)=subject
#' rownames(R)=subject
#' R
#' pcfa(R, nfactor = 2)
#'
#' }
#'
pcfa <- function(R,
                 nfactor=NULL,
                 rowname=NULL,
                 name="auto",
                 digits=2,
                 xlim = c(-1,1),
                 ylim = c(-1,1),
                 cex=1,
                 pos=1){

  library(tidyverse)

  if(nfactor==1){
    return(cat("다시 입력하세요 2이상으로 하세요"))

  }else if(nfactor>=2){


    eigen.R <- eigen(R)
    V = eigen.R$vectors
    gof = eigen.R$values/sum(eigen.R$values)*100

    # visualization
    Gof.c <- gof %>% data.frame()
    colnames(Gof.c)="Value"

    Gof.c <- Gof.c%>%
      dplyr::mutate(eig.prop= paste0("Dim",1: ncol(R))) %>%
      dplyr::select(eig.prop, Value)



    E = diag(sqrt(eigen.R$values[1:nfactor]))

    # main component
    V_main = V[, 1:nfactor]
    colnames(V_main)= paste0("Dim",1: nfactor)

    if(name=="auto"){
      rownames(V_main)= rownames(R)}
    if(name=="manual"){
      rownames(V_main)= rowname}

    # calculation main component
    LoadingMatrix = V_main %*% E
    colnames(LoadingMatrix) = paste0("PC",1:nfactor)
    if(name=="auto"){
      rownames(LoadingMatrix) = rownames(R)}
    if(name=="manual"){
      rownames(LoadingMatrix) = rowname}

    # LoadingMatrix %>% round(digits)


    # communality
    communality = LoadingMatrix%*%t(LoadingMatrix)

    # specific variance :PSi
    psi = diag(R-LoadingMatrix%*%t(LoadingMatrix))
    # psi

    #residual matrix
    RM = R-(LoadingMatrix%*%t(LoadingMatrix) + diag(psi))
    # RM

  }

  #eigen values propotion visualazation
  g <- Gof.c %>%
    ggplot(aes(x=eig.prop, y=Value))+
    geom_bar(stat="identity",
             aes(colour=eig.prop, fill=eig.prop))+
    geom_text(aes(label= paste0(round(Value,1),"(%)"),
                  vjust= -0.5 ))+
    theme_bw()

  #2 dimension graph
  plot(-LoadingMatrix[,1], -LoadingMatrix[,2],
       cex=cex, pch=21, bg="red",
       xlim=c(-1,1), ylim=c(-1,1))
  abline(v=0, h=0)
  text(-LoadingMatrix[,1], -LoadingMatrix[,2],
       labels = ifelse(name=="auto",rownames(R),
                       ifelse(name=="manual",rowname,"")),
       pos = pos)




  # result
  res = list(data=R,
             propDim=Gof.c,
             communality=communality,
             specific_variance = psi,
             residual_matrix=RM,
             factorloadings = -LoadingMatrix,
             prop_graph=g)

  res

}





#' pcfa_porp_plot
#'
#' @param Data factanal result uniqueness
#' @param digits digits 3
#' @param prop FALSE
#' @param ymax plot y size
#'
#' @return graph
#' @export
#'
#' @examples
#' \dontrun{
#'
#' library(MVT)
#' library(GPArotation)
#' data(examScor)
#' X = examScor
#' Z = scale(X, scale=T)  # scale
#' R = cor(X)  # correlation matrix
#'
#' ## factor analysis tools : factanal
#' mlfa <- factanal(Z, factors = 2, rotation = "varimax")
#'
#' mlfa|> pcfa_porp_plot()
#'
#' }
pcfa_porp_plot <- function(Data,
                           digits=2,
                           prop=FALSE,
                           ymax=NA
){

  data = Data$uniquenesses


  library(tidyverse)
  library(forcats)

  gof = data/sum(data)*100

  if(prop==FALSE){
    Gof.c <- gof %>% data.frame()
    colnames(Gof.c)="Value"

    Gof.c <- Gof.c%>%
      dplyr::mutate(eig.prop= paste0("Dim",1: length(data))) %>%
      dplyr::select(eig.prop, Value)

    g<-Gof.c %>%
      dplyr::mutate(eig.prop = forcats::fct_reorder(eig.prop,
                                                    dplyr::desc(Value))) %>%
      ggplot( aes(x= eig.prop, y=Value))+
      geom_bar(stat="identity",
               aes(colour=eig.prop, fill=eig.prop))+
      geom_text(aes(label= paste0(round(Value,digits),"(%)"),
                    vjust= -0.5 ))+
      ylim(0, ymax)+
      labs(title="eigen value proportion")+
      theme_bw()

  }else if(prop==TRUE){
    #pre calculation porp
    Gof.c <-   data %>% tibble::as_tibble() %>%
      dplyr::mutate("eig.prop" = rownames(R)) %>% select(2,1) %>%
      dplyr::rename(Value=value)
    # Gof.c <- Gof.c %>%  fct_reorder(eig.prop, Value)

    g<-Gof.c %>%
      dplyr::mutate(eig.prop = forcats::fct_reorder(eig.prop,
                                           dplyr::desc(Value))) %>%
      ggplot( aes(x= eig.prop, y=Value))+
      geom_bar(stat="identity",
               aes(colour=eig.prop, fill=eig.prop))+
      geom_text(aes(label= paste0(round(Value,digits),"")),
                vjust= -0.5 )+
      ylim(0, ymax)+
      labs(title="factor Uniquenesses")+
      theme_bw()
  }
  res=list(data, Gof.c, g)
  res
}
