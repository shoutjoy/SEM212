

#' effec1 is the path coefficient of the structural equation.
#'
#' @param x lavaan result
#' @param format 'markdown', 'html'
#' @param op1  first'~'
#' @param op2  first ':='  newparameters
#' @param what default 'std', Can be changeed to 'est'
#' @param digits round 3
#' @param layout 'tree2'
#' @param curve curve =1
#' @param mar margins default c(8,8,8,8)
#' @param residScale length of residuals. default 15
#' @param sizeLat Horizontal length of latent variable
#' @param sizeLat2 Vertical length of latent variable
#' @param sizeMan  horizontal length of the item
#' @param sizeMan2 Vertical length of the item
#' @param edge.label.cex edge size
#' @param label.cex  label size
#' @param asize arrow size
#' @param name_change When set to default, the variable name of the data is used, and when set to TRUE, a new variable name can be entered. Just enter them all in label_list using the c() function.
#' @param label_list label list

#' @export
#'
#' @examples
#'
#' \dontrun{
#' ## change label_list
#' abel_listnode.name =list(
#'   list(node="f인정욕구", to="인정신념"),
#'   list(node="f파국화",  to="불안신념1"),
#'   list(node="f과잉불안", to="불안신념2"),
#'   list(node="f무력감",  to="의존신념"),
#'   list(node="f개인완벽", to="해결신념"),
#'   list(node="IRB",  to="비합리적신념"),
#'   list(node="PREACT",  to="진로준비행동"),
#'   list(node="DECISION",  to="진로결정수준"),
#'   list(node="f정보수집",  to="정보수집"),
#'   list(node="f직업체험",  to="직업체험"),
#'   list(node="f진로탐색",  to="정보탐색"),
#'   list(node="f구체성",  to="구체성"),
#'   list(node="f확신성",  to="확신성"),
#'   list(node="f적합성",  to="적합성"))
#' }
#'
#'
effect1 <-function(x,format="markdown",op1="~",op2=":=",
                   what="std",
                   digits=3, layout = "tree2", curve=1,
                   mar = c(8,8,8,8),
                   residScale=15,
                   sizeLat = 12, sizeLat2 = 6,
                   sizeMan = 8, sizeMan2 = 4,
                   edge.label.cex= 0.9,
                   label.cex = 1.5,
                   asize=1.4,
                   name_change=FALSE,
                   label_list=NULL){
  library(dplyr)
  library(knitr)
  library(semPlot)
  library(semptools)

  res0 <- lvaan::parameterEstimates(x, standardized = T, rsquare = T,ci=T) %>%
    dplyr::filter(op==op1|op==op2) %>%
    dplyr::mutate(stars=ifelse(pvalue<0.001,"***",
                               ifelse(pvalue<0.01,"**",
                                      ifelse(pvalue<0.05,"*","")))) %>%
    dplyr::mutate(op=ifelse(op=="~","<--",
                            ifelse(op==":=","effect",""))) %>%
    dplyr::select(lhs,op,rhs,

                  est,
                  se,
                  z,
                  Sig=stars,
                  "std"= std.all,
                  ci.lower, ci.upper,
                  "p"=pvalue
    ) %>%
    knitr::kable(format=format, digits = 3,
                 caption ="Path coefficient & Total Effect(label)")
  # what="std",
  # digits=3, layout = "tree2", curve=1,
  # mar = c(8,8,8,8),
  # # edge.label.cex=0.9,
  # name_change=FALSE,
  # label_list=NULL

  if(name_change==FALSE){
    diagram<-semPlot::semPaths(x, what = what, fade=F, posCol="gray30",
                               edge.label.cex = 1, edge.label.position=0.4,
                               nCharNodes = 10,
                               rotation = 2, curve=curve,
                               sizeLat = 12, sizeLat2 = 6,
                               sizeMan = 8, sizeMan2 = 4,
                               layout = layout,
                               style = "lisrel", residScale = residScale,intercepts = F,
                               nDigits = digits,
                               label.cex = label.cex, asize=asize,
                               mar = mar) %>% semptools::mark_sig(x) %>%
      semptools::mark_se(x,"\n") %>%
      semptools::plot()

    res <- list(res0, diagram)
    res

  }else if(name_change==TRUE){
    diagram <- semPlot::semPaths(x, what = what, fade=F, posCol="gray30",
                                 edge.label.cex = edge.label.cex,
                                 edge.label.position=0.4,
                                 nCharNodes = 10,
                                 rotation = 2, curve=curve,
                                 sizeLat = sizeLat, sizeLat2 = sizeLat2,
                                 sizeMan = sizeMan, sizeMan2 = sizeMan2, layout = layout,
                                 style = "lisrel", residScale = 15,intercepts = F,
                                 nDigits = digits, border.width=2,
                                 label.cex = label.cex, asize=asize,
                                 mar = mar) %>% sempltools::mark_sig(x) %>%
      semptools::mark_se(x,"\n") %>%
      semptools::change_node_label(
        label_list=label_list
      ) %>%  semptools::plot()

    res <- list(res0, diagram)
    res

  }


}
