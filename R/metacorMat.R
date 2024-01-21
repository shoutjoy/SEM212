#' Creating a meta-structural equation correlation matrix JH, Park
#' @param data data.frame, columns:  study, n, var1_var2, var1_var2, ...
#' @param startcol The column where the correlation coefficient starts, ej, and the column where the correlation coefficient ends are automatically calculated.
#' @param var Unique variable name with correlation analysis
#' @examples
#' ##make matrix
#' \dontrun{
#'  ## making correlation matrix ##
#' cormat1 = metacorMat(rd2, startcol= 3, var=c("po","ne","se","jp"))
#'  ##variable : "po_ne" "po_se" "po_jp" "ne_se" "ne_jp" "se_jp"
#'
#' rd2<- data.frame(
#'   stringsAsFactors = FALSE,
#'   study = c(1L,
#'             2L,3L,4L,5L,6L,7L,8L,9L,10L,11L,12L,
#'             13L,14L,15L,16L,17L,18L,19L,20L,
#'             21L,22L,23L,24L,25L,26L,27L,28L,29L,
#'             30L,31L,32L,33L,34L,35L,36L),
#'   n = c(671L,
#'         92L,73L,321L,214L,947L,321L,231L,
#'         531L,567L,213L,928L,269L,593L,78L,983L,
#'         1504L,247L,79L,875L,68L,201L,891L,
#'         197L,431L,98L,179L,69L,301L,679L,273L,
#'         254L,316L,781L,273L,268L),
#'   po_ne = c(NA,
#'             NA,-0.33,NA,NA,NA,NA,-0.32,NA,NA,NA,
#'             NA,NA,NA,-0.21,-0.49,NA,-0.14,-0.29,
#'             -0.27,NA,-0.37,NA,NA,-0.35,-0.17,NA,
#'             -0.11,NA,NA,-0.41,NA,NA,NA,NA,-0.39),
#'   po_se = c(NA,
#'             0.37,0.29,0.16,0.41,NA,NA,0.21,0.59,
#'             0.53,0.31,0.38,NA,NA,0.19,NA,NA,NA,
#'             NA,0.21,0.07,0.57,0.45,NA,0.18,NA,
#'             0.45,NA,NA,0.09,NA,NA,0.37,0.19,0.23,
#'             NA),
#'   po_jp = c(0.16,
#'             0.39,0.09,0.07,NA,0.04,NA,NA,0.33,
#'             0.11,NA,0.04,NA,NA,0.21,0.23,0.09,
#'             0.11,0.31,0.11,-0.07,0.14,NA,0.19,0.13,
#'             0.25,0.35,0.29,0.35,NA,0.27,0.19,NA,
#'             NA,0.13,0.19),
#'   ne_se = c(NA,
#'             -0.31,NA,NA,NA,NA,NA,-0.19,NA,NA,NA,
#'             NA,-0.49,NA,-0.45,NA,NA,NA,NA,-0.38,
#'             NA,-0.39,NA,NA,-0.21,NA,NA,NA,NA,
#'             -0.23,NA,NA,NA,NA,-0.44,NA),
#'   ne_jp = c(NA,
#'             -0.41,NA,NA,NA,-0.18,-0.11,NA,NA,NA,
#'             NA,NA,-0.21,-0.15,-0.36,-0.29,NA,
#'             -0.27,-0.29,-0.23,NA,-0.27,NA,NA,-0.19,
#'             -0.07,NA,-0.31,NA,NA,0.07,NA,NA,NA,
#'             -0.27,-0.05),
#'   se_jp = c(NA,
#'             NA,NA,0.07,NA,NA,NA,NA,0.27,0.07,NA,
#'             NA,0.48,0.26,0.41,NA,NA,NA,NA,0.31,
#'             0.19,0.34,NA,NA,0.19,NA,0.39,NA,
#'             0.57,NA,NA,NA,NA,NA,NA,NA),
#'   jobyear = c("f",
#'               "f","s","s","s","s","f","f","s","s",
#'               "s","s","f","f","f","f","f","f","s",
#'               "s","s","s","s","f","s","f","f",
#'               "f","f","f","f","s","s","s","f","f"),
#'   wage = c(9L,
#'            1L,1L,1L,4L,9L,2L,3L,3L,3L,2L,7L,
#'            1L,8L,6L,3L,5L,7L,8L,4L,7L,1L,4L,
#'            6L,7L,1L,9L,6L,7L,7L,7L,9L,2L,5L,
#'            7L,3L)
#' )
#'
#' ## cormat1
#' ## [[1]]
#' ## po ne se   jp
#' ## po 1.00 NA NA 0.16
#' ## ne   NA NA NA   NA
#' ## se   NA NA NA   NA
#' ## jp 0.16 NA NA 1.00
#' ##
#' ## [[2]]
#' ## po    ne    se    jp
#' ## po   NA    NA  0.37  0.39
#' ## ne   NA  1.00 -0.31 -0.41
#' ## se 0.37 -0.31    NA    NA
#' ## jp 0.39 -0.41    NA  1.00
#' ## 출처 meta-analaysis 경로분석모델 1단계 - 상관행렬추정 correlation using metaSEM|자유자재Paper
#'
#' ### 1. How to obtain the correlation matrix using the fixed effects model
#' library(metaSEM)
#' fixed1 <- tssem1(Cov=cormat1, n=rd2$n, method="FEM")
#' summary(fixed1)
#' coef(fixed1)
#'
#' ### random effects model
#' random = tssem1(Cov=cormat1, n=rd2$n, method="REM" , RE.type = "Diag")
#' summary(random)
#'
#' # 2. How to find correlation matrix using random effects model
#' remodel = tssem1(Cov=cormat1, n=rd2$n, method="REM" , RE.type = "Diag")
#' random1 = summary(remodel)
#' mat1 <- vec2symMat(matrix(random1$coefficients[1:6,1]), diag = FALSE)
#' colnames(mat1)=c("po","ne","se","jp")
#' rownames(mat1)=c("po","ne","se","jp")
#' mat1
#'
#' ##path modeling
#' ### step1
#' rem = tssem1(Cov=cormat1, n=rd2$n, method="REM" , RE.type = "Diag")
#' summary(rem)
#'
#' ## path model to A
#' ## se = b31*po + b32*ne +e3
#' ## jp = b41*po + b42*ne + b43*se +e4
#' A= create.mxMatrix(
#' c(0,0,0,0,
#'   0,0,0,0,
#'   "0.1*b31","0.1*b32",0,0,
#'   "0.1*b41","0.1*b42","0.1*b43",0),
#' type="Full", nrow=4, ncol=4, byrow=TRUE, name="A")
#' A
#'
#' #exogeous correlation matrix
#' varnames <- c("po","ne","se","jp")
#' S= create.mxMatrix(
#' c(1,
#'   "0.1*p21",1,
#'   0,        0,"1*p33",
#'   0,        0,      0,"1*p44"),
#'   type="Symm", byrow = TRUE,
#'   name="S",
#'   dimnames=list(varnames, varnames)
#' )
#' S
#'
#'
#' ### step 2
#' stage2A = tssem2(rem, Amatrix = A, Smatrix = S, diag.constraints = TRUE)
#' summary(stage2A)
#' plot(stage2A, what="est", edge.label.cex = 0.9, edge.label.position=0.7,
#'     nDigits=3, layout = "spring")
#'
#'
#' ###Alternative model
#' A2 = create.mxMatrix(
#' c(0,0,0,0,
#'   0,0,0,0,
#'   "0.1*b31","0.1*b32",0,0,
#'   0,"0.1*b42","0.1*b43",0),
#' type="Full", nrow=4, ncol=4, byrow=TRUE, name="A")
#'  A2
#'
#'
#' stage3A = tssem2(rem, Amatrix = A2, Smatrix = S,
#'                  diag.constraints = TRUE, intervals.type = "LB")
#' summary(stage3A)
#'
#' plot(stage3A, what="est", edge.label.cex = 1, edge.color="gray20",
#' edge.label.position=0.5, nDigits=4,
#' layout = "spring", style="lisrel",residScale=16)
#'
#' # Test the significance of indirect effect
#' stage4A = tssem2(rem, Amatrix = A2, Smatrix = S, diag.constraints = TRUE,
#'           intervals.type = "LB",
#'           mx.algebras = list(
#'                    IDEpotojp = mxAlgebra(b31*b43, name="IDEpotojp"),
#'                    IDEnetojp = mxAlgebra(b32*b43, name="IDEnetojp")
#'                   ))
#'   summary(stage4A)
#'
#' ## mxAlgebras objects (and their 95% likelihood-based CIs):
#' ## lbound    Estimate      ubound
#' ## IDEpotojp[1,1]  0.05315542  0.08413089  0.11799399
#' ## IDEnetojp[1,1] -0.10185142 -0.06355753 -0.03440116
#'
#' }
#'
#' @export
metacorMat = function(data, startcol , var="")
{
  # library(metaSEM)
  # make list of cormatrices (cordat), NA on diagonal
  cormat = list()

  varnames <- var
  nvar <- length(varnames)
  ###### label for correlation matrix #########
  labels <- list(varnames,varnames)

  ###### number of correlations ###
  ncor = nvar*(nvar-1)/2

  ###### cor. are from column sj to ej ###
  sj = startcol
  ej = sj+ncor-1



  for (i in 1:nrow(data)){
    cormat[[i]] = metaSEM::vec2symMat(as.matrix(data[i,sj:ej]),diag = FALSE)
    dimnames(cormat[[i]]) = labels
  } ;
  # put NA on diagonal if variable is missing
  for (i in 1:length(cormat)){
    for (j in 1:nrow(cormat[[i]])){
      if (sum(is.na(cormat[[i]][j,]))==nvar-1)
      {cormat[[i]][j,j] = NA}
    }} ;

  # put NA on diagonal for variable with least present correlations
  for (i in 1:length(cormat)){
    for (j in 1:nrow(cormat[[i]])){
      for (k in 1:nvar){
        if (is.na(cormat[[i]][j,k])==TRUE
            &is.na(cormat[[i]][j,j])!=TRUE
            &is.na(cormat[[i]][k,k])!=TRUE){
          if(sum(is.na(cormat[[i]])[j,])>sum(is.na(cormat[[i]])[k,]))
          {cormat[[i]][k,k] = NA}
          if(sum(is.na(cormat[[i]])[j,])<=sum(is.na(cormat[[i]])[k,]))
          {cormat[[i]][j,j] = NA}
        }}}} ;
  #
  cormat
}



