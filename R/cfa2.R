#' Create CFA result summary by lavaan sem estimated result By Park Joonghee Ph.D
#' @param x lavaan result
#' @param format knitr markdown decision
#' @param dataset this is import name that dataset column name
#' @param model lavaan model need indices validation name
#' @param cut loading criteran
#' @param angle tesxt angle
#' @param cex text size
#' @param hjust on text value gap
#' @param val.size text size
#' @param dis.sort item sort
#' @param rename item validiation new name apply
#' @param var_name new text name default NULL
#' @param digits value rounding
#' @param res result type
#' @importFrom magrittr %>%

#' @examples
#' # example code
#' \dontrun{
#'
#' library(lavaan)
#' data(PoliticalDemocracy)
#'
#'  model1 <- "
#'  # measurement model
#'    ind60 =~ x1 + x2 + x3
#'    dem60 =~ y1 + y2 + y3 + y4
#'    dem65 =~ y5 + y6 + y7 + y8
#'  # regressions
#'    dem60 ~ ind60
#'    dem65 ~ ind60 + dem60
#'  # residual correlations
#'    y1 ~~ y5
#'    y2 ~~ y4 + y6
#'    y3 ~~ y7
#'    y4 ~~ y8
#'    y6 ~~ y8"
#'
#'

#' #' fit1 <- sem(model1, data = PoliticalDemocracy)
#' ## summary(fit2, standardized = TRUE)
#'
#' ## model 2 generate
#' model2 <- "
#'  ## measurement model
#'    ind60 =~ x1 + x2 + x3
#'    dem60 =~ y1 + y2 + y3 + y4
#'    dem65 =~ y5 + y6 + y7 + y8
#'  ## regressions
#'    dem60 ~ ind60
#'    dem65 ~ ind60 + dem60 "
#'
#'
#' fit2 <- sem(model2, data = PoliticalDemocracy)
#' ## summary(fit2, standardized = TRUE)
#' ##
#'  cfa2(fit1)
#' ## if you wnat HTMT value, input model and dataset
#' cfa2(fit1, dataset = PoliticalDemocracy, model = model )
#'
#'  cfa2(fit2)
#' ## if you wnat HTMT value, input model and dataset
#' cfa2(fit2, dataset = PoliticalDemocracy, model = model )
#'
#' #compare model fit and different values
#' CompareFit_diff(fit1, fit2)
#'

#' # AVE calculatin by hand
#' AVE(fit1)
#' AVE(fit2)
#' # model fit
#' cfa2(fit1, dataset = PoliticalDemocracy, model = model,
#'      res =  "modelfit")
#'
#' # item validattiy
#' cfa2(fit1, dataset = PoliticalDemocracy, model = model,
#'      res =  "loadings")
#'
#' # Internal consistency
#' cfa2(fit1, dataset = PoliticalDemocracy, model = model,
#'      res =  "alpha")
#'
# Convergent validity
#' cfa2(fit1, dataset = PoliticalDemocracy, model = model,
#'      res =  "CR_AVE")
#'
#' # Discriminant
#' cfa2(fit1, dataset = PoliticalDemocracy, model = model,
#'      res =  "Discriminant")
#'
#' # HTMT
#'
#' cfa2(fit1, dataset = PoliticalDemocracy, model = model,
#'      format="markdown",
#'      res =  "htmt")
#'
#' cfa2(fit, dataset = PoliticalDemocracy, model = model,
#'      format="html",
#'      res =  "htmt")%>%
#'   kable_minimal(full_width =F)
#'
#' # structure correlation
#' cfa2(fit1, dataset = PoliticalDemocracy, model = model,
#'       res =  "str_cor")
#' }


#' @export
#cfa2 CFA lavaan SEM----
cfa2 <- function(x, format="markdown",
                 dataset=NA, #dataset input htmt
                 model=NA, # lavaan Model htmt(<0.9)
                 cut=0.7,
                 angle=90,
                 cex=11, hjust=0.9,
                 val.size=4,
                 dis.sort=TRUE,
                 rename=F,
                 var_name=NA,
                 digits=3,
                 res="all"){

  library(dplyr)
  library(knitr)
  library(lavaan)
  library(semTools)
  library(tibble)
  library(semPlot)
  library(ggplot2)
  library(kableExtra)
  library(tidyverse)

  # tryCatch({

  # 01 fit table-----
  options(scipen = 100)

  fit.indices=c("chisq","pvalue", "df","rmsea",
                "gfi","agfi","srmr","cfi","tli","nfi","aic","bic")
  fitMeasures <- round(fitMeasures(x,fit.indices),3)
  fitMeasures_s <- round(fitMeasures(x,fit.indices),3)


  # fitMeasures <- as.data.frame(fitMeasures(x,fit.indices))
  fitMeasures <- as.data.frame(fitMeasures) #check.names = TRUE
  fitMeasures$critera <- c("",
                           "*p.value >= 0.05",
                           "_chisq/df <= 3(<5(ok)",
                           "*RMSEA< 0.05(or 0.08)",
                           "*GFI >= 0.95",
                           "_AGFI>= 0.90",
                           "*SRMR < 0.08",
                           "*CFI >= 0.95",
                           "_TLI >= 0.90",
                           "_NFI >= 0.90",
                           "_lower",
                           "_lower")
  fitMeasures$Ref <-c("-",
                      "-",
                      "Wheaton et al.(1977)",
                      "Browne & Cudek(1993)",
                      "Joreskog-Sorbom(1970)",
                      "Tanaka & Huba(1985)",
                      "Hu & Bentler(1999)",
                      "Kline(2011)",
                      "Bentler & Bonett(1980)",
                      "Bollen(1989)",
                      "Akaike(1973)",
                      "-")
  fitMeasures$chiq_df <- c("","",
                           round(fitMeasures[1,1]/fitMeasures[3,1],2),
                           "","","","","","","","","")
  # fitMeasures$fit_chek  <- c("absolute fit","",
  #                            "absolute fit",
  #                            "absolute fit ",
  #                            "absolute fit ",
  #                            "absolute fit ",
  #                            "absolute fit ",
  #                            "incremental fit",
  #                            "incremental fit",
  #                            "incremental fit",
  #                            "parsimonious fit",
  #                            "parsimonious fit")
  fit <- fitMeasures  %>%
    knitr::kable(digits=3, format=format,
          caption="FitMeasure and criterian
          (*)satisfy By kline(2011) Suggestion")

  # TEST[[2]]$test %in% c("satorra.bentler", "yuan.bentler.mplus", "yuan.bentler")
  # if(length(fitMeasures(x)) == 45 ){
  if(length(fitMeasures(x)) == length(fitMeasures(x)) ){

    #generarl reasearch
    #modelfit
    fitdata_00 <- fitMeasures(x,c("chisq","df","pvalue",
                                  "rmsea",
                                  "rmsea.ci.lower",
                                  "rmsea.ci.upper",
                                  "rmsea.pvalue",
                                  "srmr",
                                  "gfi",
                                  "cfi",
                                  "tli",
                                  "aic",
                                  "bic"
    ))

    criteria_data_00 = c("Chisq",
                         "df",
                         "p >0.05",
                         "RMSEA <0.05",
                         "90%CI.lower",
                         "90%CI.upper",
                         "p <= 0.05",
                         "SRMR <0.08",
                         "GFI >0.95",
                         "CFI >0.95",
                         "TLI >0.90",
                         "lower ",
                         "lower "
    )

    modelfitdata <-cbind.data.frame("criterian" = criteria_data_00,
                                    "Value" = round(fitdata_00,3))

  }else{

    #01-2 robust research--------
    #modelfit
    fitdata <- fitMeasures(x,c("chisq","df","pvalue",
                               "rmsea",
                               "rmsea.ci.lower",
                               "rmsea.ci.upper",
                               "rmsea.pvalue",
                               "srmr",
                               "gfi",
                               "cfi",
                               "tli",
                               "aic",
                               "bic",
                               "chisq.scaled", #roburst chisq
                               "df.scaled",
                               "pvalue.scaled",
                               "chisq.scaling.factor",
                               "cfi.robust",   # add
                               "tli.robust",
                               "rmsea.robust",
                               "rmsea.ci.lower.robust",
                               "rmsea.ci.upper.robust",
                               "rmsea.pvalue.robust",
                               "srmr_bentler",
                               "srmr_mplus"

    ))

    criteria_data = c("Chisq",
                      "df",
                      "p >0.05",
                      "RMSEA <0.05",
                      "90%CI.lower",
                      "90%CI.upper",
                      "p <= 0.05",
                      "SRMR <0.08",
                      "GFI >0.90",
                      "CFI >0.95",
                      "TLI >0.90",
                      "lower ",
                      "lower ",
                      "chisq.robust", #roburst chisq
                      "df.robust",
                      "p.robust",
                      "Satorra-Bentler correction",
                      "CFI.robust",   # add
                      "TLI.robust",
                      "RMSEA.robust",
                      "RMSEA.ci.lower.robust",
                      "RMSEA.ci.upper.robust",
                      "RMASE.p.robust(blank=NA)",
                      "SRMR_bentler",
                      "SRMR_Mplus"
    )

    modelfitdata <-cbind("criterian"=criteria_data,
                         "Value"=round(fitdata,3))

  }

#summary
  fitMeasures_s1 <- modelfitdata %>%
    knitr::kable(format=format,
           caption = "01 Model fit information")




  ## 02 factor loading-----
  options(knitr.kable.NA="")

  factorloading_0 <- lavaan::parameterEstimates(x, standardized=TRUE) %>%
    dplyr::filter(op=="=~") %>%
    mutate(stars=ifelse(pvalue < 0.001, "***",
                        ifelse(pvalue < 0.01, "**",
                               ifelse(pvalue < 0.05, "*", "")))) %>%
    mutate(label=ifelse(std.all>0.7,"Yes(Good)",
                        ifelse(std.all>0.5,"Yes(fair)","No"))) %>%
    dplyr::select("Latent"=lhs, Item=rhs, Est=est,S.E.=se,
                  cr=z, Sig.=stars, "p"=pvalue,
                  std=std.all, Accept=label)



  ### factpr loadings Input New variable  ------
  if(rename == TRUE){

    factorloading <- factorloading_0 %>% mutate(Indicator= var_name)%>%
      dplyr::select(Latent,Item ,Indicator , Est, S.E., cr, Sig., p ,
                    std, Accept
      ) %>%
      knitr::kable(digits=3, format=format,
            caption="02 Indicator Validity(1)
          Factor Loadings:
          (1) cr(critical ratio = Estimate/S.E) p<0.05,
          (2) std.damda >= 0.5(Bagozzi & Yi(1988)")

  }else{

    factorloading <-factorloading_0 %>%
      knitr::kable(digits=3, format=format,
            caption="02 Indicator Validity(1)
          Factor Loadings:
          (1) cr(critical ratio = Estimate/S.E) p<0.05,
          (2) std.damda >= 0.5(Bagozzi & Yi(1988)")
  }






  dataplot0 <-  lavaan::parameterEstimates(x, standardized=TRUE) %>%
    dplyr::filter(op=="=~") %>%
    mutate(stars=ifelse(pvalue < 0.001, "***",
                        ifelse(pvalue < 0.01, "**",
                               ifelse(pvalue < 0.05, "*", "")))) %>%
    mutate(label=ifelse(std.all>0.7,"Yes(Good)",
                        ifelse(std.all>0.5,"Yes(fair)","No"))) %>%
    dplyr::select("Latent"=lhs, Item=rhs, Est=est,S.E.=se,
                  cr=z, Sig.=stars, "p"=pvalue,
                  std=std.all, beta_Accept=label)






  #plotting data
  dataplot<- dataplot0%>% select(Item,Latent, std)



  varnames_check = dataplot0[,"Item"]
  #02 -2 loadings -ggplot------

  if(rename == TRUE ){
    #02 -2 loadings -ggplot------
    dataplot$Item <- var_name

    gg <-ggplot(dataplot, aes(x=Item, y=std,
                              fill=Latent))+
      geom_bar(stat="identity", position='dodge')+
      geom_hline(yintercept = cut , color= "darkred", linetype = 2)+ #cut: critera >0.7
      geom_hline(yintercept = cut-0.2, color= "gray40")+ #cut: critera >0.7
      ggtitle("Factor loadings")+
      theme_bw()+
      geom_text(aes(label=round(std,2)),
                vjust=-.3, size=val.size)+
      ylim(0, 1.1)+
      theme(axis.text.x = element_text(
        angle=angle,
        size = cex, hjust = hjust,
        face="bold")) #angle, cex

  }else if(rename == FALSE){

    gg <-ggplot(dataplot, aes(x=Item, y=std,
                              fill=Latent))+
      geom_bar(stat="identity", position='dodge')+
      geom_hline(yintercept = cut ,  color= "darkred", linetype=2)+ #cut: critera >0.7
      geom_hline(yintercept = cut-0.2, color= "gray40")+ #cut: critera >0.7
      ggtitle("Factor loadings")+
      theme_bw()+
      geom_text(aes(label=round(std,2)),
                vjust=-.3, size=val.size)+
      ylim(0, 1.1 )+
      theme(axis.text.x = element_text(
        angle= angle,
        size = cex, hjust = hjust,
        face="bold")) #angle, cex
  }else{
    gg <-ggplot(dataplot,aes(x=Item, y=std,
                             fill=Latent))+
      geom_bar(stat="identity", position='dodge')+
      geom_hline(yintercept = cut,  color= "darkred", linetype=2)+ #cut: critera >0.7
      geom_hline(yintercept = cut-0.2, color= "gray40")+ #cut: critera >0.7
      ggtitle("Factor loadings")+
      theme_bw()+
      geom_text(aes(label=round(std,2)),
                vjust=-.3, size=val.size)+
      ylim(0,1.1)+
      theme(axis.text.x = element_text(
        angle=angle,
        size = cex, hjust = hjust,
        face="bold")) #angle, cex
  }


  #Cronbach alpha
  alpha.1 <-  semTools::reliability(x,return.total = F) %>%
    # alpha.1 <- reliability(x) %>%
    t() %>%
    as.data.frame() %>%
    dplyr::select("Cronbach"=alpha,"CR" = omega3) %>%
    mutate(alpha_Check=ifelse(Cronbach>0.7,"Accept(>0.7) *",
                              ifelse(Cronbach>0.6,"Yes(poor) *", "Reject"))) %>%
    mutate(CR_Check=ifelse(CR>0.7,"Accept(>0.7) *","Reject")) %>%
    dplyr::select(Cronbach,alpha_Check,CR,CR_Check )


  #05 Reprort cronbach, AVE, C.R
  FL.1 <- cbind(alpha.1)
  FL <- FL.1%>%knitr::kable(digits=3, format=format,
                    caption="03-1. Internal consistency
          (Cronbach's Alpha, 1951) and Composite Relibility")




  ## 03 CR,AVE-----
  # ?semTools::reliability
  AVE <-  semTools::reliability(x, return.total = F) %>%
    t() %>%
    as.data.frame() %>%
    dplyr::select( "AVE"=avevar)

  sqrt.AVE <- sqrt(AVE)
  colnames(sqrt.AVE)="sqrt.AVE"

  #correlations Matrix
  rho <- lavaan::lavInspect(x,"std")$beta




  ## 04 Convergent validity-----
  alpha_AVE_CR_0 <-   semTools::reliability(x, return.total = FALSE) %>%
    # alpha_AVE_CR <-  reliability(x) %>%
    t() %>%
    as.data.frame() %>%
    dplyr::select("Cronbach"=alpha,
                  "CR" = omega3, "AVE"=avevar) %>%
    mutate(sqrt.AVE=sqrt(AVE))%>%
    mutate(AVE_check=ifelse(AVE>0.5,"Accept(>0.5) *","Reject"))%>%
    dplyr::select(Cronbach,CR, AVE,AVE_check, #sqrt.AVE
    )
  alpha_AVE_CR <-   alpha_AVE_CR_0 %>%
    knitr::kable(digits = 3,format = format,
          caption = "03 Convergent validity
          Internal consistency(Cronbach's Alpha, 1951)(>0.7)
          AVE(>0.5) & CR(>0.7): Fornell & Lacker(1981)")



  #05-1 discriminant validity=====
  betaa <- lavaan::lavInspect(x, "std")$beta

  if(is.null(betaa)){

    psi <-lavaan::lavInspect(x, "std")$psi
    psi[lower.tri(psi)==FALSE]<-0

    rho1<- psi %>% as.data.frame()
    rho1$max<- apply(rho1,1,max)
    # diff<- cbind(rho1$max, sqrt.AVE) # not rwow match(need calculate )
    # diff$delta<-diff[,2]- diff[,1]
    # diff$sig<-ifelse(diff$delta >= 0,"*","ns")
    #
    # FornellNacker <-cbind(psi, max_rho=diff[,1],
    #                       sqrt.AVE,  # row 396
    #                       sig=diff[,4]) %>% as.data.frame()

    #New revise
    rho1 <- rho1 %>% mutate(max = apply(rho1,1, max),
                            lv = rownames(rho1))
    sqrt.AVE$lv <- rownames(sqrt.AVE)

    # diff <- merge(x=rho1, y=sqrt.AVE, by="lv",
    #               all=TRUE, sort = dis.sort)
    diff_0 <- merge(x = rho1[,c("max","lv")],
                    y = sqrt.AVE, by = "lv",
                    all = TRUE,
                    sort = FALSE)
    diff <- merge(x = rho1[,-(ncol(rho1)-1)],
                  y = diff_0, by = "lv",
                  all = TRUE,
                  sort = FALSE)
    # diff$sqrt.AVE[is.na(diff$sqrt.AVE)] <- 0


    # diff <- cbind(rho1$max, sqrt.AVE)
    # diff$delta <- diff$sqrt.AVE - diff$max
    # diff$sig <-ifelse(sqrt.AVE == 0, "-",
    #                   ifelse(diff$delta >= 0, "*", "ns"))

    diff$delta <- diff[,(ncol(diff))]- diff[,(ncol(diff)-1)]
    diff$sig<-ifelse(diff$delta >= 0,"*","ns")


    FornellNacker <- diff[,c(-(ncol(diff)-1))]

    validity <- FornellNacker %>%
      knitr::kable(digits=3, format=format,
            caption="04 Discriminant Validity:
          rho < Square Root of(AVE)
           By Fornell & Lacker(1981)")

  }else{
    lv.cor <- lavaan::lavInspect(x, what="cor.lv")
    lv.cor1 <- lv.cor
    diag(lv.cor1)<-0

    rho1 <- lv.cor1 %>% as.data.frame()
    rho1[lower.tri(rho1)==FALSE]<-0
    rho1$max <- apply(rho1,1, max)

    # bind data
    rho1 <- rho1 %>% mutate(max=apply(rho1,1, max),
                            lv =rownames(rho1))
    sqrt.AVE$lv <- rownames(sqrt.AVE)

    # diff <- merge(x=rho1, y=sqrt.AVE, by="lv",
    #               all=TRUE, sort = dis.sort)
    diff_0 <- merge(x=rho1[,c("max","lv")],
                    y=sqrt.AVE, by="lv",
                    all=TRUE, sort=FALSE)
    diff <- merge(x= rho1[,-(ncol(rho1)-1)],
                  y=diff_0, by="lv",
                  all=TRUE, sort = FALSE)
    # diff$sqrt.AVE[is.na(diff$sqrt.AVE)] <- 0

    # diff <- cbind(rho1$max, sqrt.AVE)
    diff$delta <- diff[,(ncol(diff))]- diff[,(ncol(diff)-1)]
    diff$sig<-ifelse(diff$delta >= 0,"*","ns")



    FornellNacker <- diff[,c(-(ncol(diff)-1))]
    # cbind(rho1, max_rho=diff[,1], sqrt.AVE,
    #                     sig=diff[,4]) %>% as.data.frame()
    #

    validity <- FornellNacker %>%
      knitr::kable(digits=3, format=format,
            caption="04 Discriminant Validity:
          rho < Square Root of(AVE)
           By Fornell & Lacker(1981)")
  }
  # 05-2 discriminant :HTMT #####
  # Assessing Discriminant Validity using Heterotrait–Monotrait Ratio
  # discriminant validity through the heterotrait-monotrait ratio (HTMT) of the correlations (Henseler, Ringlet & Sarstedt, 2015)

  if( is.character(model)==TRUE |
      is.data.frame(dataset)==TRUE){

    options(knitr.kable.NA = '') # hide NA
    # generate dataframe
    htmt0 <- semTools::htmt(model, dataset) %>%
      as.data.frame()

    htmt0[lower.tri(htmt0)==FALSE]<-0 # diag =0
    htmt0NA <- htmt0 # NA remove
    htmt0NA[lower.tri(htmt0)==FALSE]<-NA   # upper to NA
    htmt1 <- htmt0 %>%   #make sig
      mutate(Max = apply(htmt0, 1, max, na.rm=T),  # max
             dis = ifelse(0.9 - Max== 0.9, 0, 0.9 - Max),  #discriminant
             sig = ifelse(0.9- Max >= 0,"*","ns")) #significant
    htmt2 <-cbind(htmt0NA,
                  htmt1[,c(ncol(htmt1)-2, #max
                           ncol(htmt1)-1, #dis
                           ncol(htmt1))] ) #sig


    htmt <- htmt2  %>%
      knitr::kable(format=format, digits = digits,
            caption="The heterotrait-monotrait ratio of correlations (HTMT).
          All correalation < 0.9 --> discriminant Accept(robusrst)
          general accept: < 1
          (Henseler, Ringlet & Sarstedt, 2015)

          ")


  }else{
    htmt <- print("Not calculation HTMT, input syntax is [ model = lavaan model,dataset = data] ")

  }


  ##06 cor significant----
  lv.cor.sig <- lavaan::parameterEstimates(x, standardized = T) %>%
    dplyr::filter(op=="~"|op=="~~"&lhs != rhs) %>%
    dplyr::select(lhs,op,rhs, std.lv, pvalue) %>%
    mutate(sig=ifelse(pvalue < 0.001, "***",
                      ifelse(pvalue < 0.01, "**",
                             ifelse(pvalue < 0.05, "*", "Not Sig")))) %>%
    mutate(op=ifelse(op=="~","<--",
                     ifelse(op=="~~","cor",""))) %>%
    dplyr::select(lhs,op,rhs, std.lv, pvalue,sig) %>%
    knitr::kable(digits=3, format=format,
          caption="05 latent correlation Significant Check")



  ## final result  --------------
  all.reuslt <-list(model= model,
                    fit_criterian=fit,
                    model_fit=fitMeasures_s1,
                    factorloadings=factorloading,
                    Internal_Consistency=FL,
                    Convegent=alpha_AVE_CR,
                    Discriminant=validity,
                    Discriminant_HTMT = htmt,
                    # Latent_Cor=lv.cor,
                    betaMat_sig=lv.cor.sig,
                    loadings_Bar=gg,
                    variable_order= varnames_check
  )
  # all.reuslt
  ## cfa2() output option ---------------
  # switch(res,
  #        all = all.reuslt,
  #        model = model,
  #        modelfit = fit,
  #        modelfit2 = fitMeasures_s1,
  #        loadings = factorloading,
  #        alpha = FL,
  #        CR_AVE = alpha_AVE_CR,
  #        Convegent = alpha_AVE_CR,
  #        fl_criteria = validity,
  #        Discriminant = validity,
  #        htmt = htmt,
  #        HTMT = htmt,
  #        # Latent_Cor=lv.cor,
  #        str_cor = lv.cor.sig,
  #        loadings_Bar = gg )
  switch(res,
         all = all.reuslt,
         model = model,
         modelfit = modelfitdata,
         modelfit2 = fitMeasures_s1,
         loadings = factorloading_0,
         alpha = FL.1,
         CR_AVE = alpha_AVE_CR_0,
         Convegent = alpha_AVE_CR_0,
         fl_criteria = FornellNacker,
         Discriminant = FornellNacker,
         htmt = htmt2,
         HTMT = htmt2,
         # Latent_Cor=lv.cor,
         str_cor = lv.cor.sig,
         loadings_Bar = gg )

}


#cfa2
#
# #quick markdown_table , knitr::kable(format="html) using cfa2()-----------
# markdown_table_s <- function(data,
#                              caption="html to markdown",
#                              full_width=FALSE,
#                              font_size=20,
#                              row.names = NA,
#                              col.names = NA,
#                              centering = TRUE,
#                              digits=3,
#                              show="markdown"
# ){
#
#   library(kableExtra)
#
#   if(show=="kbl"){
#     data %>%as.data.frame() %>%
#       kbl(digits = digits,
#           caption =  caption,
#           row.names=row.names,col.names=col.names,
#           centering=centering
#       ) %>%
#       knitr::kable_classic(full_width=full_width,font_size= font_size)
#   }else if(show=="markdown"){
#     data %>%
#       kable_classic(full_width=full_width,font_size= font_size)
#   }
# }







#cfa3 using data treament =============
cfa3 <- function(x, graph=F){

  library(dplyr)
  library(knitr)
  library(lavaan)
  library(semTools)
  library(tibble)
  library(semPlot)

  # tryCatch({

  #01 fit table
  options(scipen = 100)

  fit.indices=c("chisq","pvalue", "df","rmsea",
                "gfi","agfi","srmr","cfi","tli","nfi","aic","bic")
  fitMeasures <- round(fitMeasures(x,fit.indices),3)
  fitMeasures_s <- round(fitMeasures(x,fit.indices),3)


  # fitMeasures <- as.data.frame(fitMeasures(x,fit.indices))
  fitMeasures <- as.data.frame(fitMeasures)
  fitMeasures$critera <- c("",
                           "*p.value >= 0.05",
                           "_chisq/df <= 3(<5(ok)",
                           "*RMSEA< 0.05(or 0.08)",
                           "*GFI >= 0.95",
                           "_AGFI>= 0.90",
                           "*SRMR < 0.08",
                           "*CFI >= 0.95",
                           "_TLI >= 0.90",
                           "_NFI >= 0.90",
                           "_lower",
                           "_lower")
  fitMeasures$Ref <-c("-",
                      "-",
                      "Wheaton et al.(1977)",
                      "Browne & Cudek(1993)",
                      "Joreskog-Sorbom(1970)",
                      "Tanaka & Huba(1985)",
                      "Hu & Bentler(1999)",
                      "Kline(2011)",
                      "Bentler & Bonett(1980)",
                      "Bollen(1989)",
                      "Akaike(1973)",
                      "-")
  fitMeasures$chiq_df <- c("","",
                           round(fitMeasures[1,1]/fitMeasures[3,1],2),
                           "","","","","","","","","")
  # fitMeasures$fit_chek  <- c("absolute fit","",
  #                            "absolute fit",
  #                            "absolute fit ",
  #                            "absolute fit ",
  #                            "absolute fit ",
  #                            "absolute fit ",
  #                            "incremental fit",
  #                            "incremental fit",
  #                            "incremental fit",
  #                            "parsimonious fit",
  #                            "parsimonious fit")
  fit <- fitMeasures  #%>%
  # knitr::kable(digits=3, format=format,
  #       caption="FitMeasure and criterian
  #       (*)satisfy By kline(2011) Suggestion")
  #


  #modelfit
  fitdata <- fitMeasures(x,c("chisq","df","pvalue",
                             "rmsea",
                             "rmsea.ci.lower",
                             "rmsea.ci.upper",
                             "rmsea.pvalue",
                             "srmr",
                             "gfi",
                             "cfi",
                             "tli",
                             "aic",
                             "bic"
  ))

  criteria_data = c("Chisq",
                    "df",
                    "p >0.05",
                    "RMSEA <0.05",
                    "90%CI.lower",
                    "90%CI.upper",
                    "p >0.05",
                    "SRMR <0.08",
                    "GFI >0.95",
                    "CFI >0.95",
                    "TLI>0.90",
                    "lower ",
                    "lower "
  )

  modelfitdata <-cbind("criterian"=criteria_data,
                       "Value"=round(fitdata,3))

  fitMeasures_s1 <- modelfitdata# %>%
  # knitr::kable (format=format,
  #        caption = "01 Model fit information")
  #



  #04 factor loading
  options(knitr.kable.NA="")



  #
  #
  #     factorloading <- lavaan::parameterEstimates(x, standardized=TRUE) %>%
  #         dplyr::filter(op=="=~") %>%
  #         mutate(stars=ifelse(pvalue < 0.001, "***",
  #                             ifelse(pvalue < 0.01, "**",
  #                                    ifelse(pvalue < 0.05, "*", "")))) %>%
  #         mutate(label=ifelse(std.all>0.7,"Yes(Good)",
  #                             ifelse(std.all>0.5,"Yes(fair)","No"))) %>%
  #         dplyr::select("Latent"=lhs, Item=rhs, Est=est,S.E.=se,
  #                       cr=z, Sig.=stars, "p"=pvalue,
  #                       std=std.all, beta_Accept=label) #%>%
  #     # knitr::kable(digits=3, format=format,
  #     #       caption="02 Indicator Validity(1)-Factor Loadings::
  #     #       (1) cr(critical ratio =Estimate/S.E) p<0.05,
  #     #       (2) std.damda >= 0.5(Bagozzi & Yi(1988)")
  #
  ## 02 factor loading-----
  factorloading <- lavaan::parameterEstimates(x, standardized=TRUE) %>%
    dplyr::filter(op=="=~") %>%
    mutate(stars=ifelse(pvalue < 0.001, "***",
                        ifelse(pvalue < 0.01, "**",
                               ifelse(pvalue < 0.05, "*", "")))) %>%
    mutate(label=ifelse(std.all>0.7,"Yes(Good)",
                        ifelse(std.all>0.5,"Yes(fair)","No"))) %>%
    dplyr::select("Latent"=lhs, Item=rhs, Est=est,S.E.=se,
                  cr=z, Sig.=stars, "p"=pvalue,
                  std=std.all, beta_Accept=label)


  if(graph==T){
    dataplot0 <- lavaan::parameterEstimates(x, standardized=TRUE) %>%
      dplyr::filter(op=="=~") %>%
      mutate(stars=ifelse(pvalue < 0.001, "***",
                          ifelse(pvalue < 0.01, "**",
                                 ifelse(pvalue < 0.05, "*", "")))) %>%
      mutate(label=ifelse(std.all>0.7,"Yes(Good)",
                          ifelse(std.all>0.5,"Yes(fair)","No"))) %>%
      dplyr::select("Latent"=lhs,
                    Item=rhs,
                    Est=est,
                    S.E.=se,
                    cr=z,
                    Sig.=stars,
                    "p"=pvalue,
                    std=std.all,
                    beta_Accept=label)
    dataplot<- dataplot0%>% select(Item,Latent, std)


    gg <-ggplot(dataplot,aes(x=Item, y=std, fill=Latent))+
      geom_bar(stat="identity", position='dodge')+
      geom_hline(yintercept = cut, color= "red")+ #cut:  0.7
      geom_hline(yintercept = cut-0.2, color= "gray40")+ #cut: 0.7
      ggtitle("factor loadings")+
      geom_text(aes(label=round(std,2)),vjust=-.3, size=val.size)+
      theme(axis.text.x = element_text(
        angle=angle,
        size = cex, hjust = hjust,
        face="bold")) #angle, cex
  }




  #Cronbach alpha
  alpha.1 <- semTools::reliability(x,return.total = F) %>%
    t() %>%
    as.data.frame() %>%
    dplyr::select( "Cronbach"=alpha,"CR" = omega3) %>%
    mutate(alpha_Check=ifelse(`Cronbach`>0.7,"Accept(>0.7) *",
                              ifelse(`Cronbach's alpha`>0.6,"Yes(poor) *", "Reject"))) %>%
    mutate(CR_Check=ifelse(CR>0.7,"Accept(>0.7) *","Reject")) %>%
    dplyr::select(Cronbach,alpha_Check,CR,CR_Check )


  #05 Reprort cronbach, AVE, C.R
  FL.1 <- cbind(alpha.1)
  FL <-FL.1 #%>% kable(digits=3, format=format,
  #  caption="03-1. Internal consistency
  # (Cronbach's Alpha, 1951) and Composite Validity")




  #CR,AVE

  AVE <- semTools::reliability(x,return.total = F) %>%
    t() %>%
    as.data.frame() %>%
    dplyr::select( "AVE"=avevar)

  sqrt.AVE <- sqrt(AVE)
  colnames(sqrt.AVE)="sqrt.AVE"

  #correlations Matrix
  rho <- lavaan::lavInspect(x,"std")$beta




  # Convergent validity
  alpha_AVE_CR <-  semTools::reliability(x,return.total = F) %>%
    t() %>%
    as.data.frame() %>%
    dplyr::select("Cronbach"=alpha, "CR" = omega3, "AVE"=avevar) %>%
    mutate(sqrt.AVE=sqrt(AVE))%>%
    mutate(AVE_check=ifelse(AVE>0.5,"Accept(>0.5) *","Reject"))%>%
    dplyr::select(Cronbach,CR, AVE,AVE_check, #sqrt.AVE
    ) #%>%
  # knitr::kable(digits = 3,format = format,
  #       caption = "03 Convergent validity
  #       Internal consistency(Cronbach's Alpha, 1951)(>0.7)
  #       AVE(>0.5) & CR(>0.7): Fornell & Lacker(1981)")



  #06 discriminant validity
  betaa <-lavaan::lavInspect(x, "std")$beta

  if(is.null(betaa)){

    psi <-lavaan::lavInspect(x, "std")$psi
    psi[lower.tri(psi)==FALSE]<-0

    rho1<- psi %>% as.data.frame()
    rho1$max<- apply(rho1,1,max)
    diff<- cbind(rho1$max,sqrt.AVE)
    diff$delta<-diff[,2]- diff[,1]
    diff$sig<-ifelse(diff$delta>0,"*","Not Sig")

    FornellNacker <-cbind.data.frame(psi, max_rho=diff[,1],sqrt.AVE,
                                     sig=diff[,4]) %>% as.data.frame()


    validity <- FornellNacker #%>%
    # knitr::kable(digits=3, format=format,
    #       caption="04 Discriminant Validity:
    #     rho < Square Root of(AVE)
    #      By Fornell & Lacker(1981)")

  }else{
    lv.cor <- lavaan::lavInspect(x, what="cor.lv")
    lv.cor1 <- lv.cor
    diag(lv.cor1)<-0

    rho1 <- lv.cor1 %>% as.data.frame()
    rho1[lower.tri(rho1)==FALSE]<-0
    rho1$max <- apply(rho1,1, max)

    #
    rho1 <- rho1 %>% mutate(max=apply(rho1,1, max), lv =rownames(rho1))
    sqrt.AVE$lv <- rownames(sqrt.AVE)

    diff <- merge(x=rho1, y=sqrt.AVE, by="lv", all=TRUE)

    diff$sqrt.AVE[is.na(diff$sqrt.AVE)] <- 0

    # diff <- cbind(rho1$max, sqrt.AVE)
    diff$delta <- diff$sqrt.AVE- diff$max
    diff$sig <-ifelse(diff$delta>0,"*","ns")


    FornellNacker <- diff[,c(-(ncol(diff)-1))]
    # cbind(rho1, max_rho=diff[,1], sqrt.AVE,
    #                     sig=diff[,4]) %>% as.data.frame()
    #

    validity <- FornellNacker# %>%
    #   knitr::kable(digits=3, format=format,
    #         caption="04 Discriminant Validity:
    # rho < Square Root of(AVE)
    #  By Fornell & Lacker(1981)")

  }



  # cor significant
  lv.cor.sig <- lavaan::parameterEstimates(x, standardized = T) %>%
    dplyr::filter(op=="~"|op=="~~"&lhs != rhs) %>%
    dplyr::select(lhs,op,rhs, std.lv, pvalue) %>%
    mutate(sig=ifelse(pvalue < 0.001, "***",
                      ifelse(pvalue < 0.01, "**",
                             ifelse(pvalue < 0.05, "*", "Not Sig")))) %>%
    mutate(op=ifelse(op=="~","<--",
                     ifelse(op=="~~","cor",""))) %>%
    dplyr::select(lhs,op,rhs, std.lv, pvalue,sig)# %>%
  # knitr::kable(digits=3, format=format,
  #       caption="05 latent correlation Significant Check")
  #



  all.reuslt <-list(
    # fit_criterian=fit,
    model_fit=fitMeasures_s1,
    factorloadings=factorloading,
    Internal_Consistency=FL,
    Convegent=alpha_AVE_CR,
    Discriminant=validity,
    # Latent_Cor=lv.cor,
    betaMat_sig=lv.cor.sig
  )
  all.reuslt
}
