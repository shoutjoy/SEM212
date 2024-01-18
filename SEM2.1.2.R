#Verseion 2023.7.10 factorloading_grp update
#Verseion 2024.1.17 factorloading_grp update
cat("Statitcal Tools called SEM2.1.2.R 
    Park Joonghee Ph.D (yonsei cognitive engineering)
    Verseion 2024.1.17
    Updated contents:  pls, trans, plspm creat_path() ")
 
#패키지 로딩 #####
# library(dplyr)
library(tidyverse)
# library(magrittr)
# library(PerformanceAnalytics) #chart.Correlation
# library(psych)
# library(GPArotation)
# describe()<- psych::describe()
library(lavaan)
library(semPlot)
library(semTools)
library(broom)
# library(simsem)

# library(readr)
library(readxl)
# library(googlesheets4)
# library(openxlsx)
library(knitr)

library(semptools)
# library(tidyLPA)
# library(mclust)
# https://gargle.r-lib.org/articles/non-interactive-auth.html
# 인증코드
# 4/1AY0e-g40snrmGcBA4-pFSm7EPkRX9XIUpmkUQQYiTIlNx9YRB8S5A-ESpGQ
options(gargle_oob_default = TRUE)
options(dplyr.summarise.inform = FALSE)
options(knitr.kable.NA = '')
#기술통계

#skew계산 함수 
SKEW <- function(x){
  
  m3 <- mean((x - mean(x))^3)
  skewness <- m3/(sd(x)^3)
  skewness
}

#kurtosis 계산 함수 
KURT <- function(x){
  
  m4 <- mean((x - mean(x))^4) 
  kurtosis <- m4/(sd(x)^4) - 3  
  kurtosis
}

# onl[,18:32] %>%  psych::describe()
# 
# mds_onl_0$mds2 %>% SKEW()
# mds_onl_0$mds2 %>% KURT()

#기술통계

#기술통계
Summarise <- function(x,
                      type="normalitytest",
                      digit=2){
  library(stringr)
  library(tidyverse)
  library(MVN)
  tryCatch({
    #col:세로로 나타내기 (변수가 세로)
    #row:가로로 나타내기 (변수가 가로)
    # if(!is.numeric(x)) return("numeric변수가 아닙니다. 확인해서 정확한 범위를 입력하세요")
    # if(!is.numeric(x)) {
    #
    #   for(i in 1:ncol(x)){
    #     x[,i] <-  x[,i] %>% as.numeric()
    #   }
    # }
    #
    x <- x %>% as.data.frame()
    s_col <- summarise(x,
                       across( .cols=c(1:ncol(x)),
                               .fns = function(x){
                                 str_c(round(mean(x, na.rm = T),
                                digit),"(", round(sd(x),digit),")" )}
                       )) %>% t() %>% as.data.frame()
    colnames(s_col)="Mean(sd)"

    s_row<- s_col %>% t() %>% data.frame()


    #sample size
    s_n <- summarise(x, across(
      .cols=c(1:ncol(x)),
      .fns = function(x){length(x)}
      )) %>% t()
        colnames(s_n)="n"
    #mean
    s_mean <-summarise(x, across(
      .cols=c(1:ncol(x)),
      .fns = function(x){mean(x, na.rm = T)}
        )) %>% t() #%>% as.data.frame()
        colnames(s_mean)="mean"

    #Standard Deviation
    s_sd <-summarise(x, across(
      .cols=c(1:ncol(x)),
      .fns = function(x){sd(x, na.rm = T)}
       )) %>% t()# %>% as.data.frame()
       colnames(s_sd)="sd"

    #skewness
    s_skew <-summarise(x, across(
      .cols=c(1:ncol(x)),
      # .fns = function(x){SKEW(x)}
      .fns = function(x){ mean((x - mean(x))^3)/(sd(x)^3)}
       )) %>% t() #%>% as.data.frame()
       colnames(s_skew)="skew"
    #kurtosis
    s_kurt <-summarise(x, across(
      .cols=c(1:ncol(x)),
      # .fns = function(x){KURT(x)}
      .fns = function(x){mean((x - mean(x))^4) /(sd(x)^4) - 3}
       )) %>% t() #%>% as.data.frame()
          colnames(s_kurt)="kurtosis"

    #median
    s_median <-summarise(x, across(
      .cols=c(1:ncol(x)),
      .fns = function(x){median(x)}
       )) %>% t() #%>% as.data.frame()
        colnames(s_median)="median"

    #colname addon
       colName= colnames(x)

    # description = bind_cols(variable= colName,
    #                         round(s_mean, digit),
    #                         round(s_sd, digit),
    #                         s_n,
    #                         round(s_skew, digit),
    #                               round(s_kurt, digit) ) %>% tibble()
    description = bind_cols(s_mean,
                            s_sd,
                            s_n,
                            s_skew,
                            s_kurt )
    #

    #정규성 검정 자료
    output_data0 <- round(cbind.data.frame(s_mean,
                                           s_sd,
                                           s_n,
                                           s_skew,
                                           s_kurt ) , digit)


    output_data <- description
    N <- output_data$n

    output_data$skew.z <- output_data$skew/sqrt((6*N*((N-1))/((N-2)*(N+1)*(N+3))))
    output_data$kurt.z <- output_data$kurtosis/sqrt((24*N*(N-1)*(N-1))/((N-3)*(N-2)*(N+3)*(N-5)))
    # output_data
    # output_data <- as.data.frame(output_data)

    output_df <- output_data[,
                             c("n","mean","sd","skew",
                               "kurtosis", "skew.z","kurt.z")] %>%
      tibble()

    output <- output_df
    #data nomality check making .
    output[,"skew_TF"]<- "Not"
    output[output$skew.z < 3,"skew_TF"]<- "fair"
    output[output$skew.z < 1.96,"skew_TF"]<- "Good"
    output[output$skew.z < -1.96,"skew_TF"]<- "fair"
    output[output$skew.z < -3,"skew_TF"]<- "Not"

    output[,"kurt_TF"]<- "Not"
    output[output$kurt.z < 3,"kurt_TF"]<- "fair"
    output[output$kurt.z < 1.96,"kurt_TF"]<- "Good"
    output[output$kurt.z < -1.96,"kurt_TF"]<- "fair"
    output[output$kurt.z < -3,"kurt_TF"]<- "Not Sig"

    output_res= bind_cols(variabel= colName,
                          round(output_df, digit),
                          output[,c(8:9)]
    )


    ref = c("Reference:
    (1) Kline(2011):skew<3, kurt<10,
    (2) Crran,West & Finch(1997): skew<2, kurt<7
    (3) Normality Test
       |skew.Z|<1.96,|Krut.z|<1.96 -> normality is satisfied
     H0: 정규성을 충족한다,
     H1: 정규성을 충족하지 않는다 \n \n "
    )
    
    #### 정규성 검정 Henze-Zirkler, Anderson-Darling -----
    mvn_data = mvn(x, mvnTest = "mardia")  ## 11plot auto 
    shapirotest = mvn(x, univariateTest="SW")
    mvn_hz = mvn(x, mvnTest = "hz")
    
    
    

    switch(type,
           col = s_col,
           row = s_row,
           skew = round(s_skew, digit),
           kurt = round(s_kurt, digit),
           mean = round(s_mean,digit),
           sd = round(s_sd, digit),
           n = s_n,
           median = s_median,
           all = bind_cols(variable= colName, output_data0),
           all_tibble = bind_cols(variable= colName, 
                                  output_data0) %>% tibble(),
           normalitytest = list(cat(ref), output_res),
           normalitytest_data = output_res,
           df = output_res,
           mvn=mvn_hz,
           shapiro = shapirotest,
           ref= cat(ref)
           

    )
  },error=function(e)return("오류:Numeric변수가 아닌 것이 포함되었습니다. 데이터를 다시 점검하세요 missCheck()를 실행해서 NA데이터가 있는지 확인하세요. 그리고,  함수 SKEW(), KURT를 실행하세요"))

}

# ?MVN::mvn

#기술통계---------------------
Summarise1 <- function(x, 
                      type="all",
                      digit=2,
                      mvntest="mardia"
){
  library(stringr)
  library(tidyverse)
  library(MVN)
  tryCatch({
    #col:세로로 나타내기 (변수가 세로)
    #row:가로로 나타내기 (변수가 가로)
    x <- x%>% as.data.frame()
    
    s_col <- summarise(x, 
              across( .cols=c(1:ncol(x)),
               .fns = function(x){
                  str_c(round(mean(x, na.rm = T),
                digit),"(", round(sd(x, na.rm = T), digit),")" )})) %>%
      t() %>% 
      as.data.frame()
    colnames(s_col)="Mean(sd)"
    s_row<- s_col %>% t() %>% data.frame()
    
    
    #sample size
    s_n <- summarise(x, across(
      .cols=c(1:ncol(x)),
      .fns = function(x){length(x)}
    )) %>% t() 
    colnames(s_n)="n"
    #mean 
    s_mean <-summarise(x, across(
      .cols=c(1:ncol(x)),
      .fns = function(x){mean(x, na.rm = T)}
    )) %>% t() #%>% as.data.frame()
    colnames(s_mean)="mean"
    
    #Standard Deviation
    s_sd <-summarise(x, across(
      .cols=c(1:ncol(x)),
      .fns = function(x){sd(x, na.rm = T)}
    )) %>% t()# %>% as.data.frame()
    colnames(s_sd)="sd"
    
    #skewness
    s_skew <- summarise(x, across(
      .cols=c(1:ncol(x)),
      # .fns = function(x){SKEW(x)}
      .fns = function(x){ mean((x - mean(x))^3)/(sd(x)^3)}
    )) %>% t() #%>% as.data.frame()
    colnames(s_skew)="skew"
    
    single_skew<- summarise(x,
                            # .fns = function(x){SKEW(x)}
               .fns = function(x){ mean((x - mean(x))^3)/(sd(x)^3)})
    
    
    #kurtosis
    s_kurt <-summarise(x, across(
      .cols=c(1:ncol(x)),
      # .fns = function(x){KURT(x)}
      .fns = function(x){mean((x - mean(x))^4) /(sd(x)^4) - 3}
    )) %>% t() #%>% as.data.frame()
    colnames(s_kurt)="kurtosis"
    
    #median
    s_median <-summarise(x, across(
      .cols=c(1:ncol(x)),
      .fns = function(x){median(x)}
    )) %>% t() #%>% as.data.frame()
    colnames(s_median)="median"
    
    #colname addon 
    colName= colnames(x)
    
    # description = bind_cols(variable= colName, 
    #                         round(s_mean, digit), 
    #                         round(s_sd, digit), 
    #                         s_n, 
    #                         round(s_skew, digit), 
    #                               round(s_kurt, digit) ) %>% tibble()
    description = bind_cols(s_mean, 
                            s_sd, 
                            s_n, 
                            s_skew, 
                            s_kurt ) 
    # 
    
    #정규성 검정 자료
    output_data0 <- round(cbind.data.frame(s_mean, 
                                           s_sd, 
                                           s_n, 
                                           s_skew, 
                                           s_kurt ) , digit) 
    
    
    output_data <- description
    N <- output_data$n
    
    output_data$skew.z <- output_data$skew/sqrt((6*N*((N-1))/((N-2)*(N+1)*(N+3))))
    output_data$kurt.z <- output_data$kurtosis/sqrt((24*N*(N-1)*(N-1))/((N-3)*(N-2)*(N+3)*(N-5)))
    # output_data
    # output_data <- as.data.frame(output_data)
    
    output_df <- output_data[,
                             c("n","mean","sd","skew",
                               "kurtosis", "skew.z","kurt.z")] %>% 
      tibble()
    
    output <- output_df
    #data nomality check making .
    output[,"skew_TF"]<- "Not"
    output[output$skew.z < 3,"skew_TF"]<- "fair"
    output[output$skew.z < 1.96,"skew_TF"]<- "Good"
    output[output$skew.z < -1.96,"skew_TF"]<- "fair"
    output[output$skew.z < -3,"skew_TF"]<- "Not"
    
    output[,"kurt_TF"]<- "Not"
    output[output$kurt.z < 3,"kurt_TF"]<- "fair"
    output[output$kurt.z < 1.96,"kurt_TF"]<- "Good"
    output[output$kurt.z < -1.96,"kurt_TF"]<- "fair"
    output[output$kurt.z < -3,"kurt_TF"]<- "Not Sig"
    
    output_res= bind_cols(variabel= colName,
                          round(output_df, digit),
                          output[,c(8:9)]   )
    
    #### 정규성 검정 Henze-Zirkler, Anderson-Darling -----
    mvn_data = mvn(x, mvnTest = mvntest)  ## 11plot auto 
    shapirotest = mvn(x, univariateTest="SW")
    mvn_hz = mvn(x, mvnTest = "hz")
    
    # mvn_qq = mvn(x, mvnTest = mvntest, multivariatePlot = "qq")
    # univariatePlot = "histogram"
    # multivariatePlot = "qq"
    # univariateTest	
    # select one of the univariate normality tests, Shapiro-Wilk ("SW"), Cramer-von Mises ("CVM"), Lilliefors ("Lillie"), Shapiro-Francia ("SF"), Anderson-Darling ("AD"). Default is Anderson-Darling ("AD"). Do not apply Shapiro-Wilk's test, if dataset includes more than 5000 cases or less than 3 cases.
    # # If mvnTest = "mardia", it calculates the Mardia's multivariate skewness and kurtosis coefficients as well as their corresponding statistical significance. It can also calculate corrected version of skewness coefficient for small sample size (n< 20). For multivariate normality, both p-values of skewness and kurtosis statistics should be greater than 0.05. If sample size less than 20 then p.value.small should be used as significance value of skewness instead of p.value.skew. If there are missing values in the data, a listwise deletion will be applied and a complete-case analysis will be performed.
    # 
    # If mvnTest = "hz", it calculates the Henze-Zirkler's multivariate normality test. The Henze-Zirkler test is based on a non-negative functional distance that measures the distance between two distribution functions. If the data is multivariate normal, the test statistic HZ is approximately lognormally distributed. It proceeds to calculate the mean, variance and smoothness parameter. Then, mean and variance are lognormalized and the p-value is estimated. If there are missing values in the data, a listwise deletion will be applied and a complete-case analysis will be performed.
    # 
    # If mvnTest = "royston", it calculates the Royston's multivariate normality test. A function to generate the Shapiro-Wilk's W statistic needed to feed the Royston's H test for multivariate normality However, if kurtosis of the data greater than 3 then Shapiro-Francia test is used for leptokurtic samples else Shapiro-Wilk test is used for platykurtic samples. If there are missing values in the data, a listwise deletion will be applied and a complete-case analysis will be performed. Do not apply Royston's test, if dataset includes more than 5000 cases or less than 3 cases, since it depends on Shapiro-Wilk's test.
    # 
    # If mvnTest = "dh", it calculates the Doornik-Hansen's multivariate normality test. The code is adapted from asbio package (Aho, 2017).
    # 
    # If mvnTest = "energy", it calculates the Energy multivariate normality test. The code is adapted from energy package (Rizzo and Szekely, 2017).
    
    
    ref = c("Reference:
    (1) Kline(2011):skew<3, kurt<10,
    (2) Crran,West & Finch(1997): skew<2, kurt<7
    (3) Normality Test
       |skew.Z|<1.96,|Krut.z|<1.96 -> normality is satisfied 
     H0: 정규성을 충족한다,
     H1: 정규성을 충족하지 않는다 \n \n "
    )
    ###결과스위치---------
    switch(type,
           col= s_col,
           row=s_row,
           skew= round(s_skew, digit) ,
           skew_1 = single_skew,
           kurt= round(s_kurt, digit),
           mean= round(s_mean,digit),
           sd= round(s_sd, digit),
           n= s_n,
           median= s_median,
           all = bind_cols(variable= colName, 
                           output_data0),
           all_tibble = bind_cols(variable= colName, 
                                  output_data0) %>% tibble(),
           normalitytest = list(cat(ref), output_res),
           normalitytest_data = output_res %>% print(n=Inf),
           
           mvn = mvn_data,
           mvn_hz= mvn_hz,
           mvn_hist = mvn_hist,
           # mvn_qq = mvn_qq,
           shapiro = shapirotest
           
           
    )
  },error=function(e)return("오류:Numeric변수가 아닌 것이 포함되었습니다. missCheck를 통해서 NA데이터가 있는지 확인하세요 "))
  
}


# 
# mds_onl_0 %>% Summarise("col", 2)
# mds_onl_0 %>% Summarise("mean",4)
# mds_onl_0 %>% Summarise("sd",3)
# mds_onl_0 %>% Summarise("skew",4)
# mds_onl_0 %>% Summarise("kurt",4)
# 
# mds_onl_0 %>% Summarise("all")
# mds_onl_0 %>% Summarise("all_tibble")
# mds_onl_0 %>% Summarise("all", 3)
# mds_onl_0 %>% Summarise("all", 4)
# mds_onl_0 %>% Summarise("normalitytest")
# mds_onl_0 %>% Summarise("normalitytest", 3)
# mds_onl_0 %>% Summarise("normalitytest_data")

# Summarise <- function(x, type="col",digit=2){
#  library(stringr)
#   tryCatch({
#   #col:세로로 나타내기 (변수가 세로)
#   #row:가로로 나타내기 (변수가 가로)
#   # if(!is.numeric(x)) return("numeric변수가 아닙니다. 확인해서 정확한 범위를 입력하세요")
#   s_col <-summarise(x, across(
#                 .cols=c(1:ncol(x)),
#                 .fns = function(x){str_c(round(mean(x, na.ram=T),digit),"(", round(sd(x),digit),")" )}
#                      )) %>% t() %>% as.data.frame()
#   colnames(s_col)="Mean(sd)"
#   s_row<- s_col %>% t() %>% data.frame()
# 
# 
#   switch(type,
#         col=s_col,
#         row=s_row)
#   },error=function(e)return("오류:Numeric변수가 아닌 것이 포함되었습니다."))
# 
#   }

#역코딩 ------
inverseCoding <- function(data, n=6){
  data <- n - data
  data
}



#신뢰도를 표로 작성하는 함수 ----------------------
alpha_table = function(alpha_data_res,
                       title="",
                       show="markdown",
                       format="markdown",
                       digits=3,
                       variable="."
){
  library(psych)
  library(tidyverse)
  
  out <-  tryCatch(
    {  if(show =="alpha"){
      #croncbah를 계산하여 넣은 경우 
      
      alpha = paste0("alpha = ",
                     round(alpha_data_res$total[1],3),
                     ", 95%CI[",round(alpha_data_res$feldt[[1]],2),", ",
                     round(alpha_data_res$feldt[[3]],2), "]")
      
      res <- cbind.data.frame(
        Var = paste0(variable , c(1:length(data_res$keys[[1]]))),
        "alpha_95%CI" = alpha,
        subfactor = alpha_data_res$keys[[1]],
        "cronbach alpha" = alpha_data_res$alpha.drop[,1]) %>%  
        kable("markdown", digits, 
              caption = gtl(paste0(title,"에 대한 신뢰도(Cronbach's alpha)")))
      # caption = paste0(title,"에 대한 신뢰도(Cronbach's alpha)"))
      res
      
      
    }else if(show == "markdown"){
      #데이터를 이용하여 직접 계산하는 경우 
      data_res <- alpha_data_res %>%psych::alpha(check.keys = TRUE)
      
      alpha = paste0("alpha = ",
                     round(data_res$total[1],3),
                     ", 95%CI[",round(data_res$feldt[[1]],2),", ",
                     round(data_res$feldt[[3]],2), "]")
      
      res <- cbind.data.frame(
        Var = paste0(variable , c(1:length(data_res$keys[[1]]))),
        "alpha_95%CI" = alpha,
        subfactor = data_res$keys[[1]],
        "cronbach alpha"= data_res$alpha.drop[,1])%>% 
        kable(format = format, digits, 
              caption = gtl(paste0(title,"에 대한 Reliability(Cronbach's alpha)")))
      # caption = paste0(title,"에 대한 Reliability(Cronbach's alpha)"))
      res
      
    }else if(show == "data"){
      #여러 데이터를 결합하여 다른 표를 만드는 경우 필요 
      data_res <- alpha_data_res %>%psych::alpha(check.keys = TRUE)
      
      alpha = paste0("alpha = ",
                     round(data_res$total[1],3),
                     ", 95%CI[",round(data_res$feldt[[1]],2),", ",
                     round(data_res$feldt[[3]],2), "]")
      
      res <- cbind.data.frame(
        Var = paste0(variable , c(1:length(data_res$keys[[1]]))),
        "alpha_95%CI" = alpha,
        subfactor = data_res$keys[[1]],
        "cronbach alpha" = data_res$alpha.drop[,1]) 
      res
    }
    },error = function(e){
      message("
      옵션설정을 다시 확인하세요. show에서 alpha 혹은 data로 입력하세요. 
alpha를 실행한 값이면show = 'alpha'를 입력하시고, 
데이터만 가지고 분석하여 표를 만드는 경우는 show = 'data'로 설정하세요")
    } 
    
  )
  return(out)
}


#star
star_make <- function(data){
  ndata <- data %>% 
    as.data.frame() %>%   
    mutate(sig = ifelse(p.value < 0.001, "***", 
                        ifelse(p.value < 0.01, "**", 
                               ifelse(p.value < 0.05, "*", 
                                      "ns")))) 
  ndata  
}

p.value_add <-function(data){
  ndata <- data %>% 
    as.data.frame() %>%
    mutate(sig = ifelse(p.value < 0.001, "***", 
                        ifelse(p.value < 0.01, "**", 
                               ifelse(p.value < 0.05, "*", 
                                      "ns")))) 
  ndata 
}

p_add <-function(data, p){
  ndata <- data %>% 
    as.data.frame() %>%
    mutate(sig = ifelse(p < 0.001, "***", 
                 ifelse(p < 0.01, "**", 
                  ifelse(p < 0.05, "*", 
                              "ns")))) 
  ndata 
}


pvalue_add <-function(data){
  ndata <- data %>% 
    as.data.frame() %>%
    mutate(sig = ifelse(pvalue < 0.001, "***", 
                        ifelse(pvalue < 0.01, "**", 
                               ifelse(pvalue < 0.05, "*", 
                                      "ns")))) 
  ndata 
}

#mutate data generate
p_sig <- function(p.value){
  ifelse(p.value < 0.001, "***", 
         ifelse(p.value < 0.01, "**", 
                ifelse(p.value < 0.05, "*", 
                       "ns")))
}

# Using vector data
p_star <- function(p.value="p.value"){
  ifelse(p.value < 0.001, "***", 
         ifelse(p.value < 0.01, "**", 
                ifelse(p.value < 0.05, "*", 
                       "ns")))
}
# 2024.1.6. 추가 
#p값을 설정하여 추가 
# p_mark_sig <-function(data, col= p.value){
p_mark_sig <-function(data, col="p.value"){
  #변수 선택 
  library(tidyverse)
  p.value <- data %>% as.data.frame() %>%
    dplyr::select(all_of(col))
  # any_of()는 누락된 변수를 확인하지 않습니다. 변수가 제거되었는지 확인하려는 경우 음수 선택에 특히 유용합니다.
  # all_of()는 엄격한 선택을 위한 것입니다. 문자형 벡터의 변수 중 누락된 것이 있으면 오류가 발생합니다.
  # all_of : correct, any_of: some 
 ndata <- data %>% 
         as.data.frame() %>%
    mutate(sig = ifelse(p.value < 0.001, "***", 
                    ifelse(p.value < 0.01, "**", 
                       ifelse(p.value < 0.05, "*", 
                                             "ns")))) 
  #변수를 CHARACTER로 변경 
  ndata$sig <- as.character(ndata$sig) 
  ndata %>% tibble()
 
}

# ToothGrowth %>%
#   group_by(dose) %>%
#   t_test(data =., len ~ supp) %>% 
#   p_mark_sig("p")
# 
# ToothGrowth %>%
#   group_by(dose) %>%
#   t_test(data =., len ~ supp) %>% 
#   p_mark_sig()




# 데이터의 마지막 열을 p value
sig_add_last_column <- function(data){
  
  data <- data %>% 
         as.data.frame() 
 # data[, ncol(data)]
  
  
  ndata <- data%>%
           mutate(sig = ifelse( data[, ncol(data)] < 0.001, "***", 
                        ifelse( data[, ncol(data)] < 0.01, "**", 
                               ifelse( data[, ncol(data)] < 0.05, "*", 
                                      "ns")))) 
  ndata 
}





#신뢰구간 유의성 검정: 신뢰구간이 마지막 열에 있는 경우 
# 데이터의 마지막 2개의 열이 신뢰구간인 경우 유의성검정 
confint_mark_sig <- function(data){
  
  data <- data %>% data.frame()
  ndata <- data%>%
    mutate(sig = ifelse( data[, ncol(data)-1]*data[, ncol(data)] > 0, "*","ns")) 
  
  #변수를 CHARACTER로 변경 
  ndata$sig <- as.character(ndata$sig) 
  ndata %>% tibble()
  
}


# Con %>% confint() %>% confint_mark_sig()




##View 테이블 만들기 ----------
markdown_table <- function(lm_data,
                           caption = "<Table  > capiton title",
                           digits = 2,
                           font_size= 18,
                           full_width= F,
                           table ="classic",
                           show="data",
                           format="html",
                           row.names = NA,
                           col.names = NA,
                           align=NULL,
                           label = NULL,
                           format.args = list(),
                           escape = TRUE,
                           table.attr = "",
                           booktabs = FALSE,
                           longtable = FALSE,
                           valign = "t",
                           position = "",
                           centering = TRUE
                           ){
  library(tidyverse)
  library(kableExtra)
  library(broom)
  #논문에 넣을때 복사하여 넣을 것 
  cat(" *** : p < .001, ** : p < .01, * : p < .05")
  
  if(show =="lm"){
    #논문 테이블 Viewer 
    lm_data %>%
      tidy() %>% # tibble data 
      mutate(sig = ifelse(p.value < 0.001, "***", 
                          ifelse(p.value < 0.01, "**", 
                                 ifelse(p.value < 0.05, "*", 
                                        "")))) %>% 
      kbl(digits = digits, 
          caption =  caption) %>%
      kable_classic(full_width=full_width, font_size= font_size)
  }else if(show =="p_add"){
    lm_data %>%as.data.frame() %>% 
      mutate(sig = ifelse(p.value < 0.001, "***", 
                          ifelse(p.value < 0.01, "**", 
                                 ifelse(p.value < 0.05, "*", 
                                        "")))) %>% 
      kbl(digits = digits, 
          caption =  caption) %>%
      kable_classic(full_width=full_width,font_size= font_size)
  }else if(show =="data"){
    lm_data<- lm_data %>% as.data.frame() %>% 
      kbl(digits = digits, 
          format= format,
          # caption =  caption,
          row.names = row.names,
          col.names = col.names,
          align = align,
          caption = caption,
          label = label,
          format.args = list(),
          escape = escape,
          table.attr = table.attr,
          booktabs = booktabs,
          longtable = FALSE,
          valign = valign,
          position = position,
          centering = TRUE
          )
     
    if(table=="paper"){
    lm_data %>%  kable_paper(full_width = full_width,
                    font_size =  font_size)
      }else if(table =="classic"){
    lm_data %>% kable_classic(full_width = full_width,
                    font_size =  font_size)
      }else if(table =="dark"){
    lm_data %>% kable_material_dark(full_width = full_width,
                    font_size =  font_size)
      }else if(table =="minimal"){
        lm_data %>% kable_minimal(full_width = full_width,
                                        font_size =  font_size)
      }
  
  }
}
# ?kable_classic
#data에 대한 것을 Viewer로 출력 
# ?kbl



#SEM process
SEM <- function(x, type="cfa"){

  library(dplyr)
  library(knitr)
  library(lavaan)
  library(semTools)
  library(tibble)
  library(semPlot)

  tryCatch({
  switch(type,
         cfa=cfa2(x),
         CR=CR(x),
         AVE=AVE(x),
         modindices=Modindices(x),
         semPaths=SemPaths(x),
         fit=CompareFit(x),
         loadings=factor_loadings(x),
         convergent=Convergent_Validity(x),
         discriminant=Discriminant_Validity(x),
         effect=effect1(x),
         med=med_effect(x),
         label=effect2(x),
         regress=effect3(x),
         define =effect4(x),
         group=effect5(x)
         )
  },error=function(e) return("입력 오류입니다.cfa,CR,AVE,semPaths, fit, loadings,convergent,  discriminant,  effect, med,  label, regress, define,  group ")
)
}




#cfa2 CFA분석함수----
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
    kable(digits=3, format=format,
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
    
    modelfitdata <-cbind.data.frame("criterian"=criteria_data_00,
                                    "Value"=round(fitdata_00,3))
    
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
  
  fitMeasures_s1 <- modelfitdata %>%
    kable (format=format,
           caption = "01 Model fit information")
  
  
  
  
  ## 02 factor loading-----
  options(knitr.kable.NA="")
  
  factorloading <- parameterEstimates(x, standardized=TRUE) %>%
    filter(op=="=~") %>%
    mutate(stars=ifelse(pvalue < 0.001, "***",
                        ifelse(pvalue < 0.01, "**",
                               ifelse(pvalue < 0.05, "*", "")))) %>%
    mutate(label=ifelse(std.all>0.7,"Yes(Good)",
                        ifelse(std.all>0.5,"Yes(fair)","No"))) %>%
    dplyr::select("Latent"=lhs, Item=rhs, Est=est,S.E.=se,
                  cr=z, Sig.=stars, "p"=pvalue,
                  std=std.all, Accept=label) 
  
  
  
  ### factpr loadings 새로운 변수가 들어왔을 때 ------
  if(rename == TRUE){
    factorloading <- factorloading %>% mutate(Indicator= var_name)%>%
      dplyr::select(Latent,Item ,Indicator , Est, S.E., cr, Sig., p ,
                    std, Accept 
      ) %>% 
      kable(digits=3, format=format,
            caption="02 Indicator Validity(1)
          Factor Loadings:
          (1) cr(critical ratio = Estimate/S.E) p<0.05,
          (2) std.damda >= 0.5(Bagozzi & Yi(1988)") 
    
  }else{
    factorloading <-factorloading %>%
      kable(digits=3, format=format,
            caption="02 Indicator Validity(1)
          Factor Loadings:
          (1) cr(critical ratio = Estimate/S.E) p<0.05,
          (2) std.damda >= 0.5(Bagozzi & Yi(1988)")
  }
  
  
  
  
  
  
  dataplot0 <-  parameterEstimates(x, standardized=TRUE) %>%
    filter(op=="=~") %>%
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
  # 
  # gg <-ggplot(dataplot,aes(x=Item, y=std, fill=Latent))+
  #   geom_bar(stat="identity", position='dodge')+
  #   geom_hline(yintercept = cut, color= "red")+ #cut: 기준 0.7
  #   geom_hline(yintercept = cut-0.2, color= "gray40")+ #cut: 기준 0.7
  #   ggtitle("factor loadings")+
  #   geom_text(aes(label=round(std,2)),vjust=-.3, size=val.size)+
  #   theme(axis.text.x = element_text(
  #     angle=angle,
  #     size = cex, hjust = hjust,
  #     face="bold")) #angle, cex
  # dataplot3 <- dataplot
  # dataplot3$Item <- var_name
  
  if(rename == TRUE ){
    #02 -2 loadings -ggplot------
    dataplot$Item <- var_name
    
    gg <-ggplot(dataplot, aes(x=Item, y=std, 
                              fill=Latent))+
      geom_bar(stat="identity", position='dodge')+
      geom_hline(yintercept = cut , color= "red")+ #cut: 기준 0.7
      geom_hline(yintercept = cut-0.2, color= "gray40")+ #cut: 기준 0.7
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
    # print("변수명을 바꾼어 정렬을 하고자 하면, rename=TRUE로 하신 후에 var_name=c(변수명, ...)을 입력하세요. 입력순서는 lavaan model순서대로입니다.")
    gg <-ggplot(dataplot, aes(x=Item, y=std, 
                              fill=Latent))+
      geom_bar(stat="identity", position='dodge')+
      geom_hline(yintercept = cut , color= "red")+ #cut: 기준 0.7
      geom_hline(yintercept = cut-0.2, color= "gray40")+ #cut: 기준 0.7
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
      geom_hline(yintercept = cut, color= "red")+ #cut: 기준 0.7
      geom_hline(yintercept = cut-0.2, color= "gray40")+ #cut: 기준 0.7
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
  
  #," average variance extracted(AVE)"=avevar)
  
  #05 Reprort cronbach, AVE, C.R
  FL.1 <- cbind(alpha.1)
  FL <-FL.1%>%kable(digits=3, format=format,
                    caption="03-1. Internal consistency
          (Cronbach's Alpha, 1951) and Composite Relibility
   Cronbach’s α (values ≥ .7 or .8 indicate good reliability; Kline (1999))
                   ")
  
  
  
  
  ## 03 CR,AVE-------
  # ?semTools::reliability
  AVE <-  semTools::reliability(x, return.total = F) %>%
    t() %>%
    as.data.frame() %>%
    dplyr::select( "AVE"=avevar)
  
  sqrt.AVE <- sqrt(AVE)
  colnames(sqrt.AVE)="sqrt.AVE"
  
  #correlations Matrix
  rho <- lavInspect(x,"std")$beta
  
  
  
  
  ## 04 Convergent validity-----
  alpha_AVE_CR <-   semTools::reliability(x, return.total = FALSE) %>%
    # alpha_AVE_CR <-  reliability(x) %>%
    t() %>%
    as.data.frame() %>%
    dplyr::select("Cronbach"=alpha, 
                  "CR" = omega3, "AVE"=avevar) %>%
    mutate(sqrt.AVE=sqrt(AVE))%>%
    mutate(AVE_check=ifelse(AVE>0.5,"Accept(>0.5) *","Reject"))%>%
    dplyr::select(Cronbach,CR, AVE,AVE_check, #sqrt.AVE
    ) %>%
    kable(digits = 3,format = format,
          caption = "03 Convergent validity
          Internal consistency(Cronbach's Alpha, 1951)(>0.7)
          AVE(>0.5) & CR(>0.7): Fornell & Lacker(1981)")
  
  
  
  
  #check data
  # lv.cor <-lavInspect(x, what="cor.lv")
  # lv.cor1<-lv.cor
  # diag(lv.cor1)<-0
  # lv.cor_df<-lv.cor1 %>% as.data.frame()
  # lv.cor_df[lower.tri(lv.cor_df)==FALSE]<-0
  # lv.cor_df
  
  
  #05-1 discriminant validity=====
  betaa <- lavInspect(x, "std")$beta
  
  if(is.null(betaa)){
    
    psi <-lavInspect(x, "std")$psi
    psi[lower.tri(psi)==FALSE]<-0
    
    rho1<- psi %>% as.data.frame()
    rho1$max<- apply(rho1,1,max)
    # diff<- cbind(rho1$max, sqrt.AVE) #행이 다르면 계산안됨
    # diff$delta<-diff[,2]- diff[,1]
    # diff$sig<-ifelse(diff$delta >= 0,"*","ns")
    # 
    # FornellNacker <-cbind(psi, max_rho=diff[,1],
    #                       sqrt.AVE,  # row 396
    #                       sig=diff[,4]) %>% as.data.frame()
    
    #데이터 결합(새롭게 수정 )
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
      kable(digits=3, format=format,
            caption="04 Discriminant Validity:
          rho < Square Root of(AVE)
           By Fornell & Lacker(1981)")
    
  }else{
    lv.cor <- lavInspect(x, what="cor.lv")
    lv.cor1 <- lv.cor
    diag(lv.cor1)<-0
    
    rho1 <- lv.cor1 %>% as.data.frame()
    rho1[lower.tri(rho1)==FALSE]<-0
    rho1$max <- apply(rho1,1, max)
    
    #데이터 결합
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
      kable(digits=3, format=format,
            caption="04 Discriminant Validity:
          rho < Square Root of(AVE)
           By Fornell & Lacker(1981)")
  }
  # 05-2 discriminant :HTMT #####
  # Assessing Discriminant Validity using Heterotrait–Monotrait Ratio
  # discriminant validity through the heterotrait-monotrait ratio (HTMT) of the correlations (Henseler, Ringlet & Sarstedt, 2015)
  
  if( is.character(model)==TRUE | 
      is.data.frame(dataset)==TRUE){
    
    options(knitr.kable.NA = '') #NA감추기 
    #dataframe생성 
    htmt0 <- semTools::htmt(model, dataset) %>% 
      as.data.frame()
    
    htmt0[lower.tri(htmt0)==FALSE]<-0 #대각성분을 0으로 만들기
    htmt0NA <- htmt0 #NA데이터 처리 
    htmt0NA[lower.tri(htmt0)==FALSE]<-NA   #상위성분 NA로 변경 
    htmt1 <- htmt0 %>%   #유의성값 만들기  
      mutate(Max = apply(htmt0, 1, max, na.rm=T),  #최댓값
             dis = ifelse(0.9 - Max== 0.9, 0, 0.9 - Max),  #판별 
             sig = ifelse(0.9- Max >= 0,"*","ns")) #유의성
    htmt2 <-cbind(htmt0NA, 
                  htmt1[,c(ncol(htmt1)-2, #max
                           ncol(htmt1)-1, #dis
                           ncol(htmt1))] ) #sig
    
    
    htmt <- htmt2  %>%
      kable(format=format, digits = digits,
            caption="The heterotrait-monotrait ratio of correlations (HTMT).
          이종 특성 -단일 특성 상관 관계 비율(HTMT)
          All correalation < 0.9 --> discriminant Accept(robusrst)
          general accept: < 1
          (Henseler, Ringlet & Sarstedt, 2015)

          ")
    
    
  }else{
    htmt <- print("Not calculation HTMT, input syntax is [ model = lavaan model,dataset = data] ")
    
  }
  
  
  ##06 cor significant----
  lv.cor.sig <- parameterEstimates(x, standardized = T) %>%
    filter(op=="~"|op=="~~"&lhs != rhs) %>%
    dplyr::select(lhs,op,rhs, std.lv, pvalue) %>%
    mutate(sig=ifelse(pvalue < 0.001, "***",
                      ifelse(pvalue < 0.01, "**",
                             ifelse(pvalue < 0.05, "*", "Not Sig")))) %>%
    mutate(op=ifelse(op=="~","<--",
                     ifelse(op=="~~","cor",""))) %>%
    dplyr::select(lhs,op,rhs, std.lv, pvalue,sig) %>%
    kable(digits=3, format=format,
          caption="05 latent correlation Significant Check")
  
  
  
  ##최종결과물 출력 --------------
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
  ## cfa2()출력 옵션--------------- 
  switch(res,
         all = all.reuslt,
         model= model,
         modelfit=fit,
         modelfit2=fitMeasures_s1,
         loadings = factorloading,
         alpha = FL,
         CR_AVE=alpha_AVE_CR,
         Convegent=alpha_AVE_CR,
         fl_criteria = validity,
         Discriminant = validity,
         htmt = htmt,
         HTMT = htmt,
         # Latent_Cor=lv.cor,
         str_cor=lv.cor.sig,
         loadings_Bar=gg )
  
}


#cfa2함수의 각각을 표로 그리는 함수 
#quick markdown_table , kable(format="html) using cfa2()-----------
markdown_table_s <- function(data,
                             caption="html to markdown",
                             full_width=FALSE,
                             font_size=20,
                             row.names = NA,
                             col.names = NA,
                             centering = TRUE,
                             digits=3,
                             show="markdown"
){
  
  library(kableExtra)
  
  if(show=="kbl"){
    data %>%as.data.frame() %>% 
      kbl(digits = digits, 
          caption =  caption,
          row.names=row.names,col.names=col.names,
          centering=centering
      ) %>%
      kable_classic(full_width=full_width,font_size= font_size)
  }else if(show=="markdown"){
    data %>% 
      kable_classic(full_width=full_width,font_size= font_size)
  }
}

#cfa3 다른 분석처리용 함수=============
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
    # kable(digits=3, format=format,
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
    # kable (format=format,
    #        caption = "01 Model fit information")
    #



    #04 factor loading
    options(knitr.kable.NA="")



# 
# 
#     factorloading <- parameterEstimates(x, standardized=TRUE) %>%
#         filter(op=="=~") %>%
#         mutate(stars=ifelse(pvalue < 0.001, "***",
#                             ifelse(pvalue < 0.01, "**",
#                                    ifelse(pvalue < 0.05, "*", "")))) %>%
#         mutate(label=ifelse(std.all>0.7,"Yes(Good)",
#                             ifelse(std.all>0.5,"Yes(fair)","No"))) %>%
#         dplyr::select("Latent"=lhs, Item=rhs, Est=est,S.E.=se,
#                       cr=z, Sig.=stars, "p"=pvalue,
#                       std=std.all, beta_Accept=label) #%>%
#     # kable(digits=3, format=format,
#     #       caption="02 Indicator Validity(1)-Factor Loadings::
#     #       (1) cr(critical ratio =Estimate/S.E) p<0.05,
#     #       (2) std.damda >= 0.5(Bagozzi & Yi(1988)")
# 
    ## 02 factor loading-----
    factorloading <- parameterEstimates(x, standardized=TRUE) %>%
      filter(op=="=~") %>%
      mutate(stars=ifelse(pvalue < 0.001, "***",
                        ifelse(pvalue < 0.01, "**",
                                 ifelse(pvalue < 0.05, "*", "")))) %>%
      mutate(label=ifelse(std.all>0.7,"Yes(Good)",
                          ifelse(std.all>0.5,"Yes(fair)","No"))) %>%
      dplyr::select("Latent"=lhs, Item=rhs, Est=est,S.E.=se,
                    cr=z, Sig.=stars, "p"=pvalue,
                    std=std.all, beta_Accept=label)
    

    if(graph==T){    
    dataplot0 <- parameterEstimates(x, standardized=TRUE) %>%
      filter(op=="=~") %>%
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
      geom_hline(yintercept = cut, color= "red")+ #cut: 기준 0.7
      geom_hline(yintercept = cut-0.2, color= "gray40")+ #cut: 기준 0.7
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

    #," average variance extracted(AVE)"=avevar)

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
    rho <- lavInspect(x,"std")$beta




    # Convergent validity
    alpha_AVE_CR <-  semTools::reliability(x,return.total = F) %>%
        t() %>%
        as.data.frame() %>%
        dplyr::select("Cronbach"=alpha, "CR" = omega3, "AVE"=avevar) %>%
        mutate(sqrt.AVE=sqrt(AVE))%>%
        mutate(AVE_check=ifelse(AVE>0.5,"Accept(>0.5) *","Reject"))%>%
        dplyr::select(Cronbach,CR, AVE,AVE_check, #sqrt.AVE
        ) #%>%
    # kable(digits = 3,format = format,
    #       caption = "03 Convergent validity
    #       Internal consistency(Cronbach's Alpha, 1951)(>0.7)
    #       AVE(>0.5) & CR(>0.7): Fornell & Lacker(1981)")



    #06 discriminant validity
    betaa <-lavInspect(x, "std")$beta

    if(is.null(betaa)){

        psi <-lavInspect(x, "std")$psi
        psi[lower.tri(psi)==FALSE]<-0

        rho1<- psi %>% as.data.frame()
        rho1$max<- apply(rho1,1,max)
        diff<- cbind(rho1$max,sqrt.AVE)
        diff$delta<-diff[,2]- diff[,1]
        diff$sig<-ifelse(diff$delta>0,"*","Not Sig")

        FornellNacker <-cbind.data.frame(psi, max_rho=diff[,1],sqrt.AVE,
                              sig=diff[,4]) %>% as.data.frame()


        validity <- FornellNacker #%>%
        # kable(digits=3, format=format,
        #       caption="04 Discriminant Validity:
        #     rho < Square Root of(AVE)
        #      By Fornell & Lacker(1981)")

    }else{
        lv.cor <- lavInspect(x, what="cor.lv")
        lv.cor1 <- lv.cor
        diag(lv.cor1)<-0

        rho1 <- lv.cor1 %>% as.data.frame()
        rho1[lower.tri(rho1)==FALSE]<-0
        rho1$max <- apply(rho1,1, max)

        #데이터 결합
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
          #   kable(digits=3, format=format,
          #         caption="04 Discriminant Validity:
          # rho < Square Root of(AVE)
          #  By Fornell & Lacker(1981)")

    }



    # cor significant
    lv.cor.sig <- parameterEstimates(x, standardized = T) %>%
        filter(op=="~"|op=="~~"&lhs != rhs) %>%
        dplyr::select(lhs,op,rhs, std.lv, pvalue) %>%
        mutate(sig=ifelse(pvalue < 0.001, "***",
                          ifelse(pvalue < 0.01, "**",
                                 ifelse(pvalue < 0.05, "*", "Not Sig")))) %>%
        mutate(op=ifelse(op=="~","<--",
                         ifelse(op=="~~","cor",""))) %>%
        dplyr::select(lhs,op,rhs, std.lv, pvalue,sig)# %>%
    # kable(digits=3, format=format,
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




#수정지수
Modindices<- function(x,op1="~~",mi.level=10){
  modindices(x) %>%
    filter(op==op1 & mi> mi.level) %>%
    arrange(desc(mi))

}






#모형적합도----------------
Compare_Fit <- function(...,
                       option=1,
                       format="markdown") {
  library(magrittr)
  # library(stargazer)
  library(tibble)
  library(knitr)
  library(dplyr)


  m <- list(...)
tryCatch({

  if(option==1){ #knitr table 출력
    result <-sapply(m, fitMeasures) %>%
      set_colnames(paste0("Model_", 1:length(m))) %>%
      as.data.frame() %>%
      rownames_to_column("Fit_Measures") %>%
      slice(match(c("chisq",
                    "df",
                    "pvalue",
                    "rmsea",
                    "rmsea.ci.lower",
                    "rmsea.ci.upper",
                    "srmr",
                    "cfi",
                    "tli",
                    "gfi",
                    "aic",
                    "bic"),
                  Fit_Measures)) %>%
      mutate(Fit_Measures=c("Chi-square",
                            "df",
                            "p-value",
                            "RMSEA(<0.05)",
                            "_[rmsea.ci.lower",
                            ", rmsea.ci.upper]",
                            "SRMR(<0.08)",
                            "CFI(>0.95)",
                            "TLI(NNFI)(>0.9)",
                            "GFI(>0.95)",
                            "AIC",
                            "BIC"))%>%
      kable(digits=3, format=format,
            caption="model comparison")
  }else{
    if(option==2){n # data.frame로 출력
      result <-sapply(m, fitMeasures) %>%
        set_colnames(paste0("Model_", 1:length(m))) %>%
        as.data.frame() %>%
        rownames_to_column("Fit_Measures") %>%
        slice(match(c("chisq",
                      "df",
                      "pvalue",
                      "rmsea",
                      "rmsea.ci.lower",
                      "rmsea.ci.upper",
                      "srmr",
                      "cfi",
                      "tli",
                      "gfi",
                      "aic",
                      "bic"),
                    Fit_Measures)) %>%
        mutate(Fit_Measures=c("Chi-square",
                              "df",
                              "p-value",
                              "RMSEA(<0.05)",
                              "_[rmsea.ci.lower",
                              ", rmsea.ci.upper]",
                              "SRMR(<0.08)",
                              "CFI(>0.95)",
                              "TLI(NNFI)(>0.9)",
                              "GFI(>0.95)",
                              "AIC",
                              "BIC"))


    }
    else{ return("option = 1, 2중 선택")}
  }
  return(result)

},error=function(e)return("option=1, 혹은 option=2 로 정확히 입력하세요 "))

}

#모형적합도 비교 함수 -----
CompareFit2 <- function(...) {
  library(magrittr)
  library(tibble)
  library(knitr)
  library(dplyr)


  m <- list(...)
  tryCatch({


      result <- sapply(m, fitMeasures) %>%
        set_colnames(paste0("Model_", 1:length(m))) %>%
        as.data.frame() %>%
        rownames_to_column("Fit_Measures") %>%
        slice(match(c("chisq",
                      "df",
                      "pvalue",
                      "rmsea",
                      "rmsea.ci.lower",
                      "rmsea.ci.upper",
                      "srmr",
                      "cfi",
                      "tli",
                      "gfi",
                      "aic",
                      "bic"),
                    Fit_Measures)) %>%
        mutate(Fit_Measures=c("Chi-square",
                              "df",
                              "p-value",
                              "RMSEA(<0.05)",
                              "_[rmsea.ci.lower",
                              ", rmsea.ci.upper]",
                              "SRMR(<0.08)",
                              "CFI(>0.95)",
                              "TLI(NNFI)(>0.9)",
                              "GFI(>0.95)",
                              "AIC",
                              "BIC"))

    return(result)

  },error=function(e)return("^^; 그렇게 하지마... "))

}

#모형적합도 비교 ----
CompareFit_diff<- function(...) {
  library(magrittr)
  # library(stargazer)
  library(tibble)
  library(knitr)
  library(dplyr)


  m <- list(...)

    result <-sapply(m, fitMeasures) %>%
      set_colnames(paste0("Model_", 1:length(m))) %>%
      as.data.frame() %>%
      rownames_to_column("Fit_Measures") %>%
      slice(match(c("chisq",
                    "df",
                    "pvalue",
                    "rmsea",
                    "rmsea.ci.lower",
                    "rmsea.ci.upper",
                    "srmr",
                    "cfi",
                    "tli",
                    "gfi",
                    "aic",
                    "bic"),
                  Fit_Measures)) %>%
      mutate(Fit_Measures=c("Chi-square",
                            "df",
                            "p-value",
                            "RMSEA(<0.05)",
                            "_[rmsea.ci.lower",
                            ", rmsea.ci.upper]",
                            "SRMR(<0.08)",
                            "CFI(>0.95)",
                            "TLI(NNFI)(>0.9)",
                            "GFI(>0.95)",
                            "AIC",
                            "BIC"))

    tryCatch({
      result%>% mutate("diff(M2-M1)"=`Model_2`-`Model_1`) %>%
        mutate(check=ifelse(`diff(M2-M1)`>0,"증가(+)",
                            ifelse(`diff(M2-M1)`<0,"감소(-)",""))) %>%
        kable("markdown",3)

    },error=function(e)return("비교할 2개의 sem분석데이터를 입력하세요 ") )



}



#CFA:factor loading ------
factor_loadings <- function(x,format="markdown"){

  #factor loading
  library(ggplot2)

  options(knitr.kable.NA="")

  factorloading <- parameterEstimates(x, standardized=TRUE) %>%
    filter(op=="=~") %>%
    mutate(stars=ifelse(pvalue < 0.001, "***",
                        ifelse(pvalue < 0.01, "**",
                               ifelse(pvalue < 0.05, "*", "")))) %>%
    mutate(Accept=ifelse(std.all > 0.7,"Yes(**)",
                         ifelse( std.all > 0.5,"Yes( *)","No"))) %>%
    dplyr::select("Latent"=lhs,op, Item=rhs, Est=est, SE=se,
                  "cr(t)"=z, Sig.=stars, #"p-value"=pvalue,
                  std=std.all,  Accept) %>%
    kable(digits=3, format=format, caption="Factor Loadings::
          (1) c.r(=Estimate/S.E) p<0.05,
          (2) std.damda >= 0.5")

# 

  # cfa.result <- x %>% cfa3()
  dataplot <- parameterEstimates(x, standardized=TRUE) %>%
    dplyr::filter(op=="=~")%>% 
    rename(Item=rhs, Latent=lhs,std=std.all) %>% 
    dplyr::select(Item, Latent, std)

  gg<-ggplot(dataplot,aes(x=Item, y=std, fill=Latent))+
    geom_bar(stat="identity", position='dodge')+
    geom_hline(yintercept = 0.7, color= "red")+
    ggtitle("factor loadings")+
    geom_text(aes(label=round(std,2)),vjust=-.3)+
    theme(axis.text.x = element_text(angle=90))

  res<-list(factorloading, gg)
  res


}


factor_loadings2 <- function(x,format="markdown",
                             cut=0.7,
                             angle=90, cex=11, hjust=0.9,
                             val.size=4){

  #factor loading
  library(ggplot2)

  options(knitr.kable.NA="")

  factorloading <- parameterEstimates(x, standardized=TRUE) %>%
    filter(op=="=~") %>%
    mutate(stars=ifelse(pvalue < 0.001, "***",
                        ifelse(pvalue < 0.01, "**",
                               ifelse(pvalue < 0.05, "*", "")))) %>%
    mutate(Accept=ifelse(std.all>0.7,"Yes(**)",ifelse(std.all>0.5,"Yes( *)","No"))) %>%
    dplyr::select("Latent"=lhs,op, Item=rhs, Est=est, SE=se,
                  "cr(t)"=z, Sig.=stars, #"p-value"=pvalue,
                  std=std.all,
                  Accept) %>%
    kable(digits=3, format=format, caption="Factor Loadings::
          (1) c.r(=Estimate/S.E) p<0.05,
          (2) std.damda >= 0.5")



  dataplot0<-parameterEstimates(x, standardized=TRUE) %>%
    filter(op=="=~") %>%
    mutate(stars=ifelse(pvalue < 0.001, "***",
                        ifelse(pvalue < 0.01, "**",
                               ifelse(pvalue < 0.05, "*", "")))) %>%
    mutate(label=ifelse(std.all>0.7,"Yes(Good)",
                        ifelse(std.all>0.5,"Yes(fair)","No"))) %>%
    dplyr::select("Latent"=lhs, Item=rhs, Est=est,S.E.=se,
                  cr=z, Sig.=stars, "p"=pvalue,
                  std=std.all, beta_Accept=label)
  dataplot<- dataplot0%>% dplyr::select(Item,Latent, std)

  #02 -2 loadings ggplot-
  gg <-ggplot(dataplot,aes(x=Item, y=std, fill=Latent))+
    geom_bar(stat="identity", position='dodge')+
    geom_hline(yintercept = cut, color= "red")+ #cut: 기준 0.7
    geom_hline(yintercept = cut-0.2, color= "gray40")+ #cut: 기준 0.7
    ggtitle("factor loadings")+
    geom_text(aes(label=round(std,2)),vjust=-.3, size=val.size)+
    theme(axis.text.x = element_text(
      angle=angle,
      size = cex, hjust = hjust,
      face="bold")) #angle, cex



  # cfa.result <-x %>% cfa3()
  # dataplot<-cfa.result$factorloadings %>% dplyr::select(Item,Latent, std)
  #
  # gg<-ggplot(dataplot,aes(x=Item, y=std, fill=Latent))+
  #   geom_bar(stat="identity", position='dodge')+
  #   geom_hline(yintercept = 0.7, color= "red")+
  #   ggtitle("factor loadings")+
  #   geom_text(aes(label=round(std,2)),vjust=-.3)+
  #   theme(axis.text.x = element_text(angle=90))

  # res<-list(factorloading, gg)
  # res
res=  list(factorloading,bargraph= gg)
res
}


# factor그래프를  변수명을 바꾸어서 새로 그리기 -----
factor_loadings_grp <-function(data,
                              cut=0.7,
                              angle=90,
                              cex=11 ,
                              hjust=0.9,
                              val.size=4,
                              rename= FALSE,
                              var_name=c("")
){

  dataplot0<-parameterEstimates(data, standardized=TRUE) %>%
    filter(op=="=~") %>%
    mutate(stars=ifelse(pvalue < 0.001, "***",
                        ifelse(pvalue < 0.01, "**",
                               ifelse(pvalue < 0.05, "*", "")))) %>%
    mutate(label=ifelse(std.all>0.7,"Yes(Good)",
                        ifelse(std.all>0.5,"Yes(fair)","No"))) %>%
    dplyr::select("Latent"=lhs, 
                  Item = rhs, 
                  Est = est,
                  S.E.=se,
                  cr=z, Sig.=stars, "p"=pvalue,
                  std=std.all, beta_Accept=label)
  dataplot<- dataplot0%>% dplyr::select(Item, Latent, std)

  if(rename==TRUE ){
    #02 -2 loadings -ggplot------
    dataplot$Item <- var_name
    gg <-ggplot(dataplot,aes(x=Item, y=std, fill=Latent))+
      geom_bar(stat="identity", position='dodge')+
      geom_hline(yintercept = cut , color= "red")+ #cut: 기준 0.7
      geom_hline(yintercept = cut-0.2, color= "gray40")+ #cut: 기준 0.7
      ggtitle("factor loadings")+
      geom_text(aes(label=round(std,2)),vjust=-.3, size=val.size)+
      theme(axis.text.x = element_text(
        angle=angle,
        size = cex, hjust = hjust,
        face="bold")) #angle, cex

  }

    if(rename== FALSE) {
      print("변수명을 바꾼어 정렬을 하고자 하면, rename=TRUE로 하신 후에 var_name=c(변수명, ...)을 입력하세요. 입력순서는 lavaan model순서대로입니다.")
      gg <-ggplot(dataplot, aes(x=Item, y=std, fill=Latent))+
        geom_bar(stat="identity", position='dodge')+
        geom_hline(yintercept = cut , color= "red")+ #cut: 기준 0.7
        geom_hline(yintercept = cut-0.2, color= "gray40")+ #cut: 기준 0.7
        ggtitle("factor loadings")+
        geom_text(aes(label=round(std,2)),vjust=-.3, size=val.size)+
        theme(axis.text.x = element_text(
          angle= angle,
          size = cex, hjust = hjust,
          face="bold")) #angle, cex
    }


  gg
}

#varname참고용 : varname을 바꾸기 위해서 참고할 것 
factor_varname<- function(x,input=NULL){
  parameterEstimates(x) %>% 
    dplyr::filter(op=="=~") %>% 
    dplyr::select(rhs) %>% as.vector()
}

# factor_varname

# factor_loadings_grp(lrm_sem,
# var_name= c(
#   "a인정신념",
#   "a불안신념2",
#   "a불안신념1",
#   "a의존신념",
#   "a해결신념",
#   "b진로탐색",
#   "b정보수집",
#   "b직업체험",
#   "c구체성",
#   "c적합성",
#   "c적합성")
# )


# fit.math2 %>% factor_loadings()

#CR-수기계산 ################
CR <- function(x,digit=3, markdown=TRUE){
  library(knitr)
  library(dplyr)
  lavInspect(x,"std")
  #lambda
  lavInspect(x,"std")$lambda
  l.matrix <- lavInspect(x,"std")$lambda
  l.matrix[l.matrix==0]<-NA
  #theta: error coefficient
  t.matrix<- lavInspect(x,"std")$theta
  t.matrix[t.matrix==0]<-NA

  #t.matrix
  cr1 <- apply(l.matrix, 2, sum, na.rm=T)
  d.sum <- apply(1-l.matrix^2, 2, sum, na.rm=T)
  CR.c<- cr1^2/(cr1^2 + d.sum)
  
  
  CR.c %>% barplot(ylim=c(0,1))
  text(x= CR.c %>% barplot(ylim=c(0,1)),y = CR.c+0.03,
       label= CR.c)
  abline(h=0.7, lty=2, col="red")

  
  if(markdown==TRUE){
    CR <- CR.c %>% kable(digits=digit, format="pandoc", caption="Construct Reliability > 0.7(Bagozzi and Yi 1988)")
    
  }else{
    CR <- CR.c #%>% kable(digits=digit, format="pandoc", caption="Construct Reliability > 0.7(Bagozzi and Yi 1988)")

  }
  CR.c %>% barplot(ylim=c(0,1))
  text(x= CR.c %>% barplot(ylim=c(0,1)),y = CR.c+0.03,
       label= CR.c)
  abline(h=0.7, lty=2, col="red")
  
    
  res = list(CR, CR.c) #result
  res

  }





#AVE수기계산#####
AVE <-function(x, digit=3, markdown=TRUE){
  library(knitr)
  l.matrix <- lavInspect(x,"std")$lambda
  l.matrix[l.matrix==0]<-NA   #불필요한 것은 삭제
  AVE.1<-apply(l.matrix^2, 2, mean, na.rm=T)
  
  if(markdown==TRUE){
  AVE<- AVE.1 %>% kable(digits=digit, format="pandoc",
                        caption="AVE(average variance extracted)>0.5")
  }else{
    AVE.1<-apply(l.matrix^2, 2, mean, na.rm=T)
  }
  
  AVE.1 %>% barplot(ylim=c(0,1))
  text(x= AVE.1 %>% barplot(ylim=c(0,1)),y = AVE.1+0.03,
       label= AVE.1)
  abline(h=0.5, lty=2, col="red")
  
  res = list(AVE, AVE.1) #result
  res
  
}



###02 CFA Convergent_Validity####
Convergent_Validity  <- function(x,format="markdown", digit=3){
  library(dplyr)
  library(knitr)
  library(semTools)

    rel.1<- semTools::reliability(x) %>% t() %>%
      data.frame() %>%
      dplyr::select(Cronbach=alpha, CR=omega3, AVE=avevar)

    rel.2<-rel.1 %>%
      mutate(Cronbach_check=ifelse(Cronbach>0.7,
                                   "Accept(>0.7) *","Reject")) %>%
      mutate(CR_check=ifelse(CR>0.7,"Accept(>0.7) *","Reject")) %>%
      mutate(AVE_check=ifelse(AVE>0.5,"Accept(>0.5) *","Reject")) %>%
      dplyr::select( Cronbach, 
                     Cronbach_check,
                     CR,
                     CR_check, 
                     AVE, 
                     AVE_check ) %>%
      kable(digits=digit, format=format,
            caption="
            내적일관성(internal consistancy): Cronbach(>0.7), CR(>0.7)
            집중타당도(Convergent Validity)- Hair(2009):AVE(>0.5)")


    rels<- list(#Reliability= round(rel.1, digit),
                Convergent_Validity=rel.2)
    rels
}


#Discrimonant_Validity -----
Discriminant_Validity <-function(x, format="markdown", sort=F){
  library(knitr)
  library(psych)

  library(semTools)

  rel.1<- semTools::reliability(x) %>% t() %>%
    data.frame() %>%
    dplyr::select(Cronbach=alpha, CR=omega3, AVE=avevar)


  #CR,AVE

  AVE <- semTools::reliability(x,return.total = F) %>%
    t() %>%
    as.data.frame() %>%
    dplyr::select( "AVE"=avevar)

  sqrt.AVE <- sqrt(AVE)
  colnames(sqrt.AVE)="sqrt.AVE"


  #Internal consistency
  FornellNacker1 <- cbind(rel.1, sqrt.AVE)%>%
    kable(digits=3, format=format)


  #CR,AVE

  AVE <- semTools::reliability(x,return.total = F) %>%
    t() %>%
    as.data.frame() %>%
    dplyr::select( "AVE"=avevar)

  sqrt.AVE <- sqrt(AVE)
  colnames(sqrt.AVE)="sqrt.AVE"


  #check 판별타당도에 대한 정확한 행렬 ####
  # lv.cor <-lavInspect(x, what="cor.lv")
  # lv.cor1<-lv.cor
  # diag(lv.cor1)<-0
  # lv.cor_df<-lv.cor1 %>% as.data.frame()
  # lv.cor_df[lower.tri(lv.cor_df)==FALSE]<-0
  # lv.cor_df


  #06 discriminant validity
  betaa <- lavInspect(x, "std")$beta

  if(is.null(betaa)){

      psi <-lavInspect(x, "std")$psi
      psi[lower.tri(psi)==FALSE]<-0

      rho1<- psi %>% as.data.frame()
      rho1$max<- apply(rho1,1,max)
      diff<- cbind(rho1$max,sqrt.AVE)
      diff$delta<-diff[,2]- diff[,1]
      diff$sig<-ifelse(diff$delta>0,"*","ns")

      FornellNacker <-cbind(psi, max_rho=diff[,1],sqrt.AVE,
                            sig=diff[,4]) %>% as.data.frame()


      validity <- FornellNacker %>%
          kable(digits=3, format=format,
                caption="04 Discriminant Validity:
          rho < Square Root of(AVE)
           By Fornell & Lacker(1981)")

  }else{
      lv.cor <- lavInspect(x, what="cor.lv")
      lv.cor1 <- lv.cor
      diag(lv.cor1)<-0

      rho1 <- lv.cor1 %>% as.data.frame()
      rho1[lower.tri(rho1)==FALSE]<-0
      rho1$max <- apply(rho1,1, max)

      #데이터 결합
      rho1 <- rho1 %>% mutate(max=apply(rho1,1, max), lv =rownames(rho1))
      sqrt.AVE$lv <- rownames(sqrt.AVE)

      diff <- merge(x=rho1, y= sqrt.AVE, by="lv", all=TRUE, sort = sort) #

      diff$sqrt.AVE[is.na(diff$sqrt.AVE)] <- 0

      # diff <- cbind(rho1$max, sqrt.AVE)
      diff$delta <- diff$max - diff$sqrt.AVE
      diff$sig <- ifelse(diff$delta == 0,"*",
                         ifelse(diff$delta < 0,"*","ns"))

      #데이터 정리
      FornellNacker <- diff[,c(-(ncol(diff)-1))]



      validity <- FornellNacker  %>%
          kable(digits=3, format=format,
                caption="04 Discriminant Validity:
          rho < Square Root of(AVE)
           By Fornell & Lacker(1981)")



  }

  result.1 <- list(#Convegent=FornellNacker1,
                   Discriminant_validity=validity)
  result.1
}




#HTMT --------
Discriminant_HTMT <- function(model= NA, #lavaan syntax 
                              dataset=NA, #data
                 format="markdown", digits= 3,
                 sample.cov = NULL, missing = "listwise",
                 ordered = NULL, absolute = TRUE, htmt2 = TRUE
){
  library(semTools)
  
  #test model syntax
  if( is.character(model)==TRUE | 
      is.data.frame(dataset)==TRUE){
    
    options(knitr.kable.NA = '') #NA감추기 
    
    #dataframe생성 
    htmt0 <- semTools::htmt(model, dataset) %>% 
      as.data.frame()
    
    htmt0[lower.tri(htmt0)==FALSE] <- 0 #대각성분을 0으로 만들기
    htmt0NA  <- htmt0 #NA데이터 처리 
    htmt0NA[lower.tri(htmt0)==FALSE] <- NA   #상위성분 NA로 변경 
    htmt1 <- htmt0 %>%   #유의성값 만들기  
      mutate(Max = apply(htmt0, 1, max, na.rm=T),  #최댓값
             dis = ifelse(0.9 - Max== 0.9, 0, 0.9 - Max),  #판별 
             sig = ifelse(0.9- Max >= 0,"*","ns")) #유의성
    
    htmt <- htmt1  %>%
      kable(format=format, digits=digits,
            caption="The heterotrait-monotrait ratio of correlations (HTMT).
          이종 특성 -단일 특성 상관 관계 비율(HTMT)
          All correalation < 0.9 --> discriminant Accept(robusrst)
          general accept: < 1
          (Henseler, Ringlet & Sarstedt, 2015)

          ")
    
    
  }else{
    htmt <- print("Not calculation HTMT, input syntax is [ model = lavaan model,dataset = data] ")
    
  }
  
  res = list(htmt,    #판별값
             semTools::htmt(model = model, data=dataset) %>%
               as.data.frame()   )#오리지널값
  res
}



# The heterotrait-monotrait ratio of correlations (HTMT).
# 이종특성 -단일 특성 상관 관계 비율(HTMT)
HTMT <- function(model= NA, 
                 dataset=NA,
                 format="markdown", digits= 3,
                 sample.cov = NULL, missing = "listwise",
                 ordered = NULL, absolute = TRUE,
                 htmt2 = TRUE
){
  library(semTools)

  #test model syntax
  if( is.character(model)==TRUE | 
      is.data.frame(dataset)==TRUE){

    options(knitr.kable.NA = '') #NA감추기 

      #dataframe생성 
    htmt0 <- semTools::htmt(model, dataset) %>% 
      as.data.frame()
    
    htmt0[lower.tri(htmt0)==FALSE] <- 0 #대각성분을 0으로 만들기
    htmt0NA  <- htmt0 #NA데이터 처리 
    htmt0NA[lower.tri(htmt0)==FALSE] <- NA   #상위성분 NA로 변경 
    htmt1 <- htmt0 %>%   #유의성값 만들기  
      mutate(Max = apply(htmt0, 1, max, na.rm=T),  #최댓값
             dis = ifelse(0.9 - Max== 0.9, 0, 0.9 - Max),  #판별 
             sig = ifelse(0.9- Max >= 0,"*","ns")) #유의성
    
    htmt <- htmt1  %>%
      kable(format=format, digits=digits,
            caption="The heterotrait-monotrait ratio of correlations (HTMT).
          이종 특성 -단일 특성 상관 관계 비율(HTMT)
          All correalation < 0.9 --> discriminant Accept(robusrst)
          general accept: < 1
          (Henseler, Ringlet & Sarstedt, 2015)

          ")


  }else{
    htmt <- print("Not calculation HTMT, input syntax is [ model = lavaan model,dataset = data] ")

  }

  res = list(htmt,    #판별값
             semTools::htmt(model = model, data=dataset) %>%
               as.data.frame()   )#오리지널값
  res
}




#SEM 경로모형 결과-------
effect<- function(x,type="regress"){

  library(dplyr)
  # library(stargazer)

tryCatch({
  switch(type,
         basic=effect1(x),
         label=effect2(x),
         regress=effect3(x),
         dataframe=effect4(x),
         group=effect5(x),
         group1=effect6(x),
         compare=effect_grp(x),
         compare.p=effect_grp.p(x)
         )
},error=function(e) {
  return("입력 오류입니다.
         정확한 명칭을 입력해주세요(basic, label, regress, effect, group) ")})

}

effect_grp<- function(x,type="compare"){

  library(dplyr)
  # library(stargazer)

  tryCatch({
    switch(type,
           group=effect5(x),
           group1=effect6(x),
           compare=effect_grp(x),
           compare.p=effect_grp.p(x)
    )
  },error=function(e) {
    return("입력 오류입니다.
         정확한 명칭을 입력해주세요(basic, label, regress, effect, group) ")})

}



effect1_reg <-function(x,format="markdown",op1="~",op2=":=", what="std"){
    library(dplyr)
    # library(stargazer)
    library(knitr)
    library(semPlot)
    library(semptools)

    fitdata <- fitMeasures(x,c(
        "chisq","df",
        "pvalue",
        "rmsea",
        "rmsea.ci.lower",
        "rmsea.ci.upper",
        "rmsea.pvalue",
        "srmr",
        "gfi",
        "cfi",
        "tli",
        "aic",
        "bic") )
    res0 <- parameterEstimates(x, standardized = T,
                               rsquare = T,ci=T) %>%
        filter(op==op1|op==op2) %>%
        mutate(stars=ifelse(pvalue<0.001,"***",
                            ifelse(pvalue<0.01,"**",
                                   ifelse(pvalue<0.05,"*","")))) %>%
        mutate(op=ifelse(op=="~","<--",
                         ifelse(op==":=","effect",""))) %>%
        dplyr::select(lhs,op,rhs,
                      "std"= std.all,
                      est,
                      se,
                      # ci.lower, ci.upper,
                      z,
                      Sig=stars,
                      "p"=pvalue
        ) #%>%
    mean <- parameterEstimates(x, standardized = T, rsquare = T,ci=T) %>%
        filter(op=="~1") %>%
            mutate(op=ifelse(op=="~1","(Mean) ",
                         ifelse(op==":=","effect",""))) %>%
        dplyr::select(lhs,#op,rhs,
                      # "std"= std.all,
                      est#,
                      # se,
                      # ci.lower, ci.upper,
        )
    Var <- parameterEstimates(x, standardized = T, rsquare = T,ci=T) %>%
        filter(op=="~~") %>%
        mutate(op=ifelse(op=="~~","(std) ",
                         ifelse(op==":=","effect",""))) %>%
        dplyr::select(lhs,#op,rhs,
                      # "std"= std.all,
                      est#,
                      # se,
                      # ci.lower, ci.upper,
        )

    statistic = merge(mean, Var, by="lhs", all.x=T)
    colnames(statistic)=c("Variable", "Mean","Var")

    # kable(format=format, digits = 3,
    #       caption ="Path coeff. & Total Effect(label)")
    semPaths(x, what = "est", fade=F, posCol="gray30",
             edge.label.cex = 0.9, nCharNodes = 10, rotation = 2,
             sizeMan = 10, sizeMan2 = 4, layout = "tree2",
             style = "lisrel", residScale = 12,intercepts = F,
             group="latman",pastel = T,
             mar = c(10,10,10,10)) %>% mark_sig(ex21_sem) %>% plot()

    res <- list(regrssion.effect=res0,
                statistic ,
                model.fit=fitdata)
   res
}

#구조모형 분석후 결과 보기자료

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
  # library(stargazer)
  library(knitr)
  library(semPlot)
  library(semptools)

 res0 <- parameterEstimates(x, standardized = T, rsquare = T,ci=T) %>%
    filter(op==op1|op==op2) %>%
    mutate(stars=ifelse(pvalue<0.001,"***",
                        ifelse(pvalue<0.01,"**",
                               ifelse(pvalue<0.05,"*","")))) %>%
    mutate(op=ifelse(op=="~","<--",
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
    kable(format=format, digits = 3,
         caption ="Path coefficient & Total Effect(label)")
 # what="std",
 # digits=3, layout = "tree2", curve=1,
 # mar = c(8,8,8,8),
 # # edge.label.cex=0.9,
 # name_change=FALSE,
 # label_list=NULL

 if(name_change==FALSE){
   diagram<-semPaths(x, what = what, fade=F, posCol="gray30",
           edge.label.cex = 1, edge.label.position=0.4,
           nCharNodes = 10,
           rotation = 2, curve=curve,
           sizeLat = 12, sizeLat2 = 6,
           sizeMan = 8, sizeMan2 = 4,
           layout = layout,
           style = "lisrel", residScale = residScale,intercepts = F,
           nDigits = digits,
           label.cex = label.cex, asize=asize,
           mar = mar) %>% mark_sig(x) %>% mark_se(x,"\n") %>%  plot()

   res <- list(res0, diagram)
   res

 }else if(name_change==TRUE){
   diagram <- semPaths(x, what = what, fade=F, posCol="gray30",
            edge.label.cex = edge.label.cex,
            edge.label.position=0.4,
            nCharNodes = 10,
            rotation = 2, curve=curve,
            sizeLat = sizeLat, sizeLat2 = sizeLat2,
            sizeMan = sizeMan, sizeMan2 = sizeMan2, layout = layout,
            style = "lisrel", residScale = 15,intercepts = F,
            nDigits = digits, border.width=2,
            label.cex = label.cex, asize=asize,
            mar = mar) %>% mark_sig(x) %>% mark_se(x,"\n") %>%
     change_node_label(
       label_list=label_list
     ) %>%  plot()

   res <- list(res0, diagram)
   res

 }


}

# ?change_node_label()
# node.name =list(
#   list(node="f인정욕구", to="인정신념"),
#   list(node="f파국화",  to="불안신념1"),
#   list(node="f과잉불안", to="불안신념2"),
#   list(node="f무력감",  to="의존신념"),
#   list(node="f개인완벽", to="해결신념"),
#   list(node="IRB",  to="비합리적신념"),
#   list(node="PREACT",  to="진로준비행동"),
#   list(node="DECISION",  to="진로결정수준"),
#   list(node="f정보수집",  to="정보수집"),
#   list(node="f직업체험",  to="직업체험"),
#   list(node="f진로탐색",  to="정보탐색"),
#   list(node="f구체성",  to="구체성"),
#   list(node="f확신성",  to="확신성"),
#   list(node="f적합성",  to="적합성"))

effect1_ci <-function(x,format="markdown",op1="~",op2=":="){
  library(dplyr)
  # library(stargazer)
  library(knitr)
  parameterEstimates(x, standardized = T, rsquare = T,ci=T) %>%
    filter(op==op1|op==op2) %>%
    mutate(stars=ifelse(pvalue<0.001,"***",
                        ifelse(pvalue<0.01,"**",
                               ifelse(pvalue<0.05,"*","")))) %>%
    mutate(op=ifelse(op=="~","<--",
                     ifelse(op==":=","effect",""))) %>%
    dplyr::select(lhs,op,rhs,
                  "std"= std.all,
                  est,
                  se,
                  ci.lower, ci.upper,
                   z,
                  Sig=stars,
                  "p"=pvalue
    ) #%>%
    # kable(format=format, digits = 3,
    #       caption ="Path coeff. & Total Effect(label)")
}




#label표시 ####
effect2 <-function(x,
                   det="_",
                   det2="",
                   op1="~",
                   op2=":="){
  library(dplyr)
  # library(stargazer)
  library(knitr)
  library(stringr)
  parameterEstimates(x, standardized = T, rsquare = T) %>%
    filter(op==op1|op==op2) %>%
    filter(str_detect(lhs, det) & str_detect(lhs, det2) )%>%
    mutate(stars=ifelse(pvalue<0.001,"***",
                        ifelse(pvalue<0.01,"**",
                               ifelse(pvalue<0.05,"*","")))) %>%
    mutate(op=ifelse(op=="~","<--",
                     ifelse(op==":=","==",""))) %>%
    dplyr::select(lhs,op, "std"= std.all,est,se,
                   # c.r=z,
                  Sig=stars, "p"=pvalue,rhs) #%>%
    # kable(format=format, digits = 3,
    #       caption ="Path coeff. & Total Effect(label)")
    # stargazer(type="text",
    # title="Regression & Total Effect (label) .",
    # summary = FALSE,
    #           digits = 3, digits.extra = 0, rownames = FALSE)

  }


#effect3 regress만 표시 ####
effect3 <-function(x, format="markdown",op1="~"){
  library(dplyr)
   library(knitr)
  parameterEstimates(x, standardized = T, rsquare = T) %>%
    filter(op==op1| op=="~~" & lhs !=rhs) %>%
    mutate(stars=ifelse(pvalue<0.001,"***",
                        ifelse(pvalue<0.01,"**",
                               ifelse(pvalue<0.05,"*","")))) %>%
    mutate(op=ifelse(op=="~","<--",ifelse(op=="~~","cor",""))) %>%
    dplyr::select(To=lhs,
                  Path=op,
                  From=rhs,
                  est,se,
                  est.std= std.all,
                  cr=z,
                  Sig=stars,
                  "p"=pvalue) %>%
    kable(format=format, digits = 3,
          caption ="Result of Path Analysis")
    # stargazer(type="text", title="Path Coefficient", summary = FALSE,
    #           digits = 3, digits.extra = 0, rownames = FALSE)



}





#label hyperthesis
effect3_label <-function(x, op1="~"){
  library(dplyr)
  library(knitr)
  parameterEstimates(x, standardized = T, ci=TRUE) %>%
    filter(op==op1|op=="~~" & lhs !=rhs) %>%
    mutate(stars=ifelse(pvalue<0.001,"***",
                        ifelse(pvalue<0.01,"**",
                               ifelse(pvalue<0.05,"*","")))) %>%
    mutate(op=ifelse(op=="~","<--",ifelse(op==":=","==",ifelse(op=="~~","cor","")))) %>%
    dplyr::select(To=lhs,Path=op,
                  From=rhs,
                  label,
                  B=est,
                  "S.E."=se,
                  beta = std.all,
                  "C.R."=z,
                  Sig=stars,
                  "p"=pvalue,
                  ci.lower, ci.upper)

}


# effect := total effect cal제외dataframe로 출력  ####
effect4 <-function(x, format="markdown"){
  library(dplyr)
  # library(stargazer)
  library(knitr)
library(stringr)
  parameterEstimates(x, standardized = T, rsquare = T) %>%
    filter(op==":=") %>%
    filter(str_detect(lhs, "_") )%>%
    mutate(stars=ifelse(pvalue<0.001,"***",
                        ifelse(pvalue<0.01,"**",
                               ifelse(pvalue<0.05,"*","")))) %>%
    mutate(op=ifelse(op=="~","<--","" )) %>%
    dplyr::select(Parameter=lhs,
                  "Path_Coeff"= std.all,
                  # "Est"=est,se,
                  # cr=z,
                  Sig.=stars,
                  p=pvalue,
                  Detail=rhs)
    # kable(format=format, digits# = 3,
          # caption ="Define New parameter value")
    # stargazer(type="text", title="Total Effect(Direct, Indirect)",
    #           summary = FALSE,
    #           digits = 3, digits.extra = 0, rownames = FALSE)

}


#effect5 group 표시 #####
effect5 <-function(x){
  library(dplyr)
  # library(stargazer)
  library(knitr)
  parameterEstimates(x, standardized = T, rsquare = T) %>%
    filter(op=="~"|op==":=") %>%
    mutate(stars=ifelse(pvalue<0.001,"***",
                        ifelse(pvalue<0.01,"**",
                               ifelse(pvalue<0.05,"*","")))) %>%
    mutate(op=ifelse(op=="~","<--",
                     ifelse(op==":=","effect",""))) %>%
    dplyr::select(lhs,Path=op, rhs,group,
                  "Est"=est,se,c.r=z,
                  Sig=stars, "p"=pvalue)# %>%
    # kable(format=format, digits = 3,
    #       caption ="Define New parameter value")
    # stargazer(type="text", title="Regression & Total Effect (label) .",
    #           summary = FALSE,
    #           digits = 3, digits.extra = 0, rownames = FALSE)

}

#effect3 regress만 표시 ####
#분석후 표로 그리기 위한 것
#멀티그룹 분석
effect6 <-function(x,
                   format="markdown",
                   op1="~",op2=":=",
                   group=TRUE,    #group analysis function
                   view=FALSE,
                   what="std",
                   digits=3,
                   layout = "tree2",
                   curve=1,
                   sizeLat = 12, sizeLat2 = 6,
                   sizeMan = 8, sizeMan2 = 2,
                   label.cex = 1,
                   mar = c(8,10,8,10),
                   residScale = 15,
                   edge.label.cex=1,
                   name_change=FALSE,
                   label_list=NULL

                   ){
  library(dplyr)
  library(knitr)
  library(semptools)


if(group==TRUE){
  res0<- parameterEstimates(x, standardized = T, rsquare = T) %>%
    filter(op==op1| op==op2) %>%
    mutate(stars=ifelse(pvalue<0.001,"***",
                        ifelse(pvalue<0.01,"**",
                               ifelse(pvalue<0.05,"*","")))) %>%
    mutate(op=ifelse(op=="~","<--",ifelse(op==":=","==",""))) %>%
    dplyr::select(Dependent=lhs,Path=op,
                  Independent=rhs,
                  group,  #다중그룹 분석을 한 경우 나타남
                  # label,
                  est,
                  se,
                  "Path_Coeff"= std.all,
                  c.r=z,
                  Sig=stars,
                  "p"=pvalue) %>%
    kable(format=format, digits = 3,
          caption="그룹별 경로비교 ")

}else if(group==FALSE){ #그룹기능
 res0<- parameterEstimates(x, standardized = T, rsquare = T) %>%
    filter(op==op1| op==op2) %>%
    mutate(stars=ifelse(pvalue<0.001,"***",
                        ifelse(pvalue<0.01,"**",
                               ifelse(pvalue<0.05,"*","")))) %>%
    mutate(op=ifelse(op=="~","<--",ifelse(op==":=","==",""))) %>%
    dplyr::select(Dependent=lhs,Path=op,
                  Independent=rhs,
                  # group,  #다중그룹 분석을 한 경우 나타남
                  # label,
                  est,
                  se,
                  "Path_Coeff"= std.all,
                  c.r=z,
                  Sig=stars,
                  "p"=pvalue) %>%
    kable(format=format, digits = 3,
          caption="그룹별 경로비교 ")
}
  # stargazer(type="text", title="Path Coefficient", summary = FALSE,
  #           digits = 3, digits.extra = 0, rownames = FALSE)

if(view==TRUE){

  if(name_change==FALSE){
    diagram<-semPaths(x, what = what, fade=F, posCol="gray30",
                      edge.label.cex = edge.label.cex,
                      nCharNodes = 10,
                      rotation = 2, curve=curve,
                      sizeLat = sizeLat, sizeLat2 = sizeLat2,
                      sizeMan = sizeMan, sizeMan2 = sizeMan2,
                      layout = layout,
                      style = "lisrel", residScale = residScale,
                      intercepts = F,
                      nDigits = digits,border.width=2,
                      label.cex = label.cex, asize=1.2,
                      mar = mar
                      ) #%>% mark_sig(x) %>% mark_se(x,"\n") %>%  plot()

  }else if(name_change==TRUE){
    diagram <- semPaths(x, what = what, fade=F, posCol="gray30",
                        edge.label.cex = edge.label.cex,
                        nCharNodes = 10,
                        rotation = 2, curve=curve,
                        sizeLat = sizeLat, sizeLat2 = sizeLat2,
                        sizeMan = sizeMan, sizeMan2 = sizeMan2,
                        layout = layout,
                        style = "lisrel", residScale = residScale,
                        intercepts = F,
                        nDigits = digits, border.width=2,
                        label.cex = label.cex, asize=1.2,
                        mar = mar
                        ) #%>%
      #  mark_sig(x) %>% mark_se(x,"\n") %>%
      # change_node_label(
      #   label_list=label_list
      # ) %>%  plot()
  }
  res =list(diagram, res0)
  res

}else{
   res0
 }

 # par(mfrow=c(1,1))

}


#다집단그룹비교 데이터를 wide하게 하여 비교
effect_grp_diff <- function(x,
                            caption=NULL,
                            effect="Diff"
                            ){
# library(stringr)

  grp1 <-parameterEstimates(x, standardized = T) %>%
    filter(op=="~"& group==1) %>%
    mutate(stars=ifelse(pvalue<0.001,"***",
                        ifelse(pvalue<0.01,"**",
                               ifelse(pvalue<0.05,"*","")))) %>%
    mutate(op=ifelse(op=="~","<--","")) %>%
    dplyr::select(TO=lhs,Path=op,
                  FROM=rhs,
                  label,
                  est,
                  se,
                  # "β"= std.all,   # 그룹간 beta를 비교하지 않음.
                  c.r=z,
                  Sig=stars,
                  "p"=pvalue)


  grp2 <-parameterEstimates(x, standardized = T) %>%
    filter(op=="~"&group==2) %>%
    mutate(stars=ifelse(pvalue<0.001,"***",
                        ifelse(pvalue<0.01,"**",
                               ifelse(pvalue<0.05,"*","")))) %>%
    mutate(op=ifelse(op=="~","<--","")) %>%
    dplyr::select(TO=lhs,Path=op,
                  FROM=rhs,
                  label,
                  est,
                  se,
                  # "β"= std.all,
                  c.r=z,
                  Sig=stars,
                  "p"=pvalue)

  diff <- parameterEstimates(x,standardized = TRUE,ci=TRUE ) %>%
    filter(op==":=") %>%
    filter(str_detect(lhs, effect) ) %>%
    mutate(Diff.sig=cut(pvalue,c(-Inf,0.001,0.01,0.05,1),
                   labels=c("***","**","*",""))) %>%
    dplyr::select(lhs,rhs, est,se, z,Diff.sig, pvalue) %>%
    arrange(lhs) %>%
    filter(rhs != 0) %>%
   kable("markdown",3,caption="Wald Test:: 두 그룹의 차이검정")



  diff.all <- parameterEstimates(x,standardized = TRUE,ci=TRUE ) %>%
    filter(op==":=") %>%
    filter(str_detect(lhs, effect) ) %>%
    mutate(Diff.sig=cut(pvalue,c(-Inf,0.001,0.01,0.05,1),
                        labels=c("***","**","*",""))) %>%
    dplyr::select(lhs,
           # rhs,
           est,se,
           z,
           Diff.sig,
           pvalue,
           ci.lower,
           ci.upper
           ) %>%
    arrange(lhs) %>%
    # filter(rhs !=0) %>%
    kable("markdown",3, caption="두 그룹의 차이검정")

  # diff.all

  # grp2

  res0 <-cbind.data.frame(grp1[,1:3],
                   G1= grp1[,4:8],
                   G2=grp2[,4:8])

  res1 <-res0 %>%kable("markdown",3,caption = caption)
 res=list(Group.Comparison = res1,
          Wald.test= diff,
          "diff.95%CI"= diff.all,
          group1=grp1,
          group2=grp2)
 res
}







effect_grp_diff0 <- function(x,caption=NULL, effect="Diff..DE"){
  # library(stringr)
  grp1 <- parameterEstimates(x, standardized = T) %>%
    filter(op=="~"& group==1) %>%
    mutate(stars=ifelse(pvalue<0.001,"***",
                        ifelse(pvalue<0.01,"**",
                               ifelse(pvalue<0.05,"*","")))) %>%
    mutate(op=ifelse(op=="~","<--","")) %>%
    dplyr::select(TO=lhs,Path=op,
                  FROM=rhs,
                  label,
                  est,
                  Sig=stars,
                  se,
                  # "β"= std.all,
                  # c.r=z,
                  Sig=stars,
                  "p"=pvalue
    )


  grp2<-parameterEstimates(x, standardized = T) %>%
    filter(op=="~"&group==2) %>%
    mutate(stars=ifelse(pvalue<0.001,"***",
                        ifelse(pvalue<0.01,"**",
                               ifelse(pvalue<0.05,"*","")))) %>%
    mutate(op=ifelse(op=="~","<--","")) %>%
    dplyr::select(TO=lhs,Path=op,
                  FROM=rhs,
                  label,
                  est,
                  Sig=stars,
                  se,
                  # "β"= std.all,
                  # c.r=z,
                  Sig=stars,
                  "p"=pvalue
    )

  diff <- parameterEstimates(x,standardized = TRUE,ci=TRUE ) %>%
    filter(op==":=") %>%
    filter(str_detect(lhs, effect) ) %>%
    mutate(Diff.sig=cut(pvalue,c(-Inf,0.001,0.01,0.05,1),
                        labels=c("***","**","*",""))) %>%
    dplyr::select(lhs,rhs, est,se, z, pvalue, Diff.sig) %>%arrange(lhs) %>%
    filter(rhs !=0) #%>%
    #kable("markdown",3,caption="두 그룹의 차이검정")

  diff.all <- parameterEstimates(x,standardized = TRUE,ci=TRUE ) %>%
    filter(op==":=") %>%
    filter(str_detect(lhs, effect) ) %>%
    mutate(Diff.sig=cut(pvalue,c(-Inf,0.001,0.01,0.05,1),
                        labels=c("***","**","*",""))) %>%
    dplyr::select(lhs,rhs, est,se, z, pvalue, Diff.sig) %>%arrange(lhs) %>%
    filter(rhs !=0)
    #kable("markdown",3,caption="두 그룹의 차이검정")


  # grp2

#   res0 <-cbind.data.frame(grp1[,1:3],
#                           G1= grp1[,4:8],
#                           G2=grp2[,4:8])
#
#   res1 <-res0 %>%kable("markdown",3,caption = caption)
#   res=list(Group.Comparison=res1, diff.test=diff, diff.all=diff.all)
#   res
# }
  diff.all
}




#직접효과 비교

#다집단그룹비교 데이터를 wide하게 하여 비교
#직접효과를 비교하는 방법 적용
effect_grp.DE.diff <- function(x,effect="Diff.DE", title=NULL){
  # library(stringr)
  if(is.null(x)){return("분석을 먼저 실행하시고 입력해주세요 ")}

  grp1<-parameterEstimates(x, standardized = T) %>%
    filter(op=="~"& group==1) %>%
    mutate(stars=ifelse(pvalue<0.001,"***",
                        ifelse(pvalue<0.01,"**",
                               ifelse(pvalue<0.05,"*","")))) %>%
    mutate(op=ifelse(op=="~","<--","")) %>%
    dplyr::select(TO=lhs,Path=op,
                  FROM=rhs,
                  label,
                  est,
                  Sig=stars,
                  se,
                  # "β"= std.all,
                  # c.r=z,
                  Sig=stars,
                  "p"=pvalue    )


  grp2<-parameterEstimates(x, standardized = T) %>%
    filter(op=="~"&group==2) %>%
    mutate(stars=ifelse(pvalue<0.001,"***",
                        ifelse(pvalue<0.01,"**",
                               ifelse(pvalue<0.05,"*","")))) %>%
    mutate(op=ifelse(op=="~","<--","")) %>%
    dplyr::select(TO=lhs,Path=op,
                  FROM=rhs,
                  label,
                  est,
                  Sig=stars,
                  se,
                  # "β"= std.all,
                  # c.r=z,
                  Sig=stars,
                  "p"=pvalue    )

  diff <- parameterEstimates(x,standardized = TRUE,ci=TRUE ) %>%
    filter(op==":=") %>%
    filter(str_detect(lhs, effect) ) %>%
    mutate(Diff.sig=cut(pvalue,c(-Inf,0.001,0.01,0.05,1),
                        labels=c("***","**","*",""))) %>%
    dplyr::select(lhs,rhs, est,se, z, pvalue, Diff.sig) %>%arrange(lhs) %>%
    filter(rhs !=0) #%>%
    # kable("markdown",3,caption="두 그룹의 차이검정")


  res.r <- cbind.data.frame(Diff=substring(diff[,c(1)],12),
                            para=diff[,c(2)],
                            G1=grp1[,5:6],
                            G2=grp2[,5:6],
                            diff_est=diff[,c(3)],
                            sig=diff[,c(7)],
                            se=diff[,c(4)],
                            z=diff[,c(5)],
                            p=diff[,c(6)]
                            ) %>%
    kable("markdown", 3,caption = paste(title,"그룹별 직접효과 비교"))


  res.r



}




# grp.compareGraph <- function(x,var1="grp1",var2="grp2",yp=-0.5){
#   bargender <- x %>%effect_grp_d()
#   bargender <- bargender %>% mutate(name=paste(FROM,"->",TO))
#   bar1 <-bargender[,c(5,10)]
#   rownames(bar1)= bargender[,c("name")]
#
#
#   op=par( mar=c(8,3,1,0.5))
#   barp <-barplot(t(as.matrix(bar1)), beside = TRUE,
#                  col = c("gray35","orange"), las = 2, ylim = c(-0.9, 1.7),
#                  cex.names = 0.8, col.axis = "gray30", cex.axis = 0.8)
#   abline(h=0, col="gray50", lwd=2)
#   title(paste(" Path coeff. Comparion of", var1,"and",var2))
#   legend("top", legend=c(var1,var2),
#          col = c("orange","gray30"),ncol=2,bty="n",pch=22,border.col = "black",
#          pt.bg = c("orange","gray30"))
#   box()
#
#   sigcheck <- x%>% effect_grp_diff0()
#   text(x=barp[1,], y=yp,
#        labels=paste0("d= ",round(sigcheck[,7],2),cex=0.9))
#   text(x=barp[1,]+1.13, y=yp, labels= sigcheck[,8], col = "red", cex=2)
#
#   text(x=barp[1,], y=bar1[,1]+0.2, labels =round(bar1[,1],2),cex=.8,
#        col=" darkgreen")
#   text(x=barp[2,], y=bar1[,2]+0.2, labels = round(bar1[,2],2),cex=.8,
#        col="tomato")
#
#   par(op)
#
# }

grp.compareGraph <- function(x,var1="grp1",var2="grp2",
                             yp=-0.5, yp1=.08, digit=2,
                             ylim = c(-0.7, 2),las=2){
  bargender <- x %>%effect_grp_d()
  bargender <- bargender %>% mutate(name=paste(FROM,"->",TO))
  bar1 <-bargender[,c(5,10)]
  rownames(bar1)= bargender[,c("name")]


  op=par( mar=c(6,3,2,1))
  barp <-barplot(t(as.matrix(bar1)), beside = TRUE,
                 col = c("gray35","gray90"), las = las, ylim = ylim,
                 cex.names = 0.8, col.axis = "gray30", cex.axis = 0.8)
  abline(h=0, col="gray20", lwd=2)
  title(paste(" Path coeff. Comparion of", var1,"and",var2))
  legend("top", legend=c(var1,var2),
         col = c("gray10","gray10"),ncol=2,bty="n",pch=22,border.col = "black",
         pt.bg = c("gray35","gray90"))
  box()
  #경로차이
  sigcheck <- x%>% effect_grp_diff0()

  # text(x=barp[1,]+0.5, y=ifelse(yp<0, yp+0.1,yp-0.1),"||")

  text(x=barp[1,]+0.5, y=yp,
       labels= paste("d=",round(sigcheck[,3],digit),""),cex=  .8 )

         #별표시 하기
   text(x=barp[1,]+0.8,
       y=ifelse(yp>0,yp-0.08,yp+0.08),
       labels= sigcheck[,7], col = "red", cex=1.5)

  #그룹별계수;막대그래프 위에 붙이기
  #왼쪽
  text(x=barp[1,]-0.25,
       y=ifelse(bar1[,1]>0, bar1[,1]+yp1, bar1[,1]-yp1),
       labels =round(bar1[,1],digit),cex=.8,
       col=" darkgreen")
  #오른쪽
  text(x=barp[2,]+0.2,
       y=ifelse(bar1[,2]>0, bar1[,2]+yp1, bar1[,2]-yp1),   #yp1 막대위 간격
       labels = round(bar1[,2],digit),cex=.8,
       col="gray25")

  par(op)

}










effect_grp_p <- function(x){

  grp1<-parameterEstimates(x, standardized = T) %>%
    filter(op=="~"& group==1) %>%
    mutate(stars=ifelse(pvalue<0.001,"***",
                        ifelse(pvalue<0.01,"**",
                               ifelse(pvalue<0.05,"*","")))) %>%
    mutate(op=ifelse(op=="~","<--","")) %>%
    dplyr::select(TO=lhs,Path=op,
                  FROM=rhs,
                  # group,
                  est,
                  se,
                  "β"= std.all,
                  # c.r=z,
                  Sig=stars,
                  "p"=pvalue
    )


  grp2<-parameterEstimates(x, standardized = T) %>%
    filter(op=="~"&group==2) %>%
    mutate(stars=ifelse(pvalue<0.001,"***",
                        ifelse(pvalue<0.01,"**",
                               ifelse(pvalue<0.05,"*","")))) %>%
    mutate(op=ifelse(op=="~","<--","")) %>%
    dplyr::select(TO=lhs,Path=op,
                  FROM=rhs,
                  # group,
                  est,
                  se,
                  "β"= std.all,
                  # c.r=z,
                  Sig=stars,
                  "p"=pvalue    )

  # grp2

  cbind.data.frame(grp1[,1:3],
                   G1= grp1[,4:7],
                   G2=grp2[,4:7]) %>%
    kable("markdown",3)


}

# 다집단 비교자료를 데이터 프레임으로 제작
effect_grp_d <- function(x,caption=NULL){

  grp1<-parameterEstimates(x, standardized = T) %>%
    filter(op=="~"& group==1) %>%
    mutate(stars=ifelse(pvalue<0.001,"***",
                        ifelse(pvalue<0.01,"**",
                               ifelse(pvalue<0.05,"*","")))) %>%
    mutate(op=ifelse(op=="~","<--","")) %>%
    dplyr::select(TO=lhs,Path=op,
                  FROM=rhs,
                  label,
                  est,
                  se,
                  "β"= std.all,
                  # c.r=z,
                  Sig=stars,
                  "p"=pvalue
    )


  grp2<-parameterEstimates(x, standardized = T) %>%
    filter(op=="~"&group==2) %>%
    mutate(stars=ifelse(pvalue<0.001,"***",
                        ifelse(pvalue<0.01,"**",
                               ifelse(pvalue<0.05,"*","")))) %>%
    mutate(op=ifelse(op=="~","<--","")) %>%
    dplyr::select(TO=lhs,Path=op,
                  FROM=rhs,
                  label,
                  est,
                  se,
                  "β"= std.all,
                  # c.r=z,
                  Sig=stars,
                  "p"=pvalue
    )

  # diff <- parameterEstimates(x,standardized = TRUE,ci=TRUE ) %>%
  #   filter(op==":=") %>%
  #   filter(str_detect(lhs, "Diff..DE") ) %>%
  #   mutate(Diff.sig=cut(pvalue,c(-Inf,0.001,0.01,0.05,1),
  #                       labels=c("***","**","*",""))) %>%
  #   dplyr::select(lhs,rhs, est,se, z, pvalue, Diff.sig) %>%
  #   kable("markdown",3,caption="두 그룹의 차이검정")
  #




  # grp2

  res0 <-cbind.data.frame(grp1[,1:3],
                          G1= grp1[,4:8],
                          G2=grp2[,4:8])

  # res1 <-res0%>%kable("markdown",3,caption = caption)
  # res=list(Group.Comparison=res1, diff.test=diff)
  res0
}



effect_grp.DE.diff0  <- function(x,caption=NULL, effect="Diff..DE"){
  # library(stringr)
  grp1<-parameterEstimates(x, standardized = T) %>%
    filter(op=="~"& group==1) %>%
    mutate(stars=ifelse(pvalue<0.001,"***",
                        ifelse(pvalue<0.01,"**",
                               ifelse(pvalue<0.05,"*","")))) %>%
    mutate(op=ifelse(op=="~","<--","")) %>%
    dplyr::select(TO=lhs,Path=op,
                  FROM=rhs,
                  label,
                  est,
                  Sig=stars,
                  se,
                  # "β"= std.all,
                  # c.r=z,
                  Sig=stars,
                  "p"=pvalue    )


  grp2<-parameterEstimates(x, standardized = T) %>%
    filter(op=="~"&group==2) %>%
    mutate(stars=ifelse(pvalue<0.001,"***",
                        ifelse(pvalue<0.01,"**",
                               ifelse(pvalue<0.05,"*","")))) %>%
    mutate(op=ifelse(op=="~","<--","")) %>%
    dplyr::select(TO=lhs,Path=op,
                  FROM=rhs,
                  label,
                  est,
                  Sig=stars,
                  se,
                  # "β"= std.all,
                  # c.r=z,
                  Sig=stars,
                  "p"=pvalue    )

  diff <- parameterEstimates(x,standardized = TRUE,ci=TRUE ) %>%
    filter(op==":=") %>%
    filter(str_detect(lhs, effect) ) %>%
    mutate(Diff.sig=cut(pvalue,c(-Inf,0.001,0.01,0.05,1),
                        labels=c("***","**","*",""))) %>%
    dplyr::select(lhs,rhs, est,se, z, pvalue, Diff.sig) %>%arrange(lhs) %>%
    filter(rhs !=0) #%>%
  # kable("markdown",3,caption="두 그룹의 차이검정")


  res.r <- cbind.data.frame(Diff=substring(diff[,c(1)],12),
                            para=diff[,c(2)],
                            G1=grp1[,5:6],
                            G2=grp2[,5:6],
                            diff_est=diff[,c(3)],
                            sig=diff[,c(7)],
                            se=diff[,c(4)],
                            z=diff[,c(5)],
                            p=diff[,c(6)]
  )

  res.r

}













#med efffect#####

med_effect_all <- function(x,type="table.d"){

  library(dplyr)

  tryCatch({
    switch(type,
           all = med_effect(x),
           table = med_effect_table(x),
           table.d =med_effect_table.d(x),
           group = effect_grp (x),
           group.p= effect_grp_p(x)
           )
  },error=function(e) {
    return("입력 오류입니다.
         정확한 명칭을 입력해주세요 ")})

}





#effect함수
Med_effect<- function(x, type="all",# 2: ci 출력
                        effect1="",
                       caption="",
                       effect2=""#,
                      # format="markdown"

                       ){

  # switch(type,
  #        all=para_all,
  #        ci =para_ci,
  #        pvalue= para_p)

  library(knitr)
  library(dplyr)
  library(stringr)



  para_all<- parameterEstimates(x,standardized = TRUE,ci=TRUE ) %>%
    filter(op==":=") %>%
      dplyr::filter(str_detect(lhs, effect1) & str_detect(lhs, effect2)) %>%
      mutate(sig=cut(pvalue,c(-Inf,0.001,0.01,0.05,1),
                     labels=c("***","**","*",""))) %>%
      dplyr::select(lhs,parameter=rhs, est,se,std=std.all, z,sig,pvalue, ci.lower, ci.upper) #%>%
      # kable(format, digits=3, caption = caption)

   para_ci <- parameterEstimates(x,standardized = T ) %>%
     dplyr::filter(op==":=") %>%
     dplyr::filter(str_detect(lhs,effect1) & str_detect(lhs,effect2) ) %>%
      mutate(sig=cut(pvalue,c(-Inf,0.001,0.01,0.05,1),
                     labels=c("***","**","*",""))) %>%
      dplyr::select(lhs,parameter=rhs,est,se,std=std.all, z,sig,pvalue, ci.lower, ci.upper)


   para_p <- parameterEstimates(x,standardized = T ) %>%
     dplyr::filter(op==":=") %>%
     dplyr::filter(str_detect(lhs,effect1) & str_detect(lhs,effect2) ) %>%
     mutate(sig=cut(pvalue,c(-Inf,0.001,0.01,0.05,1),
                    labels=c("***","**","*",""))) %>%
     dplyr::select(lhs, parameter=rhs,est,se,std=std.all, z,sig,pvalue)

   switch(type,
          all=para_all,
          ci =para_ci,
          pvalue= para_p)



}

#매개효과 추출기
# med_effect_table0 로 계산을 위해서 필요한 함수
med_effect <- function(x,
                      effect1="",
                      caption="",
                      effect2="." ,#,
                      option = 1# 2: ci 출력
                      # format="markdown"

){
  library(knitr)
  library(dplyr)
  library(stringr)

  if(option==1){  # p
    parameterEstimates(x, standardized = TRUE,ci=TRUE ) %>%
      dplyr::filter(op==":=") %>%
      dplyr::filter(str_detect(lhs, effect1) & str_detect(lhs,effect2)) %>%
      mutate(sig=cut(pvalue,c(-Inf,0.001,0.01,0.05,1),
                     labels=c("***","**","*",""))) %>%
      dplyr::select(lhs, est,se, z, pvalue, sig,std.all, Parameter=rhs) #%>%
    # kable(format, digits=3, caption = caption)
  }  else if(option==2){   #ci
    parameterEstimates(x,standardized = T ) %>%
      dplyr::filter(op==":=") %>%
      dplyr::filter(str_detect(lhs, effect1) & str_detect(lhs, effect2) ) %>%
      mutate(sig=cut(pvalue,c(-Inf,0.001,0.01,0.05,1),
                     labels=c("***","**","*",""))) %>%
      dplyr::select(lhs,est,se, z, pvalue,std.all, sig, ci.lower, ci.upper)
  }else{}

}


#
# #effect함수
# med_effect_ci<- function(x, effect1="",
#                       caption="",
#                       effect2="",
#                       option=1,
#                       format="markdown"
#
# ){
#   library(knitr)
#   library(dplyr)
#   library(stringr)
#   if(option==1){  # p
#     parameterEstimates(x,standardized = TRUE,ci=TRUE ) %>% filter(op==":=") %>%
#       filter(str_detect(lhs,effect1) & str_detect(lhs,effect2)) %>%
#       mutate(sig=cut(pvalue,c(-Inf,0.001,0.01,0.05,1),
#                      labels=c("***","**","*",""))) %>%
#       dplyr::select(lhs, est,se, #z, pvalue,
#              ci.lower, ci.upper,
#              sig,
#              std.all, rhs) #%>%
#     # kable(format, digits=3, caption = caption)
#   }
#   else
#   {if(option==2){   #ci
#     parameterEstimates(x,standardized = T ) %>% filter(op==":=") %>%
#       filter(str_detect(lhs,effect1) & str_detect(lhs,effect2) ) %>%
#       mutate(sig=cut(pvalue,c(-Inf,0.001,0.01,0.05,1),
#                      labels=c("***","**","*",""))) %>%
#       dplyr::select(lhs,est,se, z, pvalue,std.all, sig, ci.lower, ci.upper)
#   }else{}
#   }
# }

#


#매개효과 :간접효과 분석 결과 보기
#coding시에 변수가 맞게 설정되어야 함.
med_effect_table <- function(x, namestart=1){


  De <-x %>% med_effect(effect1="DE", effect2 = ".",option=2)
  Ie <-x %>% med_effect(effect1="IE",effect2 = ".",option=2)
  Te <-x %>% med_effect(effect1="TE",effect2 = ".",option=2)



  if(nrow(De) == nrow(Te)){
   res0 <-cbind.data.frame(
     Relationships=substring(Te[,c(1)],namestart ), #앞부분 index제거
                         # 이름에 맞추어서 합치기
                   Total=Te[,c(2)], #계수
                   Direct=De[,c(2)], #계수
                   Ind=Ie[,c(2,7,8,9)] #계수, 유의성, 신뢰구간
                   )
 colnames(res0)=c("Relationships", "Total", "Direct",
                 "Indirect","sig", "ci.lower"," ci.upper")
   res0
  }else{return("코딩된 개수가 안맞습니다. ")}

  res=list(
    directEffect=De,
    indirectEffect=Ie,
    totalEffect=Te,
    Result= res0)
res
}



#coding시에 변수가 맞게 설정되어야 함.
#이것은 med_effect_table 의 기능을 포함 마크다운 설정
#필요한 것을 선택에서 뽑는 함수
med_effect_table.d <- function(x,
                               type="markdown",
                               title="",
                               namestart=1){

  De <-x %>% med_effect(effect1="DE", effect2 = ".",option=2)
  Ie <-x %>% med_effect(effect1="IE",effect2 = ".",option=2)
  Te <-x %>% med_effect(effect1="TE",effect2 = ".",option=2)

  if(nrow(De) == nrow(Te)){
   res0 <- cbind.data.frame(Relationships=
             str_replace_all(substring(Te[,c(1)], namestart ),"_"," -> "),
                             # Te[,c(1)],
                     Total=Te[,c(2,7)],
                     Direct=De[,c(2,7)],
                     Ind=Ie[,c(2,7,8,9)]
    )
   colnames(res0)=c("Relationships", "Total","T.sig","Direct","D.sig",
                   "Indirect","I.sig", "ci.lower"," ci.upper")

   res.mark<- res0%>% kable(format="markdown",3,
                           caption = paste0(title, "의 효과분해 결과 유의성"))

switch(type,
      data.frame = res,
      markdown = res.mark
      )

  }else{return("코딩된 개수가 안맞습니다. ")}

}



# 간접효과 보기기****
#bar_graph용-- 데이터가 잘 맞음
med_effect_table0 <- function(x){


  De <-x %>% med_effect(effect1="DE", effect2 = ".",option=2)
  Ie <-x %>% med_effect(effect1="IE", effect2 = ".",option=2)
  Te <-x %>% med_effect(effect1="TE", effect2 = ".",option=2)

  # 간접효과 arrange

  if(nrow(De) == nrow(Te)){
    res0 <-cbind.data.frame(
      Relationships=Te[,c(1)],
                     Total=Te[,c(2)],
                     Direct=De[,c(2)],
                     Ind=Ie[,c(2,7,8,9)]
    )



  }else{return("코딩된 개수가 안맞습니다. ")}

  # res=list(De,Ie,Te,res0)
  res0

}


#bar_graph용-- 잘 안맞음 : 사용하지 말것
med_effect_table0_grp <- function(x, namestart=1){


  De <-x %>% med_effect(effect1="DE", effect2 = ".",option=2)
  Ie <-x %>% med_effect(effect1="IE",effect2 = ".",option=2)
  Te <-x %>% med_effect(effect1="TE",effect2 = ".",option=2)

  # 간접효과 arrange
  Ie$lhs<- substring(Ie[,c(1)],namestart)
  Ie <-Ie %>% mutate(grp=substr(lhs,1,1))

  Ie <-Ie %>% arrange(grp)#%>% dplyr::select(lhs, est, se, z, pvalue, sig, std.all, Parameter)


  if(nrow(De) == nrow(Te)){
    cbind.data.frame(Relationships=Te[,c(1)],
                     Total=Te[,c(2)],
                     Direct=De[,c(2)],
                     Ind=Ie[,c(2,7,8,9)]
    )


  }else{return("코딩된 개수가 안맞습니다. ")}

}


#마크다운
med_effect_table.d_grp <- function(x,type="markdown",title="",
                               namestart=1){

  De <-x %>% med_effect(effect1="DE", effect2 = ".",option=2)
  Ie <-x %>% med_effect(effect1="IE",effect2 = ".",option=2)
  Te <-x %>% med_effect(effect1="TE",effect2 = ".",option=2)

  # 간접효과 arrange
  Ie$lhs<- substring(Ie[,c(1)],namestart)
  Ie <-Ie %>% mutate(grp=substr(lhs,1,1))

  Ie <-Ie %>% arrange(grp)#%>% dplyr::select(lhs, est, se, z, pvalue, sig, std.all, Parameter)




  if(nrow(De) == nrow(Te)){
    res <- cbind.data.frame(Relationships=
                              str_replace(substring(Te[,c(1)],namestart)
                                          ,"_"," -> "),
                            # Te[,c(1)],
                            Total=Te[,c(2,7)],
                            Direct=De[,c(2,7)],
                            Ind=Ie[,c(2,7,8,9)]
    )
    colnames(res)=c("Relationships", "Total","T.sig","Direct","D.sig",
                    "Indirect","I.sig", "ci.lower"," ci.upper")

    res.mark<- res%>% kable(format="markdown",3,
                            caption = paste(title, "효과분해 결과 유의성"))

    switch(type,
           data.frame=res,
           markdown=res.mark
    )

  }else{return("코딩된 개수가 안맞습니다. ")}

}



#효과분해 Barplot ######
#그룹간 변수를 할 때에는 namestart=7로 한다.
effect_bar <- function(x, type="indirect",
                       ylim = c(-1.3,1.6),
                       yd = -0.9, # text check
                       up1=0, size=1,las=1,beside=FALSE,label_cex=1,
                       namestart=0,
                       grpn1="",grpn2="",
                       grp_name1="",grp_name2="",
                       arrange=FALSE,
                       grp_x1=2,
                       grp_y1=1,
                       grp_x2=6,
                       grp_y2=1,
                       grp_cex=1.5){

  # library(dplyr)
  TotalEffect <- med_effect_table0(x)  # 함수의 값을 가져옴.
  # TotalEffect <- med_effect_table0_grp(x)

  tef <- TotalEffect[,c(3,4)]
  # rownames(tef)=substring(TotalEffect[,1],6)
  rownames(tef)=str_replace(substring(TotalEffect[,1],namestart),"_"," : ") #namestart=6

  tef1<-TotalEffect[, c(2,3,4,5)]
  rownames(tef1)=substring(TotalEffect[,1],namestart) # namestart=6, gruop=7

  op=par( mar=c(6,3,1,0.5))
# 효과 그래프 그리는 plot
  barg <- barplot(t(tef), border = TRUE, col=c("steelblue","Gold"),
                  beside=beside,
                  las= las ,
                  cex.names= size - 0.2,
                  cex.axis = 0.8,#beside=T,
                  legend=c("Direct effect","Indirect effect"),
                  args.legend = list(x="top",ncol=2, border=TRUE,
                                     bty="n",
                  title="Effect Decompose (Direct, Indirect, Total) Analysis"),
                  ylim = ylim
                  # ,names.arg = substring(TotalEffect$Effect,6)
  )

  #indirection
  # text(x=0,y=yi,"Indirect",col="tomato", cex=0.8)
#결과값 테이블
  # labeltext <- med_effect_table.d(x,"data.frame") #함수의 값을 가져옴
  labeltext <- med_effect_table.d_grp(x,"data.frame") #이게 정확함

  if(type=="indirect"){
    text(x=barg, y=ifelse((tef1$Ind.est+tef1$Direct)< 0,
                          (tef1$Ind.est+tef1$Direct)-0.1,
                          (abs(tef1$Ind.est+tef1$Direct))+.1),
         label=paste0("IE= ",round(tef1$Ind.est,3)), cex=1, col="gray10")

    #sig, star
    text(x=barg, y=ifelse((tef1$Ind.est+tef1$Direct)< 0,
                          (tef1$Ind.est+tef1$Direct)-0.2,
                          (abs(tef1$Ind.est+tef1$Direct))+.15),
         label=tef1$Ind.sig, cex=1.2, col="red") #star

  }else{

    if(type=="total"){
      text(x=barg, y=ifelse((tef1$Ind.est+tef1$Direct)< 0,
                            (tef1$Ind.est+tef1$Direct)-0.1,
                            (abs(tef1$Ind.est+tef1$Direct))+.1),
           label=paste0("TE= ",round(tef1$Total,3)), cex=1, col="gray10")

      #sig, star
      text(x=barg, y=ifelse((tef1$Ind.est+tef1$Direct)< 0,
                            (tef1$Ind.est+tef1$Direct)-0.2,
                            (abs(tef1$Ind.est+tef1$Direct))+.15),
           label=labeltext$T.sig, cex=1.2, col="red") #star
    }
    else{
      if(type=="direct"){
        text(x=barg, y=ifelse((tef1$Ind.est+tef1$Direct)< 0,
                              (tef1$Ind.est+tef1$Direct)-0.1,
                              (abs(tef1$Ind.est+tef1$Direct))+.1),
             label=paste0("DE= ",round(tef1$Direct,3)), cex=1, col="gray10")

        #sig, star
        text(x=barg, y=ifelse((tef1$Ind.est+tef1$Direct)<0,
                              (tef1$Ind.est+tef1$Direct)-0.2,
                              (abs(tef1$Ind.est+tef1$Direct))+.15),
             label=labeltext$D.sig, cex=1.2, col="red") #star
      }
      else{"type를 total, indirect, direct중 입력하세요"}
    }

  }


  # 그룹명 추가
  text(x= grp_x1, y= grp_y1, paste(grpn1,grp_name1),cex=grp_cex)
  text(x= grp_x2, y= grp_y2, paste(grpn2,grp_name2),cex=grp_cex)

  #direction
  # 변수명
  # if(show=TRUE){
  text(x=0.2, y=yd ," ",col="gray30",cex=label_cex) #yd: direct position
  text(x=barg, y=yd ,  #round(tef1$Ind.est+tef1$Direct,3)+.3,
       label=str_replace(substring(TotalEffect[,1],namestart),"_"," : "),
       cex= label_cex, col="gray5") #namestart=6, group인경우 7
  text(x=0.2, y=yd -0.1,"IE:",col="gray30",cex= label_cex) #yd: direct position
  text(x=barg, y=yd- 0.1,  #round(tef1$Ind.est+tef1$Direct,3)+.3,
       label=round(tef1$Ind.est,3), cex=label_cex, col="gray40")

  text(x=0.2, y=yd -0.2,"DE:",col="gray30",cex= label_cex) #yd: direct position
  text(x=barg, y=yd - 0.2,  #round(tef1$Ind.est+tef1$Direct,3)+.3,
       label=round(tef1$Direct,3), cex= label_cex, col="gray40")

  text(x=0.2, y=yd-0.3,"TE:",col="gray30",cex= label_cex) #yd: direct position
  text(x=barg, y=yd-0.3,  #round(tef1$Ind.est+tef1$Direct,3)+.3,
       label=round(tef1$Total,3), cex= label_cex, col="gray20")


  # ?text
  # grid()
  box()
  abline(h=0, lwd=1.5)
if(arrange==TRUE){
  labeltext<- arrange(Relationships)
}


  par(op)
  resr<-list(labeltext %>% kable("markdown",3), round(tef,4),
             TotalEffect)
  resr
}



#효과분해 Barplot ######
#그룹간 변수를 할 때에는 namestart=7로 한다.
effect_bar_grp <- function(x, type="indirect", ylim = c(-1.3,1.6), yd = -0.9,
                       up1=0, size=1,las=1,beside=FALSE,label_cex=1,
                       namestart=1,
                       grpn1="",grpn2="",
                       grp_name1="",grp_name2="",
                       grp_x1=2, grp_y1=1, grp_x2=6, grp_y2=1,
                       grp_cex=1.5,
                       arrange=FALSE,
                       caption="Effect Decompose (Direct, Indirect, Total) anlaysis"){

  # library(dplyr)
  # TotalEffect <- med_effect_table0_grp(x)  # 함수의 값을 가져옴.
  TotalEffect <- med_effect_table0(x)



  tef <- TotalEffect[,c(3,4)]
  # rownames(tef)=substring(TotalEffect[,1],6)
  rownames(tef)=str_replace(substring(TotalEffect[,1],namestart),"_"," -> ") #namestart=6

  tef1<-TotalEffect[,c(2,3,4,5)]
  rownames(tef1)=substring(TotalEffect[,1],namestart) # namestart=6, gruop=7

  op=par( mar=c(6,3,1,0.5))

  barg <- barplot(t(tef), border = TRUE, col=c("steelblue","Gold"),
                  beside=beside,
                  las= las ,
                  cex.names= size - 0.2,
                  cex.axis = 0.8,#beside=T,
                  legend=c("Direct effect","Indirect effect"),
                  args.legend = list(x="top",ncol=2, border=TRUE,
                                     bty="n",
                       title="Effect Decompose (Direct, Indirect, Total) Analysis"),
                  ylim = ylim
              # ,names.arg = substring(TotalEffect$Effect,6)
  )

  #indirection
  # text(x=0,y=yi,"Indirect",col="tomato", cex=0.8)

  labeltext <- med_effect_table.d_grp(x,"data.frame")
#함수의 값을 가져옴

  if(type=="indirect"){
    text(x=barg, y=ifelse((tef1$Ind.est+tef1$Direct)< 0,
                          (tef1$Ind.est+tef1$Direct)-0.1,
                          (abs(tef1$Ind.est+tef1$Direct))+.1),
         label=paste0("IE= ",round(tef1$Ind.est,3)), cex=1, col="gray10")

    #sig, star
    text(x=barg, y=ifelse((tef1$Ind.est+tef1$Direct)< 0,
                          (tef1$Ind.est+tef1$Direct)-0.2,
                          (abs(tef1$Ind.est+tef1$Direct))+.15),
         label=tef1$Ind.sig, cex=1.2, col="red") #star

  }else{

    if(type=="total"){
      text(x=barg, y=ifelse((tef1$Ind.est+tef1$Direct)< 0,
                            (tef1$Ind.est+tef1$Direct)-0.1,
                            (abs(tef1$Ind.est+tef1$Direct))+.1),
           label=paste0("TE= ",round(tef1$Total,3)), cex=1, col="gray10")

      #sig, star
      text(x=barg, y=ifelse((tef1$Ind.est+tef1$Direct)< 0,
                            (tef1$Ind.est+tef1$Direct)-0.2,
                            (abs(tef1$Ind.est+tef1$Direct))+.15),
           label=labeltext$T.sig, cex=1.2, col="red") #star
    }
    else{
      if(type=="direct"){
        text(x=barg, y=ifelse((tef1$Ind.est+tef1$Direct)< 0,
                              (tef1$Ind.est+tef1$Direct)-0.1,
                              (abs(tef1$Ind.est+tef1$Direct))+.1),
             label=paste0("DE= ",round(tef1$Direct,3)), cex=1, col="gray10")

        #sig, star
        text(x=barg, y=ifelse((tef1$Ind.est+tef1$Direct)<0,
                              (tef1$Ind.est+tef1$Direct)-0.2,
                              (abs(tef1$Ind.est+tef1$Direct))+.15),
             label=labeltext$D.sig, cex=1.2, col="red") #star
      }
      else{"type를 total, indirect, direct중 입력하세요"}
    }

  }


  # 그룹명 추가
  text(x= grp_x1, y= grp_y1, paste(grpn1,grp_name1),cex=grp_cex)
  text(x= grp_x2, y= grp_y2, paste(grpn2,grp_name2),cex=grp_cex)

  #direction
  # 변수명
  # if(show=TRUE){
  text(x=0.2, y=yd ," ",col="gray30",cex=label_cex) #yd: direct position
  text(x=barg, y=yd ,  #round(tef1$Ind.est+tef1$Direct,3)+.3,
       label=str_replace(substring(TotalEffect[,1],namestart),"_"," -> "),
       cex= label_cex, col="gray5") #namestart=6, group인경우 7
  text(x=0.2, y=yd -0.1,"IE:",col="gray30",cex= label_cex) #yd: direct position
  text(x=barg, y=yd- 0.1,  #round(tef1$Ind.est+tef1$Direct,3)+.3,
       label=round(tef1$Ind.est,3), cex=label_cex, col="gray40")

  text(x=0.2, y=yd -0.2,"DE:",col="gray30",cex= label_cex) #yd: direct position
  text(x=barg, y=yd - 0.2,  #round(tef1$Ind.est+tef1$Direct,3)+.3,
       label=round(tef1$Direct,3), cex= label_cex, col="gray40")

  text(x=0.2, y=yd-0.3,"TE:",col="gray30",cex= label_cex) #yd: direct position
  text(x=barg, y=yd-0.3,  #round(tef1$Ind.est+tef1$Direct,3)+.3,
       label=round(tef1$Total,3), cex= label_cex, col="gray20")


  # ?text
  # grid()
  box()
  abline(h=0, lwd=1.5)

  if(arrange==TRUE){
  labeltext<-labeltext %>% arrange(Relationships)
  }

  par(op)
  resr<-list(labeltext %>% kable("markdown",3, caption= caption))#, round(tef,4))
  resr
}



# #indirection
# text(x=0,y=yi,"Indirect",col="tomato", cex=0.8)
# text(x=barg, y=yi,label=round(tef1$Ind.est,3), cex=0.8, col="tomato")
# text(x=barg, y=ys,label=tef1$Ind.sig, cex=1, col="red")
# #direction
# text(x=0, y=yd,"Direct",col="gray30",cex=0.8)
# text(x=barg, y=yd,label=round(tef1$Direct,3), cex=0.8, col="gray30")

#그룹에서 비교하는 방법



#직접, 간접, 총효과
med_effect_grp <- function(x){


  De<-x %>% med_effect("diff","","DE")%>%
    dplyr::select(lhs,rhs, est, sig)# %>% kable("markdown",3)
  Ie<-x %>% med_effect("diff","","IE")%>%
    dplyr::select(lhs,rhs, est, sig)# %>% kable("markdown",3)
  Te<-x%>% med_effect("diff","","TE")%>%
    dplyr::select(lhs,rhs, est, sig)# %>% kable("markdown",3)

  if(nrow(De) == nrow(Te)){

    cbind.data.frame(Path=te[,1],
                     Direct=de[,3] ,
                     Indriect=ie[,3],
                     Total=te[,3],
                     sig=te[,4]
    ) %>%
      kable("markdown",3)
  }else{return("코딩된 개수가 안맞습니다. ")}

}



#다집단 그룹비교후 새로운 모수에 대한 비교분석
#그룹과 레이블을 모두 표시한 것
med_effect_simple <- function(x, effect1="",
                       caption="",
                       effect2="",
                        format="markdown"

){
  library(knitr)
  library(dplyr)
  library(stringr)

    parameterEstimates(x,standardized = T ) %>% filter(op==":=") %>%
      filter(str_detect(lhs,effect1) & str_detect(lhs,effect2)) %>%
      mutate(sig=cut(pvalue,c(-Inf,0.001,0.01,0.05,1),
                     labels=c("***","**","*",""))) %>%
      dplyr::select(lhs, rhs,  est,sig,pvalue, sig) %>%
      kable(format, digits=3, caption = caption)


}

#

# #모든결과 보기
# sats.sem2 %>% med_effect(effect = "E",1,effect2 = "")
#
# #indirect effect계산과정 없이 보기
# sats.sem2 %>% med_effect(effect = "E",1)
#
# # option ==1 table, option==2, data.frame()
# sats.sem2 %>% med_effect("DE", "직접효과 ",1)
# sats.sem2 %>% med_effect("IE", "간접효과 ")
# sats.sem2 %>% med_effect("TE", "총효과 ",2)




#형태 점검시
#두그룹을 분리했을 때 사용
Grp_split_lodings_diff <- function(M1,M2,op1="=~",digit=3,est1="1",est2="2"){
  library(knitr)
  library(dplyr)
  est1=est1
  est2=est2

  #simple compare
    para_0 <- parameterEstimates(M1) %>% dplyr::select(lhs, op,rhs)
    para_a<-parameterEstimates(M1) %>% dplyr::select(lhs, op,rhs,est,se)
    para_b<-parameterEstimates(M2) %>% dplyr::select(lhs, op,rhs, est,se)
    para_compare<-data.frame(para_0,
                             round(para_a[,4:5],digits = digit),
                             round(para_b[,4:5],digits = digit))
    colnames(para_compare)=c("lhs","op","rhs",
                             paste0("est.",est1),paste0("se.",est1),
                             paste0("est.",est2),paste0("se.",est2))
    # para_compare
    para_compare$diff_est<-round(para_a[,4]-para_b[,4], digits = digit)
    para_compare %>% filter(op==op1)
      # kable("pandoc",3,caption = "그룹별 모수추정치 비교")

}

#mesurement invariance####


#option 1 basic
# opption 2  label &dataframe
# option 3 add pvalue
#loadings처럼 한번에 처리한 경우 사용
# label
{
# "Group_effect_diff"   <- function(fit,
#                                         option=1,
#                                         op1="=~",
#                                         block1=1,
#                                         block2=2#,
#                                         # est1="1",est2="2"
#                                         ){
#
#   # Group_invariance_diff_inter
#
#   library(dplyr)
#  library(knitr)
#
#   if(option==1){
#     para_0<-parameterEstimates(fit) %>%
#       filter(block==block1) %>%
#       dplyr::select(lhs,op,rhs)
#
#     para_1<- parameterEstimates(fit) %>%
#       filter(block==block1) %>%
#       dplyr::select(lhs,op,rhs,"Grp1_Est"=est,"G1_se"=se,pvalue)
#     para_2 <- parameterEstimates(fit) %>%
#       filter(block==block2) %>%
#       dplyr::select(lhs,op,rhs, "Grp2_Est"=est,"G2_se"=se,pvalue)
#     # para_3 <- parameterEstimates(fit) %>%
#     #   filter(block==3) %>%
#     #   dplyr::select(lhs,op,rhs, "Grp2_Est"=est,"G2_se"=se,pvalue)
#     #
#     para_compare<-data.frame(para_0, para_1[,4:6],para_2[,4:6])
#
#     # para_compare3<-data.frame(para_0, para_1[,4:6],para_2[,4:6],para_2[,4:6])
#
#
#
#     # colnames(para_compare)=c("lhs","op","rhs",paste0("est.",est1),paste0("se.",est1),
#     #                              paste0("est.",est2),paste0("se.",est2))
#     # para_compare
#     para_compare$diff_est<-para_1[,4]-para_2[,4]
#
#     a1<- para_compare%>%  filter(op == op1) %>%
#       kable("markdown", 3, caption = "두 모수의 차이비교")
#
#
#   }else{ #1
#
#     if(option=="label"){
#       para_0<-parameterEstimates(fit) %>%
#         filter(block==block1) %>%
#         dplyr::select(lhs,op,rhs,label)
#       para_1<- parameterEstimates(fit) %>%
#         filter(block==block1) %>%
#         dplyr::select(lhs,op,rhs,"Grp1_Est"=est,"G1_se"=se)
#       para_2 <- parameterEstimates(fit) %>%
#         filter(block==block2) %>%
#         dplyr::select(lhs,op,rhs, "Grp2_Est"=est,"G2_se"=se)
#       para_compare<-data.frame(para_0,round(para_1[,4:5],3),round(para_2[,4:5],3))
#       # para_compare
#       para_compare$diff_est<-round(para_1[,4]-para_2[,4],3)
#
#
#       a1<-para_compare%>%
#         filter(op== op1)
#
#     }
#     else{#2
#
#
#       if(option==3){
#         para_0<-parameterEstimates(fit) %>%
#           filter(block==block1) %>%
#           dplyr::select(lhs,op,rhs)
#         para_1<- parameterEstimates(fit) %>%
#           filter(block==block1) %>%
#           dplyr::select(lhs,op,rhs,"Grp1_Est"=est,"G1_se"=se,pvalue)
#         para_2 <- parameterEstimates(fit) %>%
#           filter(block==block2) %>%
#           dplyr::select(lhs,op,rhs, "Grp2_Est"=est,"G2_se"=se, pvalue)
#         para_compare<-data.frame(para_0,round(para_1[,4:6],3),round(para_2[,4:6],3))
#
#         para_compare$diff_est<-round(para_1[,4]-para_2[,4],3)
#
#         a1<-para_compare%>%
#           filter(op== op1)
#       }else{
#         return("dplyr::select! option=1 or 2(label) or 3(pvalue), op1에 변수를 입력하세요, ")
#
#       }
#
#
#       }#else3
#     }#else2
#   # }#esle1
#
#
#   res=list(group=fit,loadings=a1)
#   res
# }#function
}

# 그룹변수내 차이
Group_effect_diff<-function(fit, type="grp2"){
  switch(type,
         grp2=Group_effect_diff_2grp(fit),
         grp3=Group_effect_diff_3grp(fit)
         )
}


#2 개의 그룹비교
Group_effect_diff_2grp   <- function(fit,op1="=~",
                                     format="markdown",
                                     digits=2,
                                     block1=1,
                                     block2=2){
  library(dplyr)
  library(knitr)

  para_0<-parameterEstimates(fit) %>%
      filter(block==block1) %>%
      dplyr::select(lhs,op,rhs)

    para_1<- parameterEstimates(fit) %>%
      filter(block==block1) %>%
      dplyr::select(lhs,op,rhs,"G1_Est"=est,"G1_se"=se,"p.1"=pvalue)

    para_2 <- parameterEstimates(fit) %>%
      filter(block==block2) %>%
      dplyr::select(lhs,op,rhs, "G2_Est"=est,"G2_se"=se,"p.2" =pvalue)


    para_compare<-data.frame(para_0, para_1[,4:6],para_2[,4:6])

     para_compare$diff_est<-para_1[,4]-para_2[,4]

    a1<- para_compare%>%  filter(op == op1) %>%
      kable(format = format, digits=digits,
            caption = "두 그룹의 loadings의 차이비교")
    a2<- para_compare%>%  filter(op == "~~") %>%
      kable(format = format, digits=digits,
            caption = "두 그룹의 variance의 차이비교")
    a3<- para_compare%>%  filter(op == "~") %>%
      kable(format = format, digits=digits,
            caption = "두 그룹의 regression의 차이비교")
    a4<- para_compare%>%  filter(op == "~1") %>%
      kable(format = format, digits=digits,
            caption = "두 그룹의 regression의 차이비교")

    res=list(group=fit, loadings=a1, latent_path=a2 ,
             regression=a3, intercept=a4)
    res
  }

#그룹이 3일 때,
Group_effect_diff_3grp   <- function(fit,
                                     format="markdown",
                                     digits=2,
                                     block1=1,
                                     block2=2,
                                     block3=3

                                     ){

  # Group_invariance_diff_inter

  library(dplyr)
  library(knitr)

    para_0<-parameterEstimates(fit) %>%
      filter(block==block1) %>%
      dplyr::select(lhs,op,rhs)


    para_1<- parameterEstimates(fit) %>%
      filter(block==block1) %>%
      dplyr::select(lhs,op,rhs,
             "g1_est" = est, #그룹이름름
             "se.1"= se,
             "p.1" = pvalue
      )

    para_2 <- parameterEstimates(fit) %>%
      filter(block==block2) %>%
      dplyr::select(lhs,op,rhs,
             "g2_est" = est, #그룹이름름
             "se.2"= se,
             "p.2"= pvalue
      )

    para_3 <- parameterEstimates(fit) %>%
      filter(block==block3) %>%
      dplyr::select(lhs,op,rhs,
             "g3_est" = est, #그룹이름름
             "se.3"  = se,
              "p.3" = pvalue
      )


    # para_1<- parameterEstimates(fit) %>%
    #   filter(block==block1) %>%
    #   dplyr::select(lhs,op,rhs,
    #          paste0(grp_name1, "_Est") =est, #그룹이름름
    #          paste0(grp_name1,"_se") = se,
    #          paste0(grp_name1,"_p") = pvalue
    #          )
    #
    # para_2 <- parameterEstimates(fit) %>%
    #   filter(block==block2) %>%
    #   dplyr::select(lhs,op,rhs,
    #          paste0(grp_name2,"_Est")=est, #그룹이름름
    #          paste0(grp_name2,"_se")=se,
    #          paste0(grp_name2,"_p")=pvalue
    #          )
    #
    # para_3 <- parameterEstimates(fit) %>%
    #   filter(block==block3) %>%
    #   dplyr::select(lhs,op,rhs,
    #          paste0(grp_name3,"_Est")=est, #그룹이름름
    #          paste0(grp_name3,"_se")=se,
    #          paste0(grp_name4,"_p")=pvalue
    #          )
    #


    # para_compare2 <-data.frame(para_0, para_1[,4:6],para_2[,4:6])

    para_compare3<-data.frame(para_0,
                              para_1[,c(4,6)],
                              para_2[,c(4,6)],
                              para_2[,c(4,6)])



    # colnames(para_compare)=c("lhs","op","rhs",paste0("est.",est1),paste0("se.",est1),
    #                              paste0("est.",est2),paste0("se.",est2))
    # para_compare
    # para_compare2$diff_1_2 <-para_1[,4]-para_2[,4]

    para_compare3$diff_1_2 <-para_1[,4]-para_2[,4]
    para_compare3$diff_1_3 <-para_1[,4]-para_3[,4]
    para_compare3$diff_2_3 <-para_2[,4]-para_3[,4]

    a1<- para_compare3 %>%  filter(op == "=~") %>%
      kable(format = format, digits=digits,
            caption = " 3개의 그룹의 loadings 차이비교")

    a2<- para_compare3 %>%  filter(op == "~~") %>%
      kable(format = format, digits=digits,
            caption = " 3개의 그룹의 variance 차이비교")


    a3<- para_compare3%>%  filter(op == "~") %>%
      kable(format = format, digits=digits,
            caption = " 3개의 그룹의 regression의 차이비교")

    a4<- para_compare3%>%  filter(op == "~1") %>%
      kable(format = format, digits=digits,
            caption = "두 그룹의 intercepts의 차이비교")

  res=list(group=fit,
           loadings=a1,
           latent_path=a2,
           Regression=a3,
           intercept=a4 )
  res


}#function


# 측정동등성 measurement Invarivane -------------------------------------------------------------

{# Measurementinvariance<- function(model, data, grp=""){
#   library(lavaan)
#   library(broom)
#   suppressPackageStartupMessages(library(broom))
#   library(dplyr)
#   library(knitr)
#   options(knitr.kable.NA = '')
#
#   # 형태동등성 가정
#   Configural <- sem(model, data=data , group = grp )
#   # 인자동등성
#   Loadings <- sem(model, data=data ,
#                   group = grp,group.equal=c("loadings"))
#   # 잔차동등성
#   Residuals <- sem(model, data=data ,
#                    group = grp,group.equal=c("loadings", "residuals"))
#   #분산,공분산 오차동등성
#   Variance <- sem(model, data=data ,
#                   group = grp,group.equal=c("loadings", "residuals",
#                                             'lv.variances',"lv.covariances"))
#   # 절편 평균 동등성
#   Intercept_means <- sem(model, data=data ,
#                     group = grp,group.equal=c("loadings", "residuals",
#                                               'lv.variances',"lv.covariances",
#                                               "means","intercepts"))
#   # anova
#   a1 <-lavTestLRT(Configural, Loadings, Residuals, Variance, Intercept_means) %>%
#     kable("markdown", Configural = 3, caption = paste("Chi-Squared Difference Test of ", grp))
#   # align = "lccrr")
#   a2 <-lavTestLRT(Configural, Loadings, Residuals, Variance, Intercept_means)
#   res <- list(paperTable = a1, check_invariance = a2)
#   res
#
# }
#

# MeasurementInvariance(ffm_cfa_model_invariance, data=pdh_data_f_multi, grp = "gender")
}


MeasurementInvariance <- function(model, data, grp=""){
  library(lavaan)
  library(broom)
  suppressPackageStartupMessages(library(broom))
  library(dplyr)
  library(knitr)
  options(knitr.kable.NA = '')

  # 형태동등성 가정
  Configural <- sem(model, data=data , group = grp )
  # 인자동등성
  Loadings <- sem(model, data=data ,
                  group = grp,group.equal=c("loadings"))

  # 절편 평균 동등성
  Intercepts <- sem(model, data=data ,
                    group = grp,group.equal=c("loadings","intercepts","means"))

  # 잔차동등성
  Residuals <- sem(model, data=data ,
                   group = grp,group.equal=c("loadings", "residuals",
                                             "intercepts","means"))
  #분산,공분산 오차동등성
  Lv_Variance <- sem(model, data=data ,
                  group = grp,group.equal=c("loadings", "residuals", "intercepts","means",
                                            'lv.variances',"lv.covariances"))


  # grp,group.equal=c("loadings", "residuals",
  #                   'lv.variances',"lv.covariances",
  #                   "means","intercepts")

  # anova
  a1 <- lavTestLRT(Configural, Loadings, Residuals, Lv_Variance, Intercepts ) %>%
    kable("markdown", digits = 3, caption =
            paste("Measurement invariance Testing by JH Park.
                   :: Chi-Squared Difference Test of ", grp,"\n",
                   "
                    Step 1: Configural invariance
                    Step 2: Factor loadaing invariance
                    Step 3: Intercepts and means invariance
                    Step 4: Resicuals(error) inavariance
                    Step 5: Latent Variance and Covariance
                    "
                  ))

  # align = "lccrr")
  a2 <-lavTestLRT(Configural, Loadings, Residuals, Lv_Variance, Intercepts )
  res<- list(paperTable =a1, check_invariance = a2)
  res

}



# 다집단 분석함수
# global:일반 분석
# groupsem: 그룹별 설정한 후
Compare_grp_effect <- function(global, groupsem,
                               digit =3, title="",g1="g1", g2="g2",
                               namestart=8   #group name extraction
){


  gen_global <- global %>% Med_effect(effect1 = "DE") %>% dplyr::select(est)

  #group a를 선별추출
  rowname <- groupsem %>% Med_effect(type="ci",effect1 = "DE_") %>%
    mutate(path=substring(lhs,namestart),grp=substring(parameter,3)) %>%
    filter(grp=="a") %>%
    dplyr::select(path)# %>% filter(grp=="a")


  grp1_para_0 <- groupsem %>% Med_effect(type="ci",effect1 = "DE_") %>%
    mutate(grp=substring(parameter,3)) %>% filter(grp=="a")
  #grup b를 선별 추출
  grp2_para_0 <- groupsem %>% Med_effect(type="ci",effect1 = "DE_") %>%
    mutate(grp=substring(parameter,3)) %>% filter(grp=="b")
  #차이검증 추출
  grp_diff_0 <- groupsem %>% Med_effect(type="ci",effect1 = "diff_D")



  # total_data <- rbind.data.frame(grp1_para,grp2_para,grp_diff, make.row.names = F)



  a1 <- grp_diff_0 %>% dplyr::select(parameter)
  a2 <- gen_global

  a3 <- grp1_para_0 %>% dplyr::select(est,sig)
  a4 <- grp2_para_0 %>% dplyr::select(est,sig)
  a5 <- grp_diff_0 %>% dplyr::select(est,se,z,pvalue,sig)


  groupDiff <- cbind(rowname, a1, a2, a3, a4, a5)
  groupDiff$path<-  str_replace(groupDiff[,1],"_"," -> ")

  # g1=""
  # g2=""

  colnames(groupDiff)=c("path","para","global",
                        paste0(g1,".est"),paste0(g1,".sig"),
                        paste0(g2,".est"),paste0(g2,".sig"),
                        "d.est","d.se","z","p","sig")

  #Multigroup table
    groupDE <-  rbind(grp1_para_0 %>% dplyr::select(grp, lhs, parameter, est, se,
                                           std, z, sig, pvalue, ci.lower,
                                           ci.upper),
                    grp2_para_0%>% dplyr::select(grp, lhs, parameter, est, se,
                                          std, z, sig, pvalue, ci.lower,
                                          ci.upper))
  groupDE$lhs <- str_replace(substring(groupDE[,2],namestart),"_"," -> ")

  colnames(groupDE)=c("그룹","경로","가설",
                        "비표준화계수","표준오차",
                        "표준화계수","z",
                        "sig","p","ci.lower","ci.upper")
  # groupDiff%>% kable("markdown", 3,caption = "다집단 비교 ")
  res<- list(groupDiff%>% kable("markdown",digits =digit ,
                                caption = paste("다집단 비교_Wald Test: ",
                                                title)),
             # group_a = grp1_para_0,
             # group_b = grp2_para_0,
             groupDE%>% kable("markdown",digits =digit ,
                              caption = paste("다집단 비교: ",
                                              title)),
             grp_DE_different = grp_diff_0
  )
  res
}



# Waldtest ---------------------------------------------------------------------------------
# 두개의 모수를 비교할 때
#두 집단의 간접효과를 비교할 때
# WaldTest<- function(fit, form=""){
#   library(lavaan)
#   lavTestWald(fit, constraints = form)
#   # "H1a-H1b==0"
# }
#
# lavaanWaldTest<- function(fit, form=""){
#   library(lavaan)
#   lavTestWald(fit, constraints = form)
  # "H1a-H1b==0"
# }

# Daigram  ---------------------------------------------------------------------------------



semPaths.sig<- function(x, what="std"){
  switch (what,
          std  = semPaths.std(x),
          std.all = semPaths.std_T(x),
          p = semPaths.std.p_F (x),
          p.all= semPaths.std_p(x),
          str  = semPaths_str(x),
          str.p  = semPaths_str.p(x),
          est  = semPaths.est(x)
  )
}


# Diagarm조절한 함수######
semPaths.std_T<- function(x,
                        structural = F,
                        what="std",
                        fade=F,rotation=2,

                        edge.label.cex = 1.2,
                        edge.label.position=0.6,
                        asize=1.5,
                        label.cex=2,
                        nCharNodes =10,

                        layout="tree2",
                        curve = 1.1,
                        curveAdjacent = "<->",
                        curvature=F,
                        curvePivot=F,

                        sizeLat =5,sizeLat2 = 2,
                        sizeMan =4,sizeMan2 = 2,


                        residuals=T,
                        residScale = 12,
                        intercepts = F,
                        exoCov = T ,
                        exoVar = T,
                        nDigits = 3,
                        layoutSplit = F,
                        subRes = 2,#latent
                        subScale = 0.6,
                        subScale2 = 0.08,

                        mar = c(4,3,4,4),
                        vTrans=255,
                        border.width=2,
                        edgeLabels = b,   # star
                        cut=NULL,
                        posCol="gray10"
)
{
  library(semPlot)
  library(dplyr)

  tryCatch({


    table2 <-parameterEstimates(x, standardized = T) %>%
      filter(op=="=~"|op=="~"|op=="~~")%>%
      mutate(sig=ifelse(pvalue < 0.001, "***",
                        ifelse(pvalue < 0.01, "**",
                               ifelse(pvalue < 0.05, "*", ""))))%>%
      mutate(Sig=ifelse(op=="=~","",
                        ifelse(op=="~~"&lhs != rhs, sig ,
                               ifelse(op=="~~","",
                                      ifelse(op=="~", sig, "")))))


    # b<-gettextf('%.3f \n p=%.3f', table2$std.all, digits=table2$pvalue)
    b<-gettextf('%.3f %s', table2$std.all, digits=table2$Sig)



    semPaths(x,what=what, fade=fade, style="lisrel",
             layout = layout , rotation = rotation,


             layoutSplit = layoutSplit,
             subRes = subRes,#latent
             subScale = subScale, #width
             subScale2 = subScale2, #height


             curve = curve,
             curveAdjacent = curveAdjacent, # reg or ->
             curvature = curvature,
             curvePivot=curvePivot,
             residScale = residScale,
             residuals = residuals, #관측변수의 분산 감추기
             intercepts = intercepts ,
             vTrans=vTrans,cut=cut,
             posCol=posCol,

             sizeLat =sizeLat,sizeLat2=sizeLat2,
             sizeMan = sizeMan,sizeMan2 = sizeMan2,
             edge.label.cex = edge.label.cex,
             edge.label.position=edge.label.position,
             asize=asize,

             edgeLabels =  edgeLabels ,  # pavalue


             nCharNodes =nCharNodes ,
             label.cex=label.cex,

             border.width= border.width,
             groups="latents", pastel = T,
             exoCov = exoCov ,exoVar = exoVar,
             nDigits = nDigits,
             mar = mar,
             structural = structural)

    effect3(x)}, error=function(e)return("정확한 인수로 입력하세요.what =??? "))
}



#계수나타내기
EdgeLabels<-function(x, type="est"){
  switch(type,
         std=EdgeLabels.std.p (x),
         est=EdgeLabels.est.p(x),
         estall=EdgeLabels.est.p(x),
         star=EdgeLabels.star(x)

  )
}



#표준화 계수 pvalue
EdgeLabels.std.p <-function(x){
  library(dplyr)
  table2 <-parameterEstimates(x, standardized = T) %>%
    filter(op=="=~"|op=="~"|op=="~~") %>%
    mutate(sig=ifelse(pvalue < 0.001, "***",
                      ifelse(pvalue < 0.01, "**",
                             ifelse(pvalue < 0.05, "*", ""))))%>%
    mutate(Sig=ifelse(op=="=~","",
                      ifelse(op=="~~"&lhs != rhs, sig ,
                             ifelse(op=="~~","",
                                    ifelse(op=="~", sig, "")))))

  b<-gettextf('%.3f \n p=%.2f', table2$std.all,table2$pvalue)
  # b<-gettextf('%.3f %s', table2$std.all, digits=table2$sig)

  b
}


#비표준화 계수 pvalue
EdgeLabels.est.p <-function(x){
  library(dplyr)
  table2 <-parameterEstimates(x, standardized = T) %>%
    filter(op=="=~"|op=="~"|op=="~~") %>%
    mutate(sig=ifelse(pvalue < 0.001, "***",
                      ifelse(pvalue < 0.01, "**",
                             ifelse(pvalue < 0.05, "*", ""))))%>%
    mutate(Sig=ifelse(op=="=~","",
                      ifelse(op=="~~"&lhs != rhs, sig ,
                             ifelse(op=="~~","",
                                    ifelse(op=="~", sig, "")))))

  b<-gettextf('%.3f \n p=%.2f', table2$est,table2$pvalue)
  # b<-gettextf('%.3f %s', table2$std.all, digits=table2$sig)

  b
}

EdgeLabels.est.p.all <-function(x){
  library(dplyr)
  table2 <-parameterEstimates(x, standardized = T) %>%
    filter(op=="=~"|op=="~"|op=="~~") %>%
    mutate(sig=ifelse(pvalue < 0.001, "***",
                      ifelse(pvalue < 0.01, "**",
                             ifelse(pvalue < 0.05, "*", ""))))#%>%
    # mutate(Sig=ifelse(op=="=~","",
    #                   ifelse(op=="~~"& lhs == rhs, sig ,
    #                          ifelse(op=="~~",sig,
    #                                 ifelse(op=="~", sig, "")))))

  b<-gettextf('%.3f \n p=%.2f', table2$est,table2$pvalue)
  # b<-gettextf('%.3f %s', table2$std.all, digits=table2$sig)

  b
}


#구조모형에 사용할 유의성 표시
EdgeLabels.star_structural <-function(x, type="std"){
  library(dplyr)

  table2 <-parameterEstimates(x, standardized = T) %>%
    filter(op=="~"|op=="~~"&lhs!=rhs) %>%
    mutate(sig=ifelse(pvalue < 0.001, "***",
                      ifelse(pvalue < 0.01, "**",
                             ifelse(pvalue < 0.05, "*", ""))))%>%
    mutate(Sig=ifelse(op=="=~","",
                      ifelse(op=="~~"&lhs != rhs, sig ,
                             ifelse(op=="~~","",
                                    ifelse(op=="~", sig, "")))))


  # b<-gettextf('%.3f \n p=%.3f', table2$std.all, digits=table2$pvalue)
  table2$sig <- ifelse(is.na(table2$sig),"(1)",table2$sig)
  b<-gettextf('%.3f %s', table2$std.all, table2$Sig)
  b.all<-gettextf('%.3f %s', table2$std.all, table2$sig)
  c<-gettextf('%.3f %s', table2$est, table2$Sig)
  c.all<-gettextf('%.3f %s', table2$est, table2$sig)


  switch(type,
         std=b,
         std.all=b.all,
         est=c,
         est.all=c.all)

}





# 유의성 표시를  * 로 하는 인덱스 만들기
EdgeLabels.star<-function(x, type="std"){
  library(dplyr)

  table2 <-parameterEstimates(x, standardized = T) %>%
    filter(op=="=~"|op=="~"|op=="~~"&lhs!=rhs) %>%
    mutate(sig=ifelse(pvalue < 0.001, "***",
                      ifelse(pvalue < 0.01, "**",
                             ifelse(pvalue < 0.05, "*", ""))))%>%
    mutate(Sig=ifelse(op=="=~","",
                      ifelse(op=="~~"&lhs != rhs, sig ,
                             ifelse(op=="~~","",
                                    ifelse(op=="~", sig, "")))))


  # b<-gettextf('%.3f \n p=%.3f', table2$std.all, digits=table2$pvalue)
  table2$sig <- ifelse(is.na(table2$sig),"(1)",table2$sig)
  b<-gettextf('%.3f %s', table2$std.all, table2$Sig)
  b.all<-gettextf('%.3f %s', table2$std.all, table2$sig)
  c<-gettextf('%.3f %s', table2$est, table2$Sig)
  c.all<-gettextf('%.3f %s', table2$est, table2$sig)


  switch(type,
         std=b,
         std.all=b.all,
         est=c,
         est.all=c.all)

  switch(type,
         std=b,
         est=c)


}


# 유의성 표시를  * 로모든 변수에 다 붙이기
# edgeLabels = EdgeLabels.star_all(ffm_sem_1, "std"),#계수 직접 조절
# residuals = T, #이것을 이용하여 조절함
# 구조모형내에 edgeLabels에 값을 직접 설정하여 구현함

EdgeLabels.star_all <-function(x, type="std"){
  library(dplyr)

  table2 <-parameterEstimates(x, standardized = T) %>%
    filter(op=="=~"|op=="~"|op=="~~"#&lhs!=rhs
           ) %>%
    mutate(sig=ifelse(pvalue < 0.001, "***",
                      ifelse(pvalue < 0.01, "**",
                             ifelse(pvalue < 0.05, "*", ""))))%>%
    mutate(Sig=ifelse(op=="=~","",
                      ifelse(op=="~~"&lhs != rhs, sig ,
                             ifelse(op=="~~","",
                                    ifelse(op=="~", sig, "")))))

  table3 <-parameterEstimates(x, standardized = T) %>%
    filter(op =="=~"|op =="~"|op=="~~"&lhs!=rhs
    ) %>%
    mutate(sig=ifelse(pvalue < 0.001, "***",
                      ifelse(pvalue < 0.01, "**",
                             ifelse(pvalue < 0.05, "*", ""))))%>%
    mutate(Sig=ifelse(op=="=~","",
                      ifelse(op=="~~"&lhs != rhs, sig ,
                             ifelse(op=="~~","",
                                    ifelse(op=="~", sig, "")))))

  # b<-gettextf('%.3f \n p=%.3f', table2$std.all, digits=table2$pvalue)
  table2$sig <- ifelse(is.na(table2$sig),"(1)",table2$sig)
  # table3$pvalue <- ifelse(is.na(table3$pvalue),"",table3$pvalue)

  b <-gettextf('%.3f %s', table2$std.all, table2$Sig)    #lhs ==rhs
  b.std <-gettextf('%.3f %s', table3$std.all, table3$Sig) #lhs ~=rhs

  b.all<-gettextf('%.3f %s', table2$std.all, table2$sig)
  b.all.2f <-gettextf('%.2f %s', table2$std.all, table2$sig)

  c<-gettextf('%.3f %s', table2$est, table2$Sig)
  c.all<-gettextf('%.3f %s', table2$est, table2$sig)
  c.all.2f<-gettextf('%.2f %s', table2$est, table2$sig)

  std.p<-gettextf('%.2f \n (p=%.2f)', table3$std.all,table3$pvalue)
  est.p<-gettextf('%.2f \n (p=%.2f)', table3$est,table3$pvalue)


  switch(type,
         std.str= b.std, #residuals=F이어야 함.
         std= b,
         std.all= b.all,
         std.all.2f = b.all.2f,
         std.p=std.p,

         est=c,
         est.all=c.all,
         est.al.2fl=c.all.2f,
         est.p=est.p

         )


}


#유의성데이터 만드는 함수 : 회귀분석에서 사용
EdgeLaels.lm.p<- function(x){
  library(broom)
  data <-x%>% tidy() %>% as.data.frame()

  b<-gettextf('%.3f \n p=%.2f', data$estimate, data$p.value)  #sprintf()

  b
}

#회귀분석 유의성함수 * sig 넣기
EdgeLaels.lm.sig<- function(x){
  library(broom)
  data <-x%>% tidy() %>% as.data.frame()

  table2 <- data%>%  mutate(sig=ifelse(p.value < 0.001, "***",
                                       ifelse(p.value < 0.01, "**",
                                              ifelse(p.value < 0.05, "*", ""))))

  b<-gettextf('%.3f %s', table2$estimate, table2$sig)
  # table2
  b

}



semPaths.std<- function(x,
                        structural = F,
                        what="std",
                        whatLabels="std",
                        fade=F,
                        rotation=2,

                        edge.label.cex = 1.2,
                        edge.label.position=0.6,
                        asize=1,
                        label.cex=1,
                        nCharNodes =10,
                        edge.color=NULL,
                        edge.width=NULL,

                        layout="tree2",
                        curve = 1.1,
                        curveAdjacent = "<->",
                        curvature=F,
                        curvePivot=F,

                        sizeLat =8,sizeLat2 = 1.4,
                        sizeMan =6,sizeMan2 = 1.2,


                        residuals=F,
                        residScale = 10,
                        intercepts = F,
                        exoCov = T ,
                        exoVar = F,
                        nDigits = 3,
                        layoutSplit = F,
                        subRes = 2,#latent
                        subScale = 0.5,
                        subScale2 = 0.08,

                        mar = c(1,2,1,2),
                        vTrans=255,
                        border.width=2,
                        edgeLabels =b,   #라벨 사용
                        cut=NULL,
                        posCol = "gray20"
)
{
  library(semPlot)
  library(dplyr)

  tryCatch({


    table2 <-parameterEstimates(x, standardized = T) %>%
      filter(op=="=~"|op=="~"|op=="~~"&lhs!=rhs) %>%
      mutate(sig=ifelse(pvalue < 0.001, "***",
                        ifelse(pvalue < 0.01, "**",
                               ifelse(pvalue < 0.05, "*", ""))))%>%
      mutate(Sig=ifelse(op=="=~","",
                        ifelse(op=="~~"&lhs != rhs, sig ,
                               ifelse(op=="~~","",
                                      ifelse(op=="~", sig, "")))))


    # b<-gettextf('%.3f \n p=%.3f', table2$std.all, digits=table2$pvalue)
    b<-gettextf('%.3f %s', table2$std.all, table2$Sig)



    semPaths(x,what=what, fade=fade,
             whatLabels = whatLabels,
             style="lisrel",
             layout = layout , rotation = rotation,


             layoutSplit = layoutSplit,
             subRes = subRes,#latent
             subScale = subScale, #width
             subScale2 = subScale2, #height


             curve = curve,
             curveAdjacent = curveAdjacent, # reg or ->
             curvature = curvature,
             curvePivot= curvePivot,
             residScale = residScale,
             residuals = residuals, #관측변수의 분산 감추기
             intercepts = intercepts ,
             vTrans=vTrans,
             cut=cut,


             sizeLat =sizeLat,sizeLat2=sizeLat2,
             sizeMan = sizeMan,sizeMan2 = sizeMan2,
             edge.label.cex = edge.label.cex,
             edge.label.position=edge.label.position,
             edge.color = edge.color,
             asize=asize,
             edge.width=edge.width,

             edgeLabels =  edgeLabels ,  # pavalue
             posCol = posCol,

             nCharNodes =nCharNodes ,
             label.cex=label.cex,

             border.width= border.width,
             groups="latents", pastel = T,
             exoCov = exoCov ,exoVar = exoVar,
             nDigits = nDigits,
             mar = mar,
             posCol=c("gray30","black"),
             structural = structural)

    effect3(x)}, error=function(e)return("정확한 인수로 입력하세요.what =??? "))
}


#모든 p값 출력 whatLabels
semPaths.std.p <- function(x,
                        structural = F,
                        whatLabels="std",
                        fade=F,rotation=2,

                        edge.label.cex = 1,
                        edge.label.position=0.55,
                        asize=1.5,
                        label.cex=2,
                        nCharNodes =10,
                        edge.color=NULL,

                        layout="tree2",
                        curve = 1.1,
                        curveAdjacent = "<->",
                        curvature=F,
                        curvePivot=F,

                        sizeLat =5,sizeLat2 = 2,
                        sizeMan =4,sizeMan2 = 2,


                        residuals=T,
                        residScale = 12,
                        intercepts = F,
                        exoCov = T ,exoVar = T,
                        nDigits = 3,
                        layoutSplit = F,
                        subRes = 2,#latent
                        subScale = 0.5,
                        subScale2 = 0.08,

                        mar = c(2,3,2,3),
                        vTrans=255,
                        border.width=2,
                        edgeLabels =b,  #계수 설정
                        cut=NULL,
                        posCol = "gray10"
)
{
  library(semPlot)
  library(dplyr)

  tryCatch({


    table2 <-parameterEstimates(x, standardized = T) %>%
      filter(op=="=~"|op=="~"|op=="~~") %>%
      mutate(sig=ifelse(pvalue < 0.001, "***",
                        ifelse(pvalue < 0.01, "**",
                               ifelse(pvalue < 0.05, "*", ""))))%>%
      mutate(Sig=ifelse(op=="=~","",
                        ifelse(op=="~~"&lhs != rhs, sig ,
                               ifelse(op=="~~","",
                                      ifelse(op=="~", sig, "")))))
    #option dplyr::selection

    b<-gettextf('%.3f \n p=%.2f', table2$std.all,table2$pvalue)




    semPaths(x,whatLabels = whatLabels, fade=fade, style="lisrel",
             layout = layout , rotation = rotation,

             layoutSplit = layoutSplit,
             subRes = subRes,#latent
             subScale = subScale, #width
             subScale2 = subScale2, #height


             curve = curve,
             curveAdjacent = curveAdjacent, # reg or ->
             curvature = curvature,
             curvePivot=curvePivot,
             residScale = residScale,
             residuals = residuals, #관측변수의 분산 감추기
             intercepts = intercepts ,
             vTrans=vTrans,cut=cut,

             sizeLat =sizeLat,sizeLat2=sizeLat2,
             sizeMan = sizeMan,sizeMan2 = sizeMan2,
             edge.label.cex = edge.label.cex,
             edge.label.position=edge.label.position,
             asize=asize,

             edgeLabels =  edgeLabels ,  # pavalue
             posCol = posCol,

             nCharNodes =nCharNodes ,
             label.cex=label.cex,
             edge.color=edge.color,
             border.width= border.width,
             groups="latents", pastel = T,
             exoCov = exoCov ,exoVar = exoVar,
             nDigits = nDigits,
             mar = mar,
             structural = structural)

    effect3(x)}, error=function(e)return("정확한 인수로 입력하세요.what =??? "))
}


#모든 p값 출력
semPaths.std.p_F <- function(x,
                           structural = F,
                           what="std",
                           fade=F,rotation=2,

                           edge.label.cex = 1.1,
                           edge.label.position=0.6,
                           asize=1.2,
                           label.cex=2.2,
                           nCharNodes =10,

                           layout="tree2",
                           curve = 1.1,
                           curveAdjacent = "<->",
                           curvature=F,
                           curvePivot=F,

                           sizeLat =5,sizeLat2 = 1,
                           sizeMan =4,sizeMan2 = 1,


                           residuals=F,
                           residScale = 12,
                           intercepts = F,
                           exoCov = T ,exoVar = T,
                           nDigits = 3,
                           layoutSplit = F,
                           subRes = 2,#latent
                           subScale = 0.5,
                           subScale2 = 0.08,

                           mar = c(1,3,2,1),
                           vTrans=255,
                           border.width=2,
                           edgeLabels =b,
                           cut=NULL,
                           posCol = "gray10"
)
{
  library(semPlot)
  library(dplyr)

  tryCatch({


    table2 <-parameterEstimates(x, standardized = T) %>%
      filter(op=="=~"|op=="~"|op=="~~"&lhs !=rhs) %>%
      mutate(sig=ifelse(pvalue < 0.001, "***",
                        ifelse(pvalue < 0.01, "**",
                               ifelse(pvalue < 0.05, "*", ""))))%>%
      mutate(P=round(pvalue,3)) %>%
      mutate(P2=ifelse(P==0,"p< .001",paste0("p=",P))) %>%
      mutate(Pvalue=ifelse(op=="=~","",
                           ifelse(op=="~~"&lhs != rhs, P2 ,
                                  ifelse(op=="~~","",
                                         ifelse(op=="~", P2, "")))))

    b<-gettextf('[%.3f] %s', table2$std.all,table2$Pvalue)
    # b<-gettextf('%.3f %s', table2$std.all, digits=table2$sig)



    semPaths(x,what=what, fade=fade, style="lisrel",
             layout = layout , rotation = rotation,

             layoutSplit = layoutSplit,
             subRes = subRes,#latent
             subScale = subScale, #width
             subScale2 = subScale2, #height
             posCol=posCol,


             curve = curve,
             curveAdjacent = curveAdjacent, # reg or ->
             curvature = curvature,
             curvePivot=curvePivot,
             residScale = residScale,
             residuals = residuals, #관측변수의 분산 감추기
             intercepts = intercepts ,
             vTrans=vTrans,cut=cut,

             sizeLat =sizeLat,sizeLat2=sizeLat2,
             sizeMan = sizeMan,sizeMan2 = sizeMan2,
             edge.label.cex = edge.label.cex,
             edge.label.position=edge.label.position,
             asize=asize,

             edgeLabels =  edgeLabels ,  # pavalue


             nCharNodes =nCharNodes ,
             label.cex=label.cex,

             border.width= border.width,
             groups="latents", pastel = T,
             exoCov = exoCov ,exoVar = exoVar,
             nDigits = nDigits,
             mar = mar,
             structural = structural)

    effect3(x)}, error=function(e)return("정확한 인수로 입력하세요.what =??? "))
}



# semPaths_str구조모델에서만 사용 #####
semPaths_str <- function(x,
                           structural = T,
                           what="std",
                           fade=F,rotation=2,

                           edge.label.cex = 1.5,
                           edge.label.position=0.4,
                           asize=1.5,
                           label.cex=1.7,
                           nCharNodes =8,

                           layout="tree2",
                           curve = 1.2,
                           curveAdjacent = "<->",
                           curvature=F,
                           curvePivot=F,

                           sizeLat = 8, sizeLat2 = 3,
                           sizeMan = 4, sizeMan2 = 2,


                           residuals=F,
                           residScale = 8,
                           intercepts = F,
                           exoCov = T ,
                           exoVar = T,
                           nDigits = 3,
                           layoutSplit = F,
                           subRes = 2,#latent
                           subScale = 0.5,
                           subScale2 = 0.08,

                           mar = c(4,4,4,4),
                           vTrans=255,
                           border.width=2,
                           edgeLabels =b,
                         posCol="gray30",
                           cut=NULL

)
{
  library(semPlot)
  library(dplyr)

  tryCatch({

 if(exoCov == T){
    table2 <-parameterEstimates(x, standardized = T) %>%
      filter(op=="~"|op=="~~"&lhs !=rhs) %>%#
      mutate(sig=ifelse(pvalue < 0.001, "***",
                        ifelse(pvalue < 0.01, "**",
                               ifelse(pvalue < 0.05, "*", ""))))%>%
      mutate(Sig=ifelse(ifelse(op=="~~"&lhs != rhs,sig,
                               ifelse(op=="~~","",
                                      ifelse(op=="~", sig, "")))))
 }else{
   if(exoCov == F){
     table2 <-parameterEstimates(x, standardized = T) %>%
       filter(op=="~") %>%#|op=="~~"&lhs !=rhs
       mutate(sig=ifelse(pvalue < 0.001, "***",
                         ifelse(pvalue < 0.01, "**",
                                ifelse(pvalue < 0.05, "*", ""))))%>%
       mutate(Sig=ifelse(ifelse(op=="~~"&lhs != rhs, sig ,
                                ifelse(op=="~~","",
                                       ifelse(op=="~", sig, "")))))
   }else{return("exoCov =F or T")   }
 }






    # b<-gettextf('%.3f \n p=%.3f', table2$std.all, digits=table2$pvalue)
    b<-gettextf('%.3f %s', table2$std.all, table2$sig)



    semPaths(x,what=what, fade=fade, #style="lisrel",
             layout = layout , rotation = rotation,

             layoutSplit = layoutSplit,
             subRes = subRes,#latent
             subScale = subScale, #width
             subScale2 = subScale2, #height


             curve = curve,
             curveAdjacent = curveAdjacent, # reg or ->
             curvature = curvature,
             curvePivot=curvePivot,
             residScale = residScale,
             residuals = residuals, #관측변수의 분산 감추기
             intercepts = intercepts ,
             vTrans=vTrans,cut=cut,

             sizeLat =sizeLat,sizeLat2=sizeLat2,
             sizeMan = sizeMan,sizeMan2 = sizeMan2,
             edge.label.cex = edge.label.cex,
             edge.label.position=edge.label.position,
             asize=asize,

             edgeLabels =  edgeLabels ,  # pavalue


             nCharNodes =nCharNodes ,
             label.cex=label.cex,

             border.width= border.width,
             groups="latents", pastel = T,
             exoCov = exoCov ,exoVar = exoVar,
             nDigits = nDigits,
             mar = mar,posCol=posCol,
             structural = structural)

    effect3_label(x)}, error=function(e)return("정확한 인수로 입력하세요.what =??? "))
}



#구조모델
semPaths_str.p <- function(x,
                         structural = T,
                         what="std",
                         fade=F,rotation=2,

                         edge.label.cex = 1.5,
                         edge.label.position=0.65,
                         asize=1.5,
                         label.cex=1.7,
                         nCharNodes =8,

                         layout="tree2",
                         curve = 1.2,
                         curveAdjacent = "<->",
                         curvature=F,
                         curvePivot=F,

                         sizeLat = 8, sizeLat2 = 3,
                         sizeMan = 4, sizeMan2 = 2,


                         residuals=F,
                         residScale = 8,
                         intercepts = F,
                         exoCov = T ,
                         exoVar = T,
                         nDigits = 3,
                         layoutSplit = F,
                         subRes = 2,#latent
                         subScale = 0.5,
                         subScale2 = 0.08,

                         mar = c(4,4,4,4),
                         vTrans=255,
                         border.width=2,
                         edgeLabels =b,
                         cut=NULL

)
{
  library(semPlot)
  library(dplyr)

  tryCatch({

    if(exoCov == T){
      table2 <-parameterEstimates(x, standardized = T) %>%
        filter(op=="~"|op=="~~"&lhs !=rhs) %>%
        mutate(sig=ifelse(pvalue < 0.001, "***",
                          ifelse(pvalue < 0.01, "**",
                                 ifelse(pvalue < 0.05, "*", ""))))%>%
        mutate(P=round(pvalue,3)) %>%
        mutate(P2=ifelse(P==0,"p< .001",paste0("p=",P))) %>%
        mutate(Pvalue=ifelse(op=="=~","",
                             ifelse(op=="~~"&lhs != rhs, P2 ,
                                    ifelse(op=="~~","",
                                           ifelse(op=="~", P2, "")))))
    }else{
      if(exoCov == F){

        table2 <-parameterEstimates(x, standardized = T) %>%
          filter(op=="~~"&lhs !=rhs) %>%
          mutate(sig=ifelse(pvalue < 0.001, "***",
                            ifelse(pvalue < 0.01, "**",
                                   ifelse(pvalue < 0.05, "*", ""))))%>%
          mutate(P=round(pvalue,3)) %>%
          mutate(P2=ifelse(P==0,"p< .001",paste0("p=",P))) %>%
          mutate(Pvalue=ifelse(op=="=~","",
                               ifelse(op=="~~"&lhs != rhs, P2 ,
                                      ifelse(op=="~~","",
                                             ifelse(op=="~", P2, "")))))
      }else{return("exoCov =F or T")   }
    }



    b<-gettextf('%.3f, %s.', table2$std.all, digits=table2$Pvalue)




    semPaths(x,what=what, fade=fade, style="lisrel",
             layout = layout , rotation = rotation,

             layoutSplit = layoutSplit,
             subRes = subRes,#latent
             subScale = subScale, #width
             subScale2 = subScale2, #height


             curve = curve,
             curveAdjacent = curveAdjacent, # reg or ->
             curvature = curvature,
             curvePivot=curvePivot,
             residScale = residScale,
             residuals = residuals, #관측변수의 분산 감추기
             intercepts = intercepts ,
             vTrans=vTrans,cut=cut,

             sizeLat =sizeLat,sizeLat2=sizeLat2,
             sizeMan = sizeMan,sizeMan2 = sizeMan2,
             edge.label.cex = edge.label.cex,
             edge.label.position=edge.label.position,
             asize=asize,

             edgeLabels =  edgeLabels ,  # pavalue


             nCharNodes =nCharNodes ,
             label.cex=label.cex,

             border.width= border.width,
             groups="latents", pastel = T,
             exoCov = exoCov ,exoVar = exoVar,
             nDigits = nDigits,
             mar = mar,
             structural = structural)

    effect3(x)}, error=function(e)return("정확한 인수로 입력하세요.what =??? "))
}

# 2021. 수학검사 플롯 최적화
semPaths.est<- function(x,
                        structural = F,
                        what="est",
                        fade=F,rotation=2,

                        edge.label.cex = 1.2,
                        edge.label.position=0.6,
                        asize=1,
                        label.cex=2,
                        nCharNodes =10,

                        layout="tree2",
                        curve = 1.1,
                        curveAdjacent = "<->",
                        curvature=F,
                        curvePivot=F,

                        sizeLat =5,sizeLat2 = 1.5,
                        sizeMan =4,sizeMan2 = 1,


                        residuals=F,
                        residScale = 8,
                        intercepts = F,
                        exoCov = T ,exoVar = F,
                        nDigits = 3,
                        layoutSplit = F,
                        subRes = 2,#latent
                        subScale = 0.5,
                        subScale2 = 0.08,

                        mar = c(2,3,2,3),
                        vTrans=255,
                        border.width=2,
                        edgeLabels = b,
                        cut=NULL
)
{
  library(semPlot)
  library(dplyr)

  tryCatch({


    table2 <-parameterEstimates(x, standardized = T) %>%
      filter(op=="=~"|op=="~"|op=="~~"&lhs!=rhs) %>%
      mutate(sig=ifelse(pvalue < 0.001, "***",
                        ifelse(pvalue < 0.01, "**",
                               ifelse(pvalue < 0.05, "*", ""))))%>%
      mutate(Sig=ifelse(op=="=~","",
                        ifelse(op=="~~"&lhs != rhs, sig ,
                               ifelse(op=="~~","",
                                      ifelse(op=="~", sig, "")))))


    # b<-gettextf('%.3f \n p=%.3f', table2$std.all, digits=table2$pvalue)
    b<-gettextf('%.3f %s', table2$est, digits=table2$Sig)


    semPaths(x,what=what, fade=fade, style="lisrel",
             layout = layout , rotation = rotation,

             layoutSplit = layoutSplit,
             subRes = subRes,#latent
             subScale = subScale, #width
             subScale2 = subScale2, #height


             curve = curve,
             curveAdjacent = curveAdjacent, # reg or ->
             curvature = curvature,
             curvePivot=curvePivot,
             residScale = residScale,
             residuals = residuals, #관측변수의 분산 감추기
             intercepts = intercepts ,
             vTrans=vTrans,cut=cut,


             reorder = T, #manifest variable reorder
             # latents = FALSE,

             sizeLat =sizeLat,sizeLat2=sizeLat2,
             sizeMan = sizeMan,sizeMan2 = sizeMan2,
             edge.label.cex = edge.label.cex,
             edge.label.position=edge.label.position,
             asize=asize,

             edgeLabels =  edgeLabels ,  # pavalue


             nCharNodes =nCharNodes ,
             label.cex=label.cex,

             border.width= border.width,
             groups="latents", pastel = T,
             exoCov = exoCov ,exoVar = exoVar,
             nDigits = nDigits,
             mar = mar,
             structural = structural)

    effect3(x)}, error=function(e)return("정확한 인수로 입력하세요.what =??? "))
}


#모형그리기lavaanPlot  ####
LavaanPlot<-function(x, stand=TRUE, rotation="LR", #option ="RL, Top
                     coefs=TRUE,col="gray60",
                     covs= T){
  library(lavaanPlot)
  lavaanPlot(model = x,
             edge_options = list(color = c(col)),
             coefs=coefs,
             covs= covs,
             stars = c ("covs", "latent","regress"),
             stand = stand,
             graph_options = list(layout="dot",rankdir = rotation)

             # ,
             # graph_options = list(overlap = "true", fontsize = "10" ),
             # labels = labels
  )
}





#라벨이 있는 경우
LavaanPlot_label<-function(x, stand=TRUE, rotation="LR",
                           coefs=TRUE,col="tomato",
                           covs= T,labels=""){
  library(lavaanPlot)
  lavaanPlot(model = x,
             edge_options = list(color = c(col)),
             coefs=coefs,
             covs= covs,
             stars = c ("covs", "latent","regress"),
             stand = stand,
             graph_options = list(layout="dot",rankdir = rotation),

             # ,
             # graph_options = list(overlap = "true", fontsize = "10" ),
             labels = labels)
}
#label은 변수명에 1:1로 지정해주어야 함.
# labels=c(f1_1="품질기대", f1_2="요구기대",f1_3="문제기대",
# https://blog.naver.com/shoutjoy/221951260074



#전체사용
ggsem <- function(x, type="std"){
  tryCatch({
    switch(type,
           std=ggsem_std(x),
           est=ggsem_est(x))
  }, error=function(e){return("다시 정확한 값을 입력해주세요")})
}

# SEM plotting ######
# Plot a fitted lavaan object
#layout="sugiyama" ,kk, fr
ggsem_std <- function(fit,
                      layout ="kk",labelcex=4,
                      x_pos=0.5, y_pos=0.3
                      # edge.label.cex=3
                      ) {
  library(tidyverse)
  library(tidygraph)
  library(ggraph)
  library(lavaan)
  # Extract standardized parameters
  params <- lavaan::standardizedSolution(fit)

  # Edge properties
  param_edges <- params %>%
    filter(op %in% c("=~", "~", "~~"), lhs != rhs, pvalue < .10) %>%
    transmute(to = lhs,
              from = rhs,
              val = est.std,
              type = dplyr::case_when(
                op == "=~" ~ "loading",
                op == "~"  ~ "regression",
                op == "~~" ~ "correlation",
                TRUE ~ NA_character_))

  # Identify latent variables for nodes
  latent_nodes <- param_edges %>%
    filter(type == "loading") %>%
    distinct(to) %>%
    transmute(metric = to, latent = TRUE)

  # Node properties
  param_nodes <- params %>%
    filter(lhs == rhs) %>%
    transmute(metric = lhs, e = est.std) %>%
    left_join(latent_nodes) %>%
    mutate(latent = if_else(is.na(latent), FALSE, latent))

  # Complete Graph Object
  param_graph <- tidygraph::tbl_graph(param_nodes, param_edges)

  # Plot
  g <- ggraph(param_graph, layout = layout) +
    # Latent factor Nodes
    geom_node_point(aes(alpha = as.numeric(latent)),
                    shape = 16, size = 5) +
    geom_node_point(aes(alpha = as.numeric(latent)),
                    shape = 16, size = 25, color = "skyblue") +
    # Observed Nodes
    geom_node_point(aes(alpha = as.numeric(!latent)),
                    shape = 15, size = 5) +
    geom_node_point(aes(alpha = as.numeric(!latent)),
                    shape = 15, size = 3, color = "orange") +
    # Regression Paths (and text)
    geom_edge_link(aes(color = val, label = round(val, 3), size=8, #####
                       alpha = as.numeric(type == "regression")),
                   linetype = 1, linemitre = 1,   #추가
                   angle_calc = "along", vjust = -.6,
                   arrow = arrow(20, unit(.5, "cm"), type = "closed")) +
    # Factor Loadings (no text)
    geom_edge_link(aes(color = val,
                       alpha = as.numeric(type == "loading")),
                   linetype = 1, angle_calc = "along",
                   arrow = arrow(20, unit(.3, "cm"),
                                 ends = "first", type = "closed")) +
    # Correlation Paths (no text)
    geom_edge_link(aes(color = val,
                       alpha = as.numeric(type == "correlation")),
                   linetype = 2, angle_calc = "along",
                   arrow = arrow(20, unit(.3, "cm"),
                                 type = "closed",
                                 ends = "both")) +
        # Node names::latent Variable
    geom_node_text(aes(label = metric),size=labelcex,
                   nudge_y = y_pos , nudge_x = x_pos, hjust = "inward") +
    # Node residual error
    geom_node_text(aes(label = sprintf("%.2f", e)),
                   nudge_y = -.25, size = 4) +
    # Scales and themes
    scale_alpha(guide = FALSE, range = c(0, 1)) +
    scale_edge_alpha(guide = FALSE, range = c(0, 1)) +
    scale_edge_colour_gradient2(guide = FALSE, low = "red",
                                mid = "gray40",
                                high = "navyblue") +
    scale_edge_linetype(guide = FALSE) +
    scale_size(guide = FALSE) +
    theme_graph()

  res=list(Diagram=g,Regression=effect3(fit))
  res

}




# Plot a fitted lavaan object
ggsem_est <- function(fit, layout = "sugiyama") {
  library(tidyverse)
  library(tidygraph)
  library(ggraph)
  library(lavaan)
  # Extract standardized parameters
  params <- lavaan::parameterEstimates(fit)

  # params$pvalue[params$pvalue==NA]<- 0
  # Edge properties
  param_edges <- params %>%
    filter(op %in% c("=~", "~", "~~"), lhs != rhs ) %>%  #pvalue < .10
    transmute(to = lhs,
              from = rhs,
              val = est,
              type = dplyr::case_when(
                op == "=~" ~ "loading",
                op == "~"  ~ "regression",
                op == "~~" ~ "correlation",
                TRUE ~ NA_character_))

  # Identify latent variables for nodes
  latent_nodes <- param_edges %>%
    filter(type == "loading") %>%
    distinct(to) %>%
    transmute(metric = to, latent = TRUE)

  # Node properties
  param_nodes <- params %>%
    filter(lhs == rhs) %>%
    transmute(metric = lhs, e = est) %>%
    left_join(latent_nodes) %>%
    mutate(latent = if_else(is.na(latent), FALSE, latent))

  # Complete Graph Object
  param_graph <- tidygraph::tbl_graph(param_nodes, param_edges)

  # Plot
 g <-  ggraph(param_graph, layout = layout) +
    # Latent factor Nodes
    geom_node_point(aes(alpha = as.numeric(latent)),
                    shape = 16, size = 5) +
    geom_node_point(aes(alpha = as.numeric(latent)),
                    shape = 16, size = 25, color = "skyblue") +
    # Observed Nodes
    geom_node_point(aes(alpha = as.numeric(!latent)),
                    shape = 15, size = 5) +
    geom_node_point(aes(alpha = as.numeric(!latent)),
                    shape = 15, size = 3, color = "orange") +
    # Regression Paths (and text)
    geom_edge_link(aes(color = val, label = round(val, 2),
                       alpha = as.numeric(type == "regression")),
                   linetype = 1, angle_calc = "along", vjust = -.5,
                   arrow = arrow(20, unit(.3, "cm"), type = "closed")) +
    # Factor Loadings (no text)
    geom_edge_link(aes(color = val, alpha = as.numeric(type == "loading")),
                   linetype = 3, angle_calc = "along",
                   arrow = arrow(20, unit(.3, "cm"), ends = "first", type = "closed")) +
    # Correlation Paths (no text)
    geom_edge_link(aes(color = val, alpha = as.numeric(type == "correlation")),
                   linetype = 2, angle_calc = "along",
                   arrow = arrow(20, unit(.3, "cm"), type = "closed", ends = "both")) +
    # Node names
    geom_node_text(aes(label = metric),
                   nudge_y = .25, hjust = "inward") +
    # Node residual error
    geom_node_text(aes(label = sprintf("%.2f", e)),
                   nudge_y = -.1, size = 3) +
    # Scales and themes
    scale_alpha(guide = FALSE, range = c(0, 1)) +
    scale_edge_alpha(guide = FALSE, range = c(0, 1)) +
    scale_edge_colour_gradient2(guide = FALSE, low = "red",
                                mid = "darkgray", high = "darkgreen") +
    scale_edge_linetype(guide = FALSE) +
    scale_size(guide = FALSE) +
    theme_graph()
  res=list(Diagram=g,Regression=effect3(fit))
  res

}






#모형그리기 Diagram####
SemPaths<- function(x,sizeLat=8,sizeLat2=4,sizeMan=4,sizeMan2=2,
                    edge.label.cex=1,
                    label.cex=1.5,
                    border.width=1,
                    what="std",fade=F,
                    rotation=1,
                    layout="tree2",
                    curve=1,
                    curvePivot=F,
                    curvature=1,
                    curveAdjacent="<->",
                    style="lisrel",
                    edge.label.position=.58,
                    edge.width=1.2,
                    # edge.color="",
                    residScale=3,
                    residuals=T,
                    intercepts=F,
                    structural=F,
                    asize=1.2,
                    bg="gray84",
                    nDigits=3,
                    exoCov=T,
                    exoVar=F,
                    mar= c(3,3,3,3),
                    nCharNodes=8,
                    option=2){
  library(semPlot)
  if(option==1){
    semPaths(x, what = what,fade=fade, nCharNodes = nCharNodes, #text number
             rotation = rotation ,intercepts = intercepts,
             style = style,border.width=border.width,
             exoVar =exoVar ,
             exoCov = exoCov,
             residScale = residScale,
             residuals = residuals, #관측변수의 분산 감추기
             curvePivot=curvePivot,
             curveAdjacent= curveAdjacent,
             curvature = curvature ,
             curve = curve,
             layout = layout, #shape
             layoutSplit = F ,subScale = 1,
             subScale2 = 1, # manifest 1 row, 2column
             subRes = 4, #Default=4
             sizeLat = sizeLat,sizeLat2 = sizeLat2 ,
             sizeMan = sizeMan,sizeMan2 = sizeMan2,
             color = list(lat="skyblue", man="Gold", int="gray80"),
             # groups = "latent",pastel = T,
             label.cex=label.cex,
             edge.label.cex = edge.label.cex,
             # edge.color = edge.color,
             edge.label.position= edge.label.position,
             edge.width= edge.width,
             asize=asize,
             mar=mar, bg=bg,
             structural = structural,
             nDigits = nDigits,

    )

  }
  else{
    if(option==2){
      semPaths(x, what = what,fade=fade,  nCharNodes= nCharNodes, #text number
               rotation = rotation ,intercepts = intercepts,
               style = style, border.width=border.width,
               exoVar =exoVar ,
               exoCov = exoCov,
               residScale = residScale,
               # residuals = residuals,
               curvePivot=curvePivot,
               curveAdjacent= curveAdjacent,
               curvature = curvature ,
               curve = curve,
               layout = layout, #shape
               layoutSplit = F ,subScale = 1,
               subScale2 = 1, # manifest 1 row, 2column
               subRes = 4, #Default=4
               sizeLat = sizeLat,sizeLat2 = sizeLat2 ,
               sizeMan = sizeMan,sizeMan2 = sizeMan2,
               # color = list(lat="skyblue", man="Gold", int="gray80"),
               groups = "latent",pastel = T,
               label.cex=label.cex,
               edge.label.cex = edge.label.cex,
               # edge.color = edge.color,
               edge.label.position= edge.label.position,
               edge.width= 1.2,
               asize=asize,
               mar=mar, bg="white",
               structural = structural,
               nDigits = nDigits)

    }
    else{return("input option=1,2")}
  }
}
# SemPaths(x,lat=8,lat2=4,man=4,man2=2,
#                     edge.label.cex=1,
#                     label.cex=1.5,
#                     border.width=1,
#                     what="par",fade=F,
#                     rotation=1,
#                     layout="tree2",
#                     curve=1,
#                     curvePivot=F,
#                     curvature=1,
#                     curveAdjacent="<->",
#                     style="lisrel",
#                     edge_label_pos=.58,
#                     edge.width=1.2,
#                     # edge.color="",
#                     residScale=3,
#                     intercepts=F,
#                     strut_T_F=F,
#                     asize=1.2,
#                     bg="gray84",
#                     nDigits=3,
#                     exoCov=T,
#                     exoVar=F,
#                     mar= c(3,3,3,3),
#                     nCharNodes=8,
#                     option=2)




#모형그리기 Diagram####
SemPaths_cfa<- function(x,lat=8,lat2=4,man=4,man2=2,
                        edge.label.cex=1,
                        label.cex=1.5,
                        border.width=1,
                        what="par",fade=F,
                        rotation=1,
                        layout="tree2",
                        curve=1,
                        curvePivot=F,
                        curvature=1,
                        curveAdjacent="<->",
                        style="lisrel",
                        edge_label_pos=.58,
                        edge.width=1.2,
                        # edge.color="",
                        residScale=3,
                        intercepts=F,
                        strut_T_F=F,
                        asize=1.2,
                        bg="gray84",
                        nDigits=3,
                        exoCov=T,
                        exoVar=F,
                        mar= c(3,3,3,3),
                        nCharNodes=8,
                        option=2,
                        posCol="gray20"){
  library(semPlot)
  if(option==1){
    semPaths(x, what = what,fade=fade, nCharNodes = nCharNodes, #text number
             rotation = rotation ,intercepts = intercepts,
             style = style,border.width=border.width,
             exoVar =exoVar ,
             exoCov = exoCov,
             residScale = residScale,
             # residuals = residuals,
             curvePivot=curvePivot,
             curveAdjacent= curveAdjacent,
             curvature = curvature ,
             curve = curve,
             layout = layout, #shape
             layoutSplit = F ,subScale = 1,
             subScale2 = 1, # manifest 1 row, 2column
             subRes = 4, #Default=4
             sizeLat = lat,sizeLat2 = lat2 ,
             sizeMan = man,sizeMan2 = man2,
             color = list(lat="skyblue", man="Gold", int="gray80"),
             # groups = "latent",pastel = T,
             label.cex=label.cex,
             edge.label.cex = edge.label.cex,
             # edge.color = edge.color,
             edge.label.position= edge_label_pos,
             edge.width= edge.width,
             asize=asize,
             mar=mar, bg=bg,
             structural = strut_T_F,
             nDigits = nDigits,
             posCol=posCol

    )

  }
  else{
    if(option==2){
      semPaths(x, what = what,fade=fade,  nCharNodes= nCharNodes, #text number
               rotation = rotation ,intercepts = intercepts,
               style = style, border.width=border.width,
               exoVar =exoVar ,
               exoCov = exoCov,
               residScale = residScale,
               # residuals = residuals,
               curvePivot=curvePivot,
               curveAdjacent= curveAdjacent,
               curvature = curvature ,
               curve = curve,
               layout = layout, #shape
               layoutSplit = F ,subScale = 1,
               subScale2 = 1, # manifest 1 row, 2column
               subRes = 4, #Default=4
               sizeLat = lat,sizeLat2 = lat2 ,
               sizeMan = man,sizeMan2 = man2,
               # color = list(lat="skyblue", man="Gold", int="gray80"),
               groups = "latent",pastel = T,
               label.cex=label.cex,
               edge.label.cex = edge.label.cex,
               # edge.color = edge.color,
               edge.label.position= edge_label_pos,
               edge.width= 1.2,
               asize=asize,
               mar=mar, bg="white",
               structural = strut_T_F,
               nDigits = nDigits,
               posCol=posCol)

    }
    else{return("input option=1,2")}
  }
}


# Diagarm조절한 함수######
#
semPaths1<- function(x,structural = F,what="std",
                     fade=F,

                     edge.label.cex = 0.9,
                     edge.label.position=0.65,
                     asize=1.2,
                     rotation=2,

                     layout="tree2",
                     curve = 1.1,
                     curveAdjacent = "<->",
                     curvature=F,
                     curvePivot=F,

                     sizeLat =6,sizeLat2=3,
                     sizeMan =5,sizeMan2 = 2,

                     label.cex=1.8,
                     residuals=T,
                     residScale = 8,

                     nDigits = 3,
                     layoutSplit = F,
                     subRes = 2,#latent
                     subScale = 0.5,
                     subScale2 = 0.08,
                     intercepts = F,
                     exoCov = T ,exoVar = F,
                     nCharNodes =10,
                     mar = c(2,3,2,3),
                     vTrans=255,
                     border.width=2,
                     # edgeLabels =b,
                     cut=NULL, #cut=.3
                     posCol = "gray20"
)
{
  library(semPlot)
  library(dplyr)

  tryCatch({
    #
    #     table2 <-parameterEstimates(x, standardized = T) %>%
    #       filter(op=="=~"|op=="~")
    #     b<-gettextf('%.3f \n p=%.3f',
    #                 table2$std.all, digits=table2$pvalue)
    #


    semPaths(x,what=what, fade=fade, style="lisrel",
             layout = layout , rotation = rotation,

             layoutSplit = layoutSplit,
             subRes = subRes,#latent
             subScale = subScale, #width
             subScale2 = subScale2, #height
             posCol = posCol,


             curve = curve,
             curveAdjacent = curveAdjacent, # reg or ->
             curvature = curvature,
             curvePivot=curvePivot,
             residScale = residScale,
             residuals = residuals, #관측변수의 분산 감추기
             intercepts = intercepts ,
             vTrans=vTrans,cut=cut,


             reorder = T, #manifest variable reorder
             # latents = FALSE,

             sizeLat =sizeLat,sizeLat2=sizeLat2,
             sizeMan = sizeMan,sizeMan2 = sizeMan2,
             edge.label.cex = edge.label.cex,
             edge.label.position=edge.label.position,
             asize=asize,

             # edgeLabels =  edgeLabels ,  # pavalue


             nCharNodes =nCharNodes ,
             label.cex=label.cex,

             border.width= border.width,
             groups="latents", pastel = T,
             exoCov = exoCov ,exoVar = exoVar,
             nDigits = nDigits,
             mar = mar,
             structural = structural)
    #경로계수 Ef
    library(dplyr)
    library(knitr)
    # parameterEstimates(x, standardized = T, rsquare = T) %>%
    #   filter(op=="~"| op==":=") %>%
    #   mutate(stars=ifelse(pvalue<0.001,"***",
    #                       ifelse(pvalue<0.01,"**",
    #                              ifelse(pvalue<0.05,"*","")))) %>%
    #   mutate(op=ifelse(op=="~","<--","" )) %>%
    #   dplyr::select(Dependent=lhs,Path=op,
    #                 Independent=rhs,
    #                 est,se,
    #                 "Path_Coeff"= std.all,
    #                 c.r=z,
    #                 Sig.=stars,
    #                 "p"=pvalue) %>%
    #   kable(format=format, digits = 3,
    #         caption ="Path Coefficient")
    effect3(x)}, error=function(e)return("정확한 인수로 입력하세요.what =??? "))
}


# Diagarm조절한 함수######
# 2021. 수학검사 플롯 최적화#####
semPaths2<- function(x,structural = F,what="std",
                     edge.label.cex = 0.9,
                     edge.label.position=0.6,
                     edge.color="gray30",

                     asize=1.2,
                     rotation=2,
                     residuals=T,
                     layout="tree2",
                     curve = 1.1,
                     curveAdjacent = "->",
                     curvature=F,
                     curvePivot=F,
                     sizeLat =8,sizeLat2=1, sizeMan =4,sizeMan2 = 1,
                     label.cex=2.5,
                     residScale = 8,
                     fade=F,
                     nDigits = 3,
                     layoutSplit = F,
                     subRes = 2,#latent
                     subScale = 0.5,
                     subScale2 = 0.08,
                     intercepts = F,
                     exoCov = T ,exoVar = F,
                     nCharNodes =10,
                     mar = c(2,3,2,3),
                     vTrans=255,
                     cut=NULL,
                     border.width=2

)
{  library(semPlot)

  semPaths(x,what=what, fade=fade, style="lisrel",
           layout = layout , rotation = rotation,

           layoutSplit = layoutSplit,
           subRes = subRes,#latent
           subScale = subScale, #width
           subScale2 = subScale2, #height


           curve = curve,
           curveAdjacent = curveAdjacent, # reg or ->
           curvature = curvature,
           curvePivot=curvePivot,
           residScale = residScale,
           residuals = residuals, #관측변수의 분산 감추기
           intercepts = intercepts ,
           vTrans=vTrans,cut=cut,


           reorder = T, #manifest variable reorder
           # latents = FALSE,

           sizeLat =sizeLat,sizeLat2=sizeLat2,
           sizeMan = sizeMan,sizeMan2 = sizeMan2,
           edge.label.cex = edge.label.cex,
           edge.label.position=edge.label.position,
           edge.color = edge.color,

           asize=asize,


           nCharNodes =nCharNodes ,
           label.cex=label.cex,

           border.width= border.width,
           groups="latents", pastel = T,
           exoCov = exoCov ,exoVar = exoVar,
           nDigits = nDigits,
           mar = mar,
           structural = structural)


  #수학검사 라인 조절
semPaths2.1 <- function(x,structural = F,whatLabels="std",
                       edge.label.cex = 0.9,
                       edge.label.position=0.6,
                       edge.color="gray30",
                       asize=1.2,
                       rotation=2,
                       residuals=T,
                       layout="tree2",
                       curve = 1.1,
                       curveAdjacent = "->",
                       curvature=F,
                       curvePivot=F,
                       sizeLat =8,sizeLat2=1, sizeMan =4,sizeMan2 = 1,
                       label.cex=2.5,
                       residScale = 8,
                       fade=F,
                       nDigits = 3,
                       layoutSplit = F,
                       subRes = 2,#latent
                       subScale = 0.5,
                       subScale2 = 0.08,
                       intercepts = F,
                       exoCov = T ,exoVar = F,
                       nCharNodes =10,
                       mar = c(2,3,2,3),
                       vTrans=255,
                       cut=NULL,
                       border.width=2

  )
  {  library(semPlot)

    semPaths(x,whatLabels = whatLabels, fade=fade, style="lisrel",
             layout = layout , rotation = rotation,

             layoutSplit = layoutSplit,
             subRes = subRes,#latent
             subScale = subScale, #width
             subScale2 = subScale2, #height


             curve = curve,
             curveAdjacent = curveAdjacent, # reg or ->
             curvature = curvature,
             curvePivot=curvePivot,
             residScale = residScale,
             residuals = residuals, #관측변수의 분산 감추기
             intercepts = intercepts ,
             vTrans=vTrans,cut=cut,


             reorder = T, #manifest variable reorder
             # latents = FALSE,

             sizeLat =sizeLat,sizeLat2=sizeLat2,
             sizeMan = sizeMan,sizeMan2 = sizeMan2,
             edge.label.cex = edge.label.cex,
             edge.label.position=edge.label.position,
             edge.color = edge.color,

             asize=asize,


             nCharNodes =nCharNodes ,
             label.cex=label.cex,

             border.width= border.width,
             groups="latents", pastel = T,
             exoCov = exoCov ,exoVar = exoVar,
             nDigits = nDigits,
             mar = mar,
             structural = structural)
  }


  #경로계수 Ef
  # library(dplyr)
  # library(knitr)
  # parameterEstimates(x, standardized = T, rsquare = T) %>%
  #   filter(op=="~"| op==":=") %>%
  #   mutate(stars=ifelse(pvalue<0.001,"***",
  #                       ifelse(pvalue<0.01,"**",
  #                              ifelse(pvalue<0.05,"*","")))) %>%
  #   mutate(op=ifelse(op=="~","<--","" )) %>%
  #   dplyr::select(Dependent=lhs,Path=op,
  #                 Independent=rhs,
  #                 est,se,
  #                 "Path_Coeff"= std.all,
  #                 c.r=z,
  #                 Sig.=stars,
  #                 "p"=pvalue) %>%
  #   kable(format=format, digits = 3,
  #         caption ="Path Coefficient")
  #effect3(x)}, error=function(e)return("정확한 인수로 입력하세요.what =??? "))
}





#왜도와 첨도에 의한 정규성 검정  데이터 테이블을 확인, 정밀한 왜도와 첨도기준
#계산 방법은 SPSS와 동일함
#정규성 판단####
NormalityTest<- function(input_data,
                         format="markdown",
                         digit=3){
  library(psych)
  library(knitr)
  input_data<-as.data.frame(input_data)
  output_data <- psych::describe(input_data)
  N<-output_data$n
  output_data$skew.z <- output_data$skew/sqrt((6*N*((N-1))/((N-2)*(N+1)*(N+3))))
  output_data$kurt.z <- output_data$kurtosis/sqrt((24*N*(N-1)*(N-1))/((N-3)*(N-2)*(N+3)*(N-5)))
  output_data
  output_data<-as.data.frame(output_data)

  a1 <-output_data%>% kable(digits=digit,
                            format="pandoc",
                            caption="Describe Statistics data")

  output<-round(output_data[,c("n","mean","sd","skew", "kurtosis", "skew.z","kurt.z")],digit)
  #data nomality check making .
  output[,"skew_TF"]<- "Not"
  output[output$skew.z < 3,"skew_TF"]<- "fair.norm"
  output[output$skew.z < 1.96,"skew_TF"]<- "Good.norm"
  output[output$skew.z < -1.96,"skew_TF"]<- "fair.norm"
  output[output$skew.z < -3,"skew_TF"]<- "Not"

  output[,"kurt_TF"]<- "Not"
  output[output$kurt.z < 3,"kurt_TF"]<- "fair.norm"
  output[output$kurt.z < 1.96,"kurt_TF"]<- "Good.norm"
  output[output$kurt.z < -1.96,"kurt_TF"]<- "fair.norm"
  output[output$kurt.z < -3,"kurt_TF"]<- "Not"

  a2 <-output%>% kable(digits=digit, format=format, caption="Describe Normality, |skew.Z|<1.96,|Krut.z|<1.96 ")
  cat("ref: Kline(2011):skew<3, kurt<10, Crran,West & Finch(1997):skew<2, kurt<7 ","\n",
      "|skew.Z|<1.96,|Krut.z|<1.96인 경우 정규성을 충족한다","\n",
      "H0: 정규성을 충족한다, H1:정규성을 충족하지 않는다","\n",
      "skew.z=data$skew/sqrt((6*N*((N-1))/((N-2)*(N+1)*(N+3))))","\n",
      "kurt.z=data$kurtosis/sqrt((24*N*(N-1)*(N-1))/((N-3)*(N-2)*(N+3)*(N-5)))")
  res=list(a2, output)
  res

  #ss<- list(a2,pairs.panels(input_data))
  #ss
}




#다변량 분석 카이제곱 그림 함수 ------
chiq2plot <- function(input, title=""){
  library(psych)
  # 기본통계적 데이터
  x=input #데이터
  n=nrow(x) # 샘플수 행
  p=ncol(x) # 변수의 개수
  S=cov(x)
  xbar=colMeans(x)

  #마할노비스 거리 구하기
  m=mahalanobis(x, xbar, S)
  #마할노비스거리의 순위
  m=sort(m)
  #카이제곱 분포 분위수 계산
  id=seq(1, n)
  pt=(id-0.5)/n
  #카이제곱 산출
  q=qchisq(pt, p)

  # correlation:통계적 검증(직진성 평가)
  rq = corr.test(cbind.data.frame(q, m))

  #카이제곱그림그리기
  plot(q, m, pch="●", col="steelblue",
       xlab="Quantile", ylab="Ordered Squared Distance")
  abline(0, 1, col="tomato", lwd=2 )
  grid()
  #제목에 내용붙이기
  title(paste("Chi-squre Plot for Checking MVN(", title,
              "), r =",round(rq$r[1,2],3),"(", rq[13]$stars[1,2] ,")"))

  #CONSOLE에 분석한 데이터 산출하기
  res=list(dim_RC=cbind(n,p),S,xbar,print(rq, digits=3) ) #correlation result
  res
}
 # setosa = iris[1:50, 1:4]  # Iris data only for setosa
# chiq2plot(iris[1:50, 1:4], "setosa")


#히스토그램 #####
histogram <-function(x,ycut=0,
                     breaks=max(x), 
                     main="", 
                     xlab="",
                     ylab=""){
  if(is.numeric(x)){
    hist(x, breaks= breaks, labels=TRUE,
         ylim=c(0, length(x)-ycut),
         density=30, angle=135, border="black",
         main=main, xlab=xlab,ylab=ylab )
    lines(density(x))
    box()

  }else{
    if(!is.numeric(x)){
      x%>%table %>% barplot(ylim=c(0,length(x)))
      text(table(x)+2,label=table(x), cex = 1.5)
      box()
    }else(return("numeric 변수를 입력하세요 "))}
}




#hotelling two samples ------
Hotelling_twoSamples <- function(inputGroup1=NULL, 
                                 inputGroup2=NULL,
                                 group=c("G1","G2")){

  library(tidyverse)
  library(biotools)

  #data treatment
  if(is.data.frame(inputGroup1)){
    x <- inputGroup1 %>% as.matrix()}else{(cat("입력형식을 맞추어주세요, data.frame으로 "))}

  if(is.data.frame(inputGroup1)){
    y <- inputGroup2 %>% as.matrix()}else{(cat("입력형식을 맞추어주세요, data.frame으로 "))}

  #1. 정규성 검정
  Nromality_x <-  mvn(x)
  Nromality_y <-  mvn(y)

  NormResult <- rbind.data.frame(G1 = Nromality_x$multivariateNormality,
                                 G2 = Nromality_x$multivariateNormality)
  NormResult_detail <- rbind.data.frame( G1 =  Nromality_x$univariateNormality,
                                         G2 =  Nromality_y$univariateNormality)



  statData <- rbind.data.frame(
    G1 = Nromality_x$Descriptives[,c(2,3,1)],
    G2 = Nromality_y$Descriptives[,c(2,3,1)])


  # rownames(NormResult_detail) = c(rep(group[1], nrow(NormResult_detail)/2),
  #                                 rep(group[2], nrow(NormResult_detail)/2))



  p=ncol(x)
  n1=nrow(x)
  n2=nrow(y)
  f=n1+n2-2   #degree of freedom

  #[Step2 공분산행렬의 동질성 검정 Box M-test
  # library(biotools)
  # 동질성 검정에 맞게 데이터 정리
  data<- rbind( cbind.data.frame(group= rep(1, nrow(x)), x),
                cbind.data.frame(group= rep(2, nrow(x)), y))

  xy <- data[,-1]
  cluster <- data[,1]

  # 공분산행렬의 동질성 검정 Box M-test
  # Box's M-test for Homogeneity of Covariance Matrices
  boxM_homegeneity  <-  boxM(xy,cluster)   # 데이터, 그룹

  # boxM_homegeneity$statistic
  # boxM_homegeneity$parameter
  # boxM_homegeneity$p.value

  if(boxM_homegeneity$p.value > 0.05){boxM_mag=
    sprintf("Box's M-test for Homogeneity of Covariance Matrices를 이용한 동질성 검정결과, 두개의 자료는 동질하였다, chisq(df = %.4f) = %.4f, p = %.4f ",boxM_homegeneity$parameter,boxM_homegeneity$statistic ,boxM_homegeneity$p.value   )}
  if(boxM_homegeneity$p.value < 0.05){boxM_mag=
    sprintf("Box's M-test for Homogeneity of Covariance Matrices를 이용한 동질성 검정결과, 두개의 자료는 동질하지않았다, chisq(df = %.4f) = %.4f, p = %.4f ",boxM_homegeneity$parameter,boxM_homegeneity$statistic ,boxM_homegeneity$p.value   )}


  if(boxM_homegeneity$p.value > 0.05){
    #[두 데이터가 동질한 경우 ]두 집단의 평균벡터 검정: H0: mu1=mu2
    xb <- matrix(1,n1,1)%*%matrix(1/n1,1,n1)%*%x   # colMeans(x)
    yb <- matrix(1,n2,1)%*%matrix(1/n2,1,n2)%*%y   #colMeans(y)
    s1 <- t(x-xb)%*%(x-xb)/(n1-1)
    s2 <- t(y-yb)%*%(y-yb)/(n2-1)

    sp <- ((n1-1)*s1+(n2-1)*s2)/(n1+n2-2)  #pooled covariance

    Tsq <- (n1*n2)/(n1+n2)*((xb-yb)[1,])%*%solve(sp)%*%matrix((xb-yb)[1,])

    Tasq <- f*p/(f-p+1)*qf(1-0.05,p, f-p+1)  #가설 F

    F0 <- (f-p+1)/(f*p)*Tasq

    values<- c(round(Tsq,7),round(Tasq,7))
    names(values)<-c("Hotelling T2","Chisq_Tasq") #method 2

    if(Tsq > Tasq) {
      result <- sprintf("Hotelling T2(= %.4f) > chisq_Tasq(=%.4f) : H0 reject --> 귀무가설 H0(mu1 = mu2)는 기각되었으므로 두 그룹은 통계적으로 유의한 차이가 나타났다, F(%d, %d) = %.2f  ",Tsq, Tasq, p, f-p+1, F0)}
    if(Tsq < Tasq) {
      result<-sprintf("Hotelling T2(= %.4f) < chisq_Tasq(=%.4f) : H0 accept --> 대립가설[H1: mu1 != mu2]은 기각되었으므로, 두 그룹간 통계적으로 유의한 차이는 나타나지 않았다, F(%d, %d) = %.2f", Tsq, Tasq, p, f-p+1, F0)}
  }


  #동질하지 않은 경우
  if(boxM_homegeneity$p.value < 0.05){
    xb <- matrix(1,n1,1)%*%rep(1/n1,n1)%*%x
    yb <- matrix(1,n2,1)%*%rep(1/n2,n2)%*%y
    s1 <- t(x-xb)%*%(x-xb)/(n1-1)
    s2 <- t(y-yb)%*%(y-yb)/(n2-1)
    e <- (xb[1,]-yb[1,])%*%solve(s1/n1+s2/n2)
    Tsq <- e%*%(xb[1,]-yb[1,])

    finv <- (e%*%s1%*%t(e))^2/((Tsq^2)*(n1^3-n1^2))+(e%*%s2%*%t(e))^2/((Tsq^2)*(n2^3-n2^2))  #df
    F0 <- round(1/finv,0)

    # n1^n2 : samll -> F-dist
    #T0sq <- (f-p+1)/(f*p)*Tsq
    #Ta <- qf(1-0.05,p,f-p+1)

    # n1^n2 : large -> Chi^2-dist
    T0sq<-Tsq # n1^n2
    Ta<- qchisq(1-0.05, p)

    values<-c(T0sq,Ta)
    names(values)<-c("T0sq","Ta")

    if(T0sq > Ta) {
      result <- sprintf("Hotelling T2(= %.4f) > chisq_Tasq(=%.4f) : H0 reject --> 귀무가설 H0(mu1 = mu2)는 기각되었으므로 두 그룹은 통계적으로 유의한 차이가 나타났다, F(%d, %.4f) = %.2f  ", T0sq, Ta, p, finv-p+1, F0)}
    if(T0sq < Ta) {
      result<-sprintf("Hotelling T2(= %.4f) < chisq_Tasq(=%.4f) : H0 accept --> 대립가설[H1: mu1 != mu2]은 기각되었으므로, 두 그룹간 통계적으로 유의한 차이는 나타나지 않았다, F(%d, %.4f) = %.2f", T0sq, Ta, p, finv-p+1, F0)}
  }


  #결과보기
  res=list(MardiaTest=NormResult,
           MardiaTestDetail= NormResult_detail,
           BoxM_test=boxM_homegeneity,
           boxM_mag,
           values,
           Hypothesis = result,
           statistics = statData )
  res
}


# # 공분산 행렬이 동질한 경우
# Hotelling_twoSamples(x,y)
# Hotelling_twoSamples(x,y, group=c("first","second"))
# Hotelling_twoSamples(data %>% filter(pop==1) %>% select(2:3) ,
#                      data %>% filter(pop==2) %>% select(2:3) )
#
#
#
#
# # 공분산 행렬이 동질하지 않은 경우
# Hotelling_twoSamples(iris %>% filter(Species == "setosa") %>% dplyr::select(1:4),
#                      iris %>% filter(Species == "versicolor") %>% dplyr::select(1:4),
#                      group=c("setosa","verscolor"))
#
#
# #
# #
#
#히스토그램 #####
# histogram <-function(x,main="",eight=1.1,
#                      xlab="",ylab=""){
#   if(is.numeric(x)){
#     a<-hist(x, main=main,
#             xlab=xlab, ylab=ylab
#     )
#     text(a$mids,a$counts+height,label=a$counts,cex=1.5)
#   }else{
#     if(!is.numeric(x)){
#       x%>%table %>% barplot(ylim=c(0,length(x)))
#       text(x %>%table+2,label=x %>%table, cex = 1.5)
#     }else(return("numeric 변수를 입력하세요 "))}
# }

#
# grp.compare<- function(x,var1="grp1",var2="grp2"){
#   bargender<-x %>%effect_grp_d()
#   bar1<-bargender[,c(5,10)]
#   # rep(1:nrow(x)
#   rownames(bar1)<-c('수학신념 <- 수학불안','수학신념 <- 인지부하','진로효능감 <- 수학불안','진로효능감 <- 인지부하','학습효능감 <- 수학불안','학습효능감 <- 인지부하','학습참여 <- 수학불안','학습참여 <- 인지부하','진로효능감 <- 수학신념','학습효능감 <- 수학신념','학습참여 <- 수학신념','학습참여 <- 진로효능감','학습참여 <- 학습효능감')
#   # paste(bargender[,c(1)],"<-",bargender[,c(3)],collapse = ",") %>%
#
#   # bar1
#   op=par( mar=c(8,3,1,0.5))
#   barplot(t(as.matrix(bar1)), beside = TRUE,
#           col = c("orange","gray30"), las = 2, ylim = c(-0.5, 1.3),
#           cex.names = 0.8, col.axis = "gray30", cex.axis = 0.8)
#   abline(h=0, col="gray50")
#   title(paste(" Path coefficient of", var1,"and",var2))
#   legend("top", legend=c(var1,var2),
#          col = c("orange","gray30"),ncol=2,bty="n",pch=22,
#          pt.bg = c("orange","gray30"))
#   par(op)
# }



# meta-analysis ----------------------------------------------------------------------------

# 효과크기
cal_eff <- function(data, digits=4){
  library(dplyr)

  dataout <- data %>% mutate(
    MeanEffect = sum(WY)/sum(W),
    Vmean =1/sum(W),
    SEmean = sqrt(Vmean),
    Lower = MeanEffect -1.96*SEmean,
    Upper = MeanEffect +1.96*SEmean,
    Z = MeanEffect / SEmean,
    p =(1-pnorm(Z))*2)

  dataout1 <- dataout %>% 
    dplyr::select(MeanEffect:p) %>% 
    slice(1) %>% 
    t() %>% 
    round(digits)
  
  rownames(dataout1)=c("평균효과크기(M)","분산(Vm)","표준오차",
                       "하한선(95%)","상한선(95%)","Z", "p")
  colnames(dataout1)="value"
  print(dataout1)
}



# Two Way ANOVA emmeans_contrast ----
## twoway anova

emmeans_contrast = function(aov_data, var="", ..., 
                            auto= T,
                            type="all",
                            hjust=0.1,
                            vjust=-2){
  library(emmeans)
  
  # 자동변수 선정하여 분석 
  if(auto == TRUE){
    #결과보고 
    summary <- aov_data %>% rstatix::Anova(type="III")
    apa_report <- aov_data %>% report::report()  
    # aov_formula <- aov_data %>% formula()  
    aov_formula0 =  aov_data%>%formula() 
    aov_formula = aov_formula0%>% as.character() %>% 
      str_split("~") %>% unlist()
    DV_name = aov_formula[3]
    
    n_data <- aov_data %>% tidy()
    var1 = n_data[1,1] %>% unlist()
    var2 = n_data[2,1]%>% unlist()
    
    EM = aov_data %>% 
      emmeans(specs = c(var1, var2)) %>% 
      data.frame()
    
    EM <- EM%>% 
      mutate(mu =c(paste0("m", 1:(nrow(EM)) )) ) %>% 
      dplyr::select(8,1:7) %>% tibble()
    
    
    if(nrow(EM)==4){ #2*2 anova 
      Contrast = aov_data %>% 
        emmeans(specs = c(var1, var2)) %>% 
        contrast(list(  
          "m1-m2: sme of Var1(1st)" = c(1,-1,0,0),
          "m3-m4: sme of Var1(2nd)" = c(0,0,1,-1),
          "m1-m3: sme of Var2(1st)" = c(1,0,-1,0),
          "m2-m4: sme of Var2(2nd)" = c(0,1,-1,0),
          "m1-m2-m3+m4:Interaction"=c(1,-1,-1,1) 
        ))
      
    }else if(nrow(EM)==6){  # 2*3 anova 
      Contrast = aov_data %>% 
        emmeans(specs = c(var1, var2)) %>% 
        contrast(list(
          "m1-m2-m3+m4: Interaction 1" = c(1,-1,-1,1,0,0),
          "m1-m2-m5+m6: Interaction 2" = c(1,-1,0,0,-1,1),
          "m3-m4-m5+m6: Interaction 3" = c(0,0, 1,-1,-1,1),
          "m1-m2: simple main effect 1" = c(1,-1,0,0,0,0),
          "m3-m4: simple main effect 2" = c(0,0, 1,-1,0,0),
          "m5-m6: simple main effect 3" = c(0,0,0,0,1,-1) 
        ))
    }
    
    #significant 
    ndata <- Contrast %>% 
      as.data.frame() %>%
      mutate(sig = ifelse(p.value < 0.001, "***", 
                          ifelse(p.value < 0.01, "**", 
                                 ifelse(p.value < 0.05, "*", 
                                        "ns")))) 
    #변수를 CHARACTER로 변경 
    ndata$sig <- as.character(ndata$sig) 
    ndata <- ndata %>% tibble()
    ndata_plot <- Contrast %>% plot()+
      geom_vline(xintercept = 0, linetype=2, col="Red")+
      labs(x ="Confidence Range",
           title = paste0("Planned Contrast DV: ", DV_name)
      )
    
    variable1 <- as.vector(var1)
    variable2 <- as.vector(var2)
    # 
    EM0 <- EM
    colnames(EM0) <-c("mu","v1","v2","emmean",
                      "SE","df","lower.CL","upper.CL")
    
    #interaction plot 
    library(ggrepel)
    interaction.plot <- EM0 %>% 
      ggplot(aes(x = v2, y = emmean))+
      geom_point(size=3)+
      geom_point(aes(shape= v1,color=v1), #,
                 show.legend = T, size= 4)+
      geom_line(aes(group = v1, linetype= v1), 
                linewidth=0.8, show.legend = F)+
      geom_text_repel(aes(label= round(emmean,2)),
                      vjust = vjust, hjust = hjust, size=5)+
      labs(x = variable2, 
           color = variable1,
           shape = variable1,
           y = DV_name,
           # subtitle = aov_formula0
      )+
      theme_bw()
    
    
    
    #result 
    res=list(
      ANOVA = summary, 
      Estimate_marginal_mean = EM,
      Planned_contrast = ndata,
      APA = apa_report,
      confint_plot = ndata_plot,
      interaction_plot = interaction.plot
    )
    #수동 변수 선정 
  }else if(auto == FALSE){
    #결과보고 type="III"
    summary <- aov_data %>% rstatix::Anova(type="III")
    apa_report <- aov_data %>% report::report() 
    specs0 = c(var, ...)
    
    aov_formula0 =  aov_data%>%formula() 
    aov_formula = aov_formula0%>% as.character() %>% 
      str_split("~") %>% unlist()
    DV_name = aov_formula[3]
    
    
    EM = aov_data %>% 
      emmeans(specs = specs0) %>% 
      data.frame()
    
    EM <- EM%>% 
      mutate(mu =c(paste0("m", 1:(nrow(EM)) ))) %>% 
      dplyr::select(8,1:7) %>% tibble()
    
    
    
    
    if(nrow(EM)==4){ 
      Contrast = aov_data %>% 
        emmeans(specs = specs0) %>% 
        contrast(list(
          "m1-m2: sme of Var1(1st)" = c(1,-1,0,0),
          "m3-m4: sme of Var1(2nd)" = c(0,0,1,-1),
          "m1-m3: sme of Var2(1st)" = c(1,0,-1,0),
          "m2-m4: sme of Var2(2nd)" = c(0,1,-1,0),
          "m1-m2-m3+m4:Interaction"=c(1,-1,-1,1) 
        ))
    }else if(nrow(EM)==6){
      Contrast = aov_data %>% 
        emmeans(specs = specs0) %>% 
        contrast(list(
          "m1-m2-m3+m4: Interaction 1" = c(1,-1,-1,1,0,0),
          "m1-m2-m5+m6: Interaction 2" = c(1,-1,0,0,-1,1),
          "m3-m4-m5+m6: Interaction 3" = c(0,0, 1,-1,-1,1),
          "m1-m2: simple main effect 1" = c(1,-1,0,0,0,0),
          "m3-m4: simple main effect 2" = c(0,0, 1,-1,0,0),
          "m5-m6: simple main effect 3" = c(0,0,0,0,1,-1) 
        ))
    }
    ndata <- Contrast %>% 
      as.data.frame() %>%
      mutate(sig = ifelse(p.value < 0.001, "***", 
                          ifelse(p.value < 0.01, "**", 
                                 ifelse(p.value < 0.05, "*", 
                                        "ns")))) 
    #변수를 CHARACTER로 변경 
    ndata$sig <- as.character(ndata$sig) 
    ndata <- ndata %>% tibble()
    ndata_plot <- Contrast %>% plot()+
      geom_vline(xintercept = 0, linetype=2, col="Red")+
      labs(x="Confidence Range",
           title = paste0("Planned Contrast DV: ", DV_name)
      )
    
    
    #interaction
    variable1 <- as.vector(specs0[1])
    variable2 <- as.vector(specs0[2])
    EM0 <- EM
    colnames(EM0) <-c("mu","v1","v2","emmean", 
                      "SE","df","lower.CL","upper.CL")
    
    #interaction plot 
    library(ggrepel)
    interaction.plot <- EM0 %>% 
      ggplot(aes(x = v2, y = emmean))+
      geom_point(size=3)+
      geom_point(aes(shape= v1,color=v1), #,
                 show.legend = T, size= 4)+
      geom_line(aes(group = v1, linetype= v1), 
                linewidth=0.8, show.legend = F)+
      geom_text_repel(aes(label= round(emmean,2)),
                      vjust = vjust, hjust = hjust, size=5)+
      labs(x = variable2, 
           color = variable1,
           shape = variable1,
           y = DV_name,
           # subtitle = aov_formula0
      )+
      theme_bw()
    
    
    #result
    
    res=list(
      ANOVA =summary ,
      Estimate_marginal_mean = EM,
      Planned_contrast = ndata,
      APA = apa_report,
      confint_plot = ndata_plot,
      interaction_plot = interaction.plot
    )
    
  }
  dev.off()
  switch(type,
         all = res,
         anova = summary, 
         emmeans = EM,
         contrast = ndata,
         APA = apa_report,
         confint = ndata_plot,
         interaction = interaction.plot
  )
}


#사용법 
# 
# aov(len ~ supp*dose, data = ToothGrowth) %>% 
#   emmeans_contrast()
# 
# ToothGrowth %>% str()
# ToothGrowth$dose<- as.factor(ToothGrowth$dose)
# aov(len ~ factor(dose)*supp, data = ToothGrowth) %>% tidy()
# 
# aov(len ~ dose*supp, data = ToothGrowth) %>%
#   emmeans(specs = c("dose", "supp")) %>% data.frame()
# 
# 
# aov(len ~ dose*supp, data = ToothGrowth) %>% 
#   emmeans_contrast(type = "emmeans")
# 
# aov(len ~ supp*dose, data = ToothGrowth) %>% 
#   emmeans_contrast()
# 
# aov(AveragePleasantness ~ Gender*Condition, data = epl) %>% 
#   emmeans_contrast()
# 
# aov(AveragePleasantness ~ Gender*Condition, data = epl) %>% 
#   emmeans_contrast(type="emmeans")
# aov(AveragePleasantness ~ Gender*Condition, data = epl) %>% 
#   emmeans_contrast(type="contrast")
# 
# 
# aov(AveragePleasantness ~ Gender*Condition, data = epl) %>% 
#   emmeans_contrast("Gender","Condition", auto = F)
# 
# #바꾼 경우 
# aov(AveragePleasantness ~ Gender*Condition, data = epl) %>% 
#   emmeans_contrast("Condition", "Gender", auto = F)
# 



# 회귀 계수에 대한 해석 -----
interpritation<- function(dataset,
   pos= 1:3,
                          iv="",
                          dv="", 
                          coef=""){
  library(broom)
  library(knitr)
  # data1 <- summary(dataset)

  DV <- dataset$call$formula[2] %>% substring(1)
  df <- dataset$df


  data <- summary(dataset) %>% tidy()
  data.res <- data #%>% slice(pos)
  
  term <- data.res$term
  est <- data.res$estimate
  std <-  data.res$std.error
  t <-  data.res$statistic
  p <-  data.res$p.value
  p.value  <-  ifelse(data.res$p.value< .001,
                      "p < 0.001", data.res$p.value)
  intres <- ifelse(p < 0.05,"유의하였다","유의하지 않았다")

  ival <- iv   #변수명 입력된 데이터 
  dval <- dv   #변수명 입력된 데이터 

  
  for(i in 1:ncol(data)){
 
    report = sprintf(' %s%s에 대한 %s%s의 회귀분석 결과는 %s, est%s = %.3f, t(%.d) = %.3f, p = %.3f.',
            DV, dval, term, 
            ival, intres, 
            coef, 
            est ,
            df, 
            t, 
            p)

  }#for
  report 
  }
# 
# interpritation(lm(mpg~wt+hp+disp+qsec,data=mtcars),
# iv = "(X)",
# dv = "(Y)")[2]
# 
# lm(mpg~wt+hp+disp+qsec, data=mtcars) %>% summary()
# lm(mpg~wt+hp+disp+qsec, data=mtcars)$call$formula[3] %>% substring(1) 
  

#제외시킨 것 
# if(p<0.001){
#  sprintf(' %s%s에 대한 %s%s의 회귀분석 결과는 %s,
#           est%s = %.3f, t(%.d) = %.3f, %s.',
#           DV, dval, term, ival, 
#           intres, coef, est ,df, t, p.value)%>%
#     kable("pandoc",col.names = "[회귀분석 결과]")
# }else{
#   sprintf('%s%s에 대한 %s%s의  회귀분석 결과는 %s,
#           est%s = %.3f, t(%.d) = %.3f, p = %.3f.',
#           DV,dval, term, ival, 
#           intres, coef, est,df, t, p)%>%
#     kable("pandoc",col.names = "[회귀분석 결과]")
# } 

# mtcars %>% str()
# interpritation(fit13,2,iv="(X)", dv="(M)", coef = "(a)")
# interpritation(fit14,2,iv="(M)", dv="(Y)", coef = "(b1)")
# interpritation(fit14,3,iv="(W)", dv="(Y)", coef = "(b2)")
# interpritation(fit14,4,iv="(X)", dv="(Y)", coef = "(c')")
# interpritation(fit14,5,iv="(M:W)", dv="(Y)", coef = "(b3)")

# fit14$call$formula[2]%>% substring(1)
# lm(mpg~wt,data=mtcars) %>% tidy()
# interpritation(lm(mpg~wt,data=mtcars),2)
# interpritation(lm(mpg~wt*hp,data=mtcars),
#                pos = 1,
#                iv = "(X)",
#                dv = "(Y)")


#lavaan 신뢰구간의 유의성 표시 함수 --------------
# 2024.1.7
confint_plot <- function(data,
                         y="lhs", 
                         x="est",
                         lower="ci.lower",
                         upper="ci.upper",
                         size_text=12,
                         color="steelblue",
                         linewidth=2,
                         alpha=1,
                         vlinewidth=1,
                         vlinecolor="red"
){
  library(tidyverse)
  
  
  data%>% rename(
    lhs = all_of(y),
    est = all_of(x),
    ci.lower = all_of(lower),
    ci.upper = all_of(upper),
  )%>%
    tibble()%>% 
    ggplot(aes(y = lhs, 
               x = est, 
               xmin = ci.lower, 
               xmax = ci.upper, 
               height = 0)) + 
    geom_errorbarh(color=color, 
                   linewidth=linewidth, 
                   alpha=alpha)+
    geom_point(size=3) +
    geom_vline(xintercept = 0, 
               lty = 2, 
               color=vlinecolor, 
               linewidth = vlinewidth) +
    theme_bw()+
    theme(axis.text.y = element_text(size=size_text))
}



# 
# parameterEstimates(est.lav4) %>% 
#   filter(op==":=") %>%
#   select(lhs, op, rhs, est,se,t=z,ci.lower ,ci.upper, pvalue)%>%
#   p_mark_sig("pvalue")%>%
#   confint_plot()
# 
# 
# lm(mpg ~ wt + qsec, data = mtcars)%>%tidy(conf.int=T)%>%
#   confint_plot(x="estimate", y="term", 
#                lower="conf.low", upper="conf.high" )
# 




# 회계계수의 유의성 표시 plot ----------
lm_sig_plot <- function(lm_data,
                        size_text=12,
                        color="steelblue",
                        linewidth=1,
                        alpha=1){
  library(broom)
  
  lm_data%>% 
    broom::tidy(conf.int = TRUE) %>% 
    ggplot(aes(estimate, term, 
               xmin = conf.low, 
               xmax = conf.high, height = 0)) + 
    geom_errorbarh(color=color, linewidth=linewidth, alpha=alpha)+
    geom_point(size=3) +
    geom_vline(xintercept = 0, lty = 2) +
    theme_bw()+
    theme(axis.text.y = element_text(size=size_text))
}

# lm(satisfy ~ learn_convenience*upgrade, stat_onl) %>% 
#   lm_sig_plot(alpha=0.7, linewidth = 3)
# yhs2023_redata$개인적완벽성10r <- yhs2023_redata$개인적완벽성10 %>% inverseCoding()

# 회귀계수에 대한 비교 -----------

#test
ztest<- function(b1, se1, b2, se2){
  library(broom)
  
  z = (b1 - b2)/ sqrt(se1^2 + se2^2)
  p = 2*(1-pnorm(abs(z)))
  
  
  res = tidy(c(z.value= z, pvalue=p))
  colnames(res)=c("statistics","value")
  res 
}

# hp, wt
# ztest(b1= -0.032230,se1= 0.00892,   b2=-3.23 , se2= 0.796 )

# z test using data.frame(or tibble)
ztest_df <- function(data, row1=2, row2=3, 
                     rname1="", rname2="",
                     sel="row"){
  
  library(broom)
  library(tidyverse)
  data <- tidy(data)
  
  if(sel=="row"){
    b1 = data[row1, 2] %>%unlist()
    b2 = data[row2, 2] %>%unlist()
    se1 = data[row1, 3]%>%unlist()
    se2 = data[row2, 3]%>%unlist()
    se1_square = se1^2
    se2_square = se2^2 
    diff= b1 - b2
    
    z = (b1 - b2)/ sqrt(se1_square + se2_square)
    p = 2*(1-pnorm(abs(z)))
    statistics = tibble(b1,b2,se1,se2,diff, z, p) 
    #colnames
    colnames(statistics)=c(
      paste0(data[row1, 1] %>%unlist(),"_est"),
      paste0(data[row2, 1] %>%unlist(),"_est"),
      paste0(data[row1, 1] %>%unlist(),"_se"),
      paste0(data[row2, 1] %>%unlist(),"_se"),
      "diff","z","p.value")
    
    #selcting by variable name 
  }else if(sel=="term"){
    b1  = data %>% filter(term==rname1) %>% 
      dplyr::select(2) %>%unlist()
    b2  = data %>% filter(term==rname2) %>%
      dplyr::select(2) %>%unlist()
    se1 = data %>% filter(term==rname1) %>%
      dplyr::select(3) %>%unlist()
    se2 = data %>% filter(term==rname2) %>% 
      dplyr::select(3)%>%unlist()
    se1_square = se1^2
    se2_square = se2^2 
    diff= b1 - b2
    z = (b1 - b2)/ sqrt(se1_square + se2_square)
    p = 2*(1-pnorm(abs(z)))
    statistics = tibble(b1,b2,se1,se2, diff, z, p) 
    
    colnames(statistics)=c(
      paste0(rname1,"_est"),
      paste0(rname2,"_est"),
      paste0(rname1,"_se"),
      paste0(rname2,"_se"),
      "diff","z","p.value")
  }
  statistics
  
  
}
# 
# # default 
# lm(mpg ~ hp + wt + drat, mtcars) %>%
  # ztest_df()
# #selecting variable
# lm(mpg ~ hp + wt + drat, mtcars) %>%  
#   ztest_df(row1=2, row2=3)
# 
# # When other variables are selected and analyzed
# lm(mpg ~ hp + wt + drat, mtcars) %>%  
#   ztest_df(row1=2, row2=4)
# 
# # When variable-names are selected
# lm(mpg ~ hp + wt + drat, mtcars) %>%  
#   ztest_df(sel="term", rname1="hp", rname2="wt")
# 
# lm(mpg ~ hp + wt + drat, mtcars) %>%  
#   ztest_df(sel="term", rname1="hp", rname2="drat")



#메타구조방정식 상관행렬 만들기 ---------
metacorMat = function(data, startcol , var="")
{
  library(metaSEM)
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
    cormat[[i]] = vec2symMat(as.matrix(data[i,sj:ej]),diag = FALSE)
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

# cormat1 = metacorMat(rd2, startcol= 3, var=c("po","ne","se","jp"))

#
# rd2<- data.frame(
#   stringsAsFactors = FALSE,
#   study = c(1L,
#             2L,3L,4L,5L,6L,7L,8L,9L,10L,11L,12L,
#             13L,14L,15L,16L,17L,18L,19L,20L,
#             21L,22L,23L,24L,25L,26L,27L,28L,29L,
#             30L,31L,32L,33L,34L,35L,36L),
#   n = c(671L,
#         92L,73L,321L,214L,947L,321L,231L,
#         531L,567L,213L,928L,269L,593L,78L,983L,
#         1504L,247L,79L,875L,68L,201L,891L,
#         197L,431L,98L,179L,69L,301L,679L,273L,
#         254L,316L,781L,273L,268L),
#   po_ne = c(NA,
#             NA,-0.33,NA,NA,NA,NA,-0.32,NA,NA,NA,
#             NA,NA,NA,-0.21,-0.49,NA,-0.14,-0.29,
#             -0.27,NA,-0.37,NA,NA,-0.35,-0.17,NA,
#             -0.11,NA,NA,-0.41,NA,NA,NA,NA,-0.39),
#   po_se = c(NA,
#             0.37,0.29,0.16,0.41,NA,NA,0.21,0.59,
#             0.53,0.31,0.38,NA,NA,0.19,NA,NA,NA,
#             NA,0.21,0.07,0.57,0.45,NA,0.18,NA,
#             0.45,NA,NA,0.09,NA,NA,0.37,0.19,0.23,
#             NA),
#   po_jp = c(0.16,
#             0.39,0.09,0.07,NA,0.04,NA,NA,0.33,
#             0.11,NA,0.04,NA,NA,0.21,0.23,0.09,
#             0.11,0.31,0.11,-0.07,0.14,NA,0.19,0.13,
#             0.25,0.35,0.29,0.35,NA,0.27,0.19,NA,
#             NA,0.13,0.19),
#   ne_se = c(NA,
#             -0.31,NA,NA,NA,NA,NA,-0.19,NA,NA,NA,
#             NA,-0.49,NA,-0.45,NA,NA,NA,NA,-0.38,
#             NA,-0.39,NA,NA,-0.21,NA,NA,NA,NA,
#             -0.23,NA,NA,NA,NA,-0.44,NA),
#   ne_jp = c(NA,
#             -0.41,NA,NA,NA,-0.18,-0.11,NA,NA,NA,
#             NA,NA,-0.21,-0.15,-0.36,-0.29,NA,
#             -0.27,-0.29,-0.23,NA,-0.27,NA,NA,-0.19,
#             -0.07,NA,-0.31,NA,NA,0.07,NA,NA,NA,
#             -0.27,-0.05),
#   se_jp = c(NA,
#             NA,NA,0.07,NA,NA,NA,NA,0.27,0.07,NA,
#             NA,0.48,0.26,0.41,NA,NA,NA,NA,0.31,
#             0.19,0.34,NA,NA,0.19,NA,0.39,NA,
#             0.57,NA,NA,NA,NA,NA,NA,NA),
#   jobyear = c("f",
#               "f","s","s","s","s","f","f","s","s",
#               "s","s","f","f","f","f","f","f","s",
#               "s","s","s","s","f","s","f","f",
#               "f","f","f","f","s","s","s","f","f"),
#   wage = c(9L,
#            1L,1L,1L,4L,9L,2L,3L,3L,3L,2L,7L,
#            1L,8L,6L,3L,5L,7L,8L,4L,7L,1L,4L,
#            6L,7L,1L,9L,6L,7L,7L,7L,9L,2L,5L,
#            7L,3L)
# )


#메타sem 1차분석 해석결과------
poolcor_comment<- function(res){
  fs <- summary(res)
  n    <- fs$stat[1]
  chi2 <- fs$stat[2]
  df   <- fs$stat[3]
  p    <- fs$stat[4]
  rmsea <- fs$stat[7]
  rmsea.lower <- fs$stat[8]
  rmsea.upper <- fs$stat[9]
  srmr <- fs$stat[10]
  tli <- fs$stat[11]
  cfi <- fs$stat[12]

  chi2.dis <- ifelse(p <0.05,"유의하였다. 따라서 모형은 적절하지 않았다", "유의하지 않았다. 따라서 귀무가설을 기각할 수 없으므로  모형은 적절하였다")

  rmsea.dis <- ifelse(rmsea < 0.05,"적절하였다", "적절하지 않았다")
  srmr.dis <- ifelse(rmsea < 0.05,"적절하였다,", "적절하지 않았다")
  tli.dis <- ifelse(tli > 0.95,"적절하였다", "적절하지 않았다")
  cfi.dis <- ifelse(cfi > 0.95,"적절하였다", "적절하지 않았다")

  # if(p<0.001){"p < .001"}else

  cat(sprintf('1단계로 tssem을 이용하여 합동상관행렬(pooled correlation matrix)을 추정한 결과, 카이제곱분석은 %s, chi2(%.1d) = %.3f, p = %.3f. 모형이 모집단에 비교적 잘(reasonably well) 적합하는지 정도를 평가하는지 알아보는 평가지수(Brown, 2015)인 RMSEA값을 보면 모형은 %s, RMSEA = %.3f, 95％CI[%.3f, %.3f]. 표준화RMR(공분산 행렬의 차이), 표본상관행렬과 예측 상관행렬의 차이는 0.05보다 작으면 양호한 것으로 판단한다(Bentler, 1995). SRMR =%.3f으로 %s. CFI와 TLI는 모형적합도가 기저모형에 비해 향상되었는지 평가하는 지표로 기저모형은 모든 변수간의 공산분이 0인 모형을 나타내는데, CFI값은 1에 가까울수록 양호하며, 0.95이상이 되어야한다(HU & Bentler, 1999). CFI= %.3f로 %s. 마지막으로 TLI는 비표준화 적합지수(non- normal fit index; NNFI)로 CFI와 유사하다. TLI = %.3f로 %s. ',
              chi2.dis, df, chi2, p,
              rmsea.dis, rmsea, rmsea.lower, rmsea.upper,
              srmr, srmr.dis,
              cfi, cfi.dis,
              tli,tli.dis
  )
  )

}

# poolcor_comment(fixed1)


#metasem 구조모형 분석 적합도 코멘트----------
poolcor_comment2 <- function(res){
  fs <- summary(res)
  n    <- fs$stat[1]
  chi2 <- fs$stat[2]
  df   <- fs$stat[3]
  p    <- fs$stat[4]
  rmsea <- fs$stat[9]
  rmsea.lower <- fs$stat[10]
  rmsea.upper <- fs$stat[11]
  srmr <- fs$stat[12]
  tli <- fs$stat[13]
  cfi <- fs$stat[14]

  chi2.dis <- ifelse(p <0.05,"유의하였다. 따라서 모형은 적절하지 않았다", "유의하지 않았다. 따라서 귀무가설을 기각할 수 없으므로 모형은 적절하였다")

  rmsea.dis <- ifelse(rmsea < 0.05,"적절하였다", "적절하지 않았다")
  srmr.dis <- ifelse(rmsea < 0.05,"적절하였다,", "적절하지 않았다")
  tli.dis <- ifelse(tli > 0.95,"적절하였다", "적절하지 않았다")
  cfi.dis <- ifelse(cfi > 0.95,"적절하였다", "적절하지 않았다")

  # if(p<0.001){"p < .001"}else


  cat(sprintf('tssem을 이용하여 구조모형의 적합도를 추정한 결과, 카이제곱분석은 %s, chi2(%.1d) = %.3f, p = %.3f. \n모형이 모집단에 비교적 잘(reasonably well) 적합하는지 정도를 평가하는지 알아보는 평가지수(Brown, 2015)인 RMSEA값을 보면 모형은 %s, RMSEA = %.3f, 95％CI[%.3f, %.3f]. \n표준화RMR(공분산 행렬의 차이), 표본상관행렬과 예측 상관행렬의 차이는 0.05보다 작으면 양호한 것으로 판단한다(Bentler, 1995). SRMR = %.3f으로 %s. \nCFI와 TLI는 모형적합도가 기저모형에 비해 향상되었는지 평가하는 지표로 기저모형은 모든 변수간의 공산분이 0인 모형을 나타내는데, CFI값은 1에 가까울수록 양호하며, 0.95이상이 되면 적합하다(HU & Bentler, 1999). CFI= %.3f로 %s. 마지막으로 TLI는 비표준화 적합지수(non-normal fit index; NNFI)로 CFI와 유사하다. TLI = %.3f로 %s. ',
              chi2.dis, df, chi2, p,
              rmsea.dis, rmsea, rmsea.lower, rmsea.upper,
              srmr, srmr.dis,
              cfi, cfi.dis,
              tli,tli.dis
  )
  )

}

# poolcor_comment2(random_sem2)





#빈도출력 함수
#빈도출력 함수

## 

propTable <- function(x, method="post"){
  library(dplyr)
  library(forcats)
  
  if(method=='post'){ #fct_count()로 빈도분석을 실시한 경우 
    x%>% mutate('prop'= paste0(round(p*100,2),"%"),
                'rank'= rank(n, ties.method = "random")) %>% 
      # ranks duplicates in random order
      dplyr::select(f,n, prop, rank)
    
  }else if(method=="pre"){
    #fct_count()로 빈도분석을 실시하지 않은 경우 
    x%>%fct_count( prop = T) %>% 
      mutate('prop'= paste0(round(p*100,2),"%"),
             'rank'= rank(n, ties.method = "random")) %>% 
      # ranks duplicates in random order
      dplyr::select(f,n, prop, rank)
  }
}
# fct_count(ca_onl1$쌍방향, prop = T) %>% propTable()
# ca_onl1$쌍방향%>% propTable("pre) :사전에 빈도분석을 하지 않은 경우 


#Freq_table  빈도출력 함수------------
# 
Freq_table <- function(data,format="markdown",  #df: dataframe
 title="빈도분석결과",
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
 #결과정리 
  if(format=="markdown"){
   res %>%
      kable(caption = title, format = format)
  }else if(format=="df"){
    res
  }
} 

#usage
# raw6hs$성별 %>% Freq_table("성별 빈도분석", arrange = "decs")
#
# raw6hs$학년 %>% Freq_table("학년별 빈도분석")

#Freq_table_data 대응분석용 빈도를 계산하여 정리 ------
Freq_table_data <- function(data, 
                            name="park", #자동으로 입력
                            row=6,  #같은 levels의 개수를 입력 
                            col=15, # 변수의 개수를 입력 
                            col_name="", #name을 manual로 하고 입력
                            row_name="",
                            lang="kor"
){
  library(forcats)
  library(dplyr)
  #matrix data generation 
  
  
  if(name=="manual"){
    
    Mat <- matrix(0, row, col)
    
    for(i in 1: col){
      ds <- data[, i] %>% table()
      # da <- as.matrix(ds)
      # print(ds[,2])
      
      Mat[, i] = ds
      
    }
    colnames(Mat) = col_name
    rownames(Mat) = row_name
    Mat
  }else if(name=="auto"){
    # 임의로 이름을 붙이기 
    col <- ncol(data)
    row <- length(levels(data[,1]))
    Mat <- matrix(0, row, col)
    
    for(i in 1: col){
      ds <- data[, i] %>% table()
      # da <- as.matrix(ds)
      # print(ds[,2])
      
      Mat[, i] = ds
      
    }
    colnames(Mat) = rep(paste0("C",1:col))  #임의로 이름 붙이기 
    rownames(Mat) = rep(paste0("R",1:row))  #임의로 이름 붙이기 
    Mat
  }else if(name=="park"){
    
    col <- ncol(data)
    row <- length(levels(data[,1]))
    Mat <- matrix(0, row, col)
    
    for(i in 1: col){
      ds <- data[, i] %>% table()
      # da <- as.matrix(ds)
      # print(ds[,2])
      
      Mat[, i] = ds
      
    }
    
    kor=c("쌍방향소통","실시간소통","발표도구우수","소통질문","토론",
          "복습유리","시청방법","강의접속","쉬운조작","UI편리성","PC선호",
          "mobile선호","결제요구","추천앱","기술보완필요")
    eng=c("duplex.Interactive","live.broadcast","presentation",
          "communication","debate","review","watch","access.App",
          "easyofUse","UI","PC.like","mobile",
          "settlement","recommend","need_repair")
    tool=c("ZOOM","Webex","googleMEET","Youtube","naveBand","prism")
    
    if(lang=="kor"){
      colnames(Mat) = kor
      rownames(Mat) = tool
    }else if(lang =="eng"){
      colnames(Mat) = eng
      rownames(Mat) = tool}
    Mat
  }
}


# 
# ca_onl1[1:5,]%>% Freq_table_data()
#자동을 계산 

# 
# # 
# ca_onl1[,1:6]%>% Freq_table_data()
# #자동을 계산 
# ca_onl1 %>% Freq_table_data()
# 
# 
# zz1=c("쌍방향소통","실시간소통","발표도구우수","소통질문","토론",
#       "복습유리","시청방법","강의접속","쉬운조작","UI편리성","PC선호",
#       "mobile선호","결제요구","추천앱","기술보완필요")
# zz=c("duplex.Interactive","live.broadcast","presentation",
#      "commucation","debate","review","watch","access.App",
#      "easyofUse","UI","PC.like","mobile",
#      "settlement","recommend","need_repair")
# zz2=c("ZOOM","Webex","googleMEET","Youtube","naveBand","prism")
# 
# ca_onl1 %>% Freq_table_data(name="manual",
#                             col_name = zz1,
#                             row_name = zz2)

# correspondence analysis chisq.test ------
advanced.chisq.test<-function(cri, option=TRUE, B= 5000, data_show=FALSE){
  freqMatrix <- cri[rowSums(cri)>0,]
  df = (nrow(cri)-1)*(ncol(cri)-1)
  dof_data = data.frame(
    row = nrow(cri),
    col = ncol(cri),
    row_df= nrow(cri)-1,
    col_df= ncol(cri)-1,
    dof_r_by_c= df
  ) %>% as_tibble()
  
  
  if(option == TRUE){
    cat("정확검정을 위해 Monte Carlo simulation(B=5,000)이 실행되었습니다(park, 2020). \n")
    chisqT= chisq.test(freqMatrix, simulate.p.value = TRUE, B=B)
  }
  else{chisq.test(freqMatrix)
    cat("아래에 나오는 경고메시지를 없애려면 
          Monte Carlo simulation, option=TRUE로 실행하세요. \n")}
  
  if(data_show == TRUE){
    res= list(chisqT,
              dof=dof_data,
              mat=cri)
    res}else if(data_show == FALSE){
      res= list(chisqT,
                dof=dof_data)
    }
  res
}

# online_freq %>% class()
# advanced.chisq.test(online_freq, option=T,B=5000)
# advanced.chisq.test(online_freq, data_show = T)

#NA to zero-----------
all_na_zero <- function(data, fun="extend"){
  data <- mutate(data, across(everything(), ~replace_na(.x, 0)))  # base function
  data_base <- replace(data, is.na(data), 0)
  
  # replace(data, is.na(data), 0)   #
  switch(fun,
         extend = data,
         base = data_base
  )
}  

#결측치(missing data) 검사와 해결------------
#결측치가 있다면 몇개가 있는지 출력
missCheck <- function(data, plot=FALSE){
  library(dplyr)
  library(VIM)
  aggr_data_total  <-aggr(data, prop=FALSE, numbers= TRUE, plot = plot)$missings
  aggr_data  <-aggr(data, prop=FALSE, numbers= TRUE, plot=plot)$missings%>%
    filter(Count !=0)

  library(Hmisc)
  #impute
  datacount_total <-colSums(!is.na(data))
  datacount <-colSums(!is.na(data))


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


# sleep %>% str()
# sleep %>% missCheck()


# 결측치가 있는지 검사하여 plot과 내용을 보여주기
missCheckTotal <- function(data,plot=TRUE){
  library(dplyr)
  library(VIM)
  aggr_data_total  <-aggr(data, prop=FALSE, numbers= TRUE, plot=plot)$missings
  aggr_data  <-aggr(data, prop=FALSE, numbers= TRUE, plot=plot)$missings%>% filter(Count !=0)

  library(Hmisc)
  #impute
  datacount_total <-colSums(!is.na(data))
  datacount <-colSums(!is.na(data))


  table_data<-cbind.data.frame(aggr(data, prop=FALSE, numbers= TRUE, plot=plot)$missings[1], V_count=colSums(!is.na(data)) %>% as.data.frame(),
                aggr(data, prop=FALSE, numbers= TRUE, plot=plot)$missings[2]
  )

  table_data_miss <- table_data %>% dplyr::filter(Count != 0)
  # res <- table_data
  res<- list(    # aggr_data,
    # datacount,
    table_data,
    table_data_miss
  )
  res
}


# aggr(yhs2023_redata_rrs[2] , prop=FALSE, numbers= TRUE, plot = FALSE)$missings[2]

# yhs2023_redata_rrs[2] %>% missCheck()
# colSums(!is.na(yhs2023_redata_rrs[2]))

#결측된 데이터를 평균으로 대체하는 함수 ---------
missfill<- function(data, fn= mean){
  library(Hmisc) #평균값으로 대체하기
  data <- Hmisc::impute(data,
                        fun=fn)
  data
}

# sleep$NonD %>% missfill()
# sleep$Exp %>% missfill()

#결측치 판단후 결측치가 있으면 채우는 함수: 메인함수
# 결측이 없으면 원래 데이터 출력
missfillin <- function(data, fn=mean){
  library(VIM)
  library(Hmisc)

  data<- as.data.frame(data)
  name <- colnames(data)

  if(aggr(data, prop=FALSE, numbers= TRUE, plot = FALSE)$missings[2]!=0){
    data <-  Hmisc::impute(unlist(data), fn(unlist(data), na.rm=T))
    # data <- as_tibble(data)
  }else{
    data
  }
  data
}
# sleep$NonD %>% missfillin()
# sleep$NonD %>% missfillin(fn=median)
# sleep$Exp %>% missfillin()


# 결측치 검사후 채우고 원래 결측이 없으면 문제없으면 알려주기
# missfillinData<-function(data)
  # 결측치 검사후 채우고 원래 결측이 없으면 문제없으면 알려주기
  missfillinData<-function(data, fn=mean){
    library(VIM)
    library(Hmisc)

    # if(is.list(data)){
    # data <- unlist(data)}
    data<- as.data.frame(data)

    name <- colnames(data)

    if(aggr(data, prop=FALSE, numbers= TRUE, plot = FALSE)$missings[2]!=0){
      data <-  Hmisc::impute(unlist(data), fn(unlist(data), na.rm=T))
      # data <- as_tibble(data)
    }else{
      data <- aggr(data, prop=FALSE, numbers= TRUE)$missings %>% as.data.frame()
      cat("missing data가 없습니다")
      data<- as_tibble(data)
    }
    data
  }


#
# sleep %>% missfillinData() #error
# sleep$NonD %>% missfillinData(fn=median)
# sleep$Exp %>% missfillinData()

#결측을 한번에 채우기
# yhs2023_redata_rrs
# missfillinFor<- function(data, iter=""){
#
#   for(i in iter){
#     data[i] = data[i] %>% missfillin()
#   }
#   # colSums(!is.na(data))
#   data
# }
# 적용하기
# yhs2023_redata_rrs<-yhs2023_redata_rrs %>% missfillinFor(iter=1:103)

# aggr(yhs2023_redata_rrs$개인적완벽성7, prop=FALSE, numbers= TRUE, plot = FALSE)$missings[2]!=0


# 확인#####
# 결측을 한번에 채우기 ----------------------
## yhs2023_redata_rrs
missfillinFor <- function(data, fn=mean){

  for(i in 1:ncol(data)){
    data[i] = data[i] %>% missfillin(fn=fn)
  }
  # colSums(!is.na(data))
  data
}

# sleep %>% str()
# sleep %>% ncol()

# sleep %>% missfillinFor()
# sleep %>% missfillinFor(fn=median)


# 잠재프로파일분석 ----------------------------------------------------------------
# library(mclust)
# 모형적합
# 01LPA 모형적합 -explore_model_fit  model data function--------
explore_model_fit <- function(df, n_profiles_range = 1:9, #profile n
                              model_names = c("EII", "VVI", "EEE", "VVV")) {
  library(tidyverse, warn.conflicts = FALSE)
  library(mclust)
  library(hrbrthemes) #Additional Themes and Theme Components for 'ggplot2'

  x <- mclustBIC(df, G = n_profiles_range, modelNames = model_names)
  y <- x %>%
    as.data.frame.matrix() %>%
    rownames_to_column("n_profiles") %>%
    rename(`Constrained variance, fixed covariance` = EII,
           `Freed variance, fixed covariance` = VVI,
           `Constrained variance, constrained covariance` = EEE,
           `Freed variance, freed covariance` = VVV)

  y
}


#NEW
explore_model_fit2 <- function(df, n_profiles_range = 1:9, #profile n
                               modelNames =c("EEV","EEI","EEE","EII")) {
  library(tidyverse, warn.conflicts = FALSE)
  library(mclust)
  library(hrbrthemes) #Additional Themes and Theme Components for 'ggplot2'

  x <- mclustBIC(irb_df, G = n_profiles_range, modelNames = modelNames)
  y <- x %>%
    as.data.frame.matrix() %>%
    rownames_to_column("n_profiles") %>%
    rename(`[EEV:ellipsoidal, equal volume and equal shape]`=EEV,
           `[EEI:diagonal, equal volume and shape]` = EEI,
           # `diagonal, varying volume and shape` = VVI,
           `[EEE:ellipsoidal, equal volume, shape, and orientation]` = EEE,
           # `ellipsoidal, varying volume, shape, and orientation` = VVV
           `[EII:spherical, equal volume]` = EII
    )

  y
}
# 실행절차
# data(iris)
# df <- dplyr::select(iris, -Species)
# df %>% explore_model_fit( n_profiles_range = 1:4)
#

# irb_df %>% explore_model_fit2()


#02 LPA Plot function -----------------
LPA_BIC_plot <- function(data, n_profiles_range = 1:9){
  library(forcats)

  #long data trnasformation
  to_plot <- explore_model_fit(data, n_profiles_range = n_profiles_range)%>%
    gather(`Covariance matrix structure`, val, -n_profiles) %>%
    mutate(`Covariance matrix structure` = as.factor(`Covariance matrix structure`), val = abs(val))

  # C- variable arrange
  to_plot$`Covariance matrix structure` <- fct_relevel(
    to_plot$`Covariance matrix structure`,
    "Constrained variance, fixed covariance",
    "Freed variance, fixed covariance",
    "Constrained variance, constrained covariance",
    "Freed variance, freed covariance")

  #plotting
  gg <-ggplot(to_plot, aes(x = n_profiles, y = val,
                           color = `Covariance matrix structure`,
                           group = `Covariance matrix structure`)) +
    geom_line(linewidth = 1) +
    geom_point(size=2) +
    ylab("BIC (smaller value is better)") +
    guides( linewidth="none", alpha="none")+
    theme_bw()

  res= list(to_plot, gg)
  res

}

#NEW LPA dis plot
LPA_BIC_plot2 <- function(data,
                          n_profiles_range = 1:9, #범위
                          modelNames=NULL,  #모델선정
                          v= 0, #모델개수한정라인
                          point_size=1.5, #데이터구분점
                          v_linewidth=0.6, #profile cut
                          l_linewidth=1, #line width
                          title="적절한 개수의 profiles 판정 ",
                          flip=TRUE,
                          line=FALSE
                          ){
  library(forcats)

  #long data trnasformation
  if(flip== TRUE){
  to_plot <- explore_model_fit2(irb_df, n_profiles_range = n_profiles_range,
                              modelNames = modelNames)%>%
    pivot_longer(names_to = "Covariance_matrix_str",
                 values_to =  "val", cols = - n_profiles) %>%
    mutate("Covariance.matrix.str" = as.factor(Covariance_matrix_str),
           "val" = val) %>% dplyr::select(1,4, 3) %>% as.data.frame()

  }else if(flip==FALSE){
    to_plot <- explore_model_fit2(irb_df, n_profiles_range = n_profiles_range,
                                  modelNames = modelNames)%>%
      pivot_longer(names_to = "Covariance_matrix_str",
                   values_to =  "val", cols = - n_profiles) %>%
      mutate("Covariance.matrix.str" = as.factor(Covariance_matrix_str),
             "val" = abs(val)) %>% dplyr::select(1,4, 3) %>% as.data.frame()

  }else if(filp=="none"){
          stop("end revise ")
        }

#plotting

  if(line==TRUE){
  gg <-ggplot(to_plot, aes(x = n_profiles, y = val

                           )) +
    geom_line(aes(group = Covariance.matrix.str,
                  color = Covariance.matrix.str,
                  linetype= Covariance.matrix.str),linewidth = l_linewidth) +
    geom_point(aes(group = Covariance.matrix.str), size=point_size) +# 데이터사이즈
    ylab("BIC (smaller value is better)") +
    xlab("Number of Components(profiles) ")+

    guides( linewidth="none", alpha="none")+
    geom_vline(xintercept = v, colour="red",
               linetype = "longdash", linewidth=v_linewidth, # cutline
               alpha=.4)+
    labs(title=title)+
    theme_bw()
  }else if(line==FALSE){
    gg <-ggplot(to_plot, aes(x = n_profiles, y = val

    )) +
      geom_line(aes(group = Covariance.matrix.str,
                    color = Covariance.matrix.str #,
                    # linetype= Covariance.matrix.str
                    ),linewidth = l_linewidth) +
      geom_point(aes(group = Covariance.matrix.str),
                 size=point_size) +# 데이터사이즈
      ylab("BIC (smaller value is better)") +
      xlab("Number of Components(profiles) ")+
      guides( linewidth="none", alpha="none")+
      geom_vline(xintercept = v, colour="red",
                 linetype = "longdash", linewidth=v_linewidth, # cutline
                 alpha=.4)+
      labs(title=title)+
      theme_bw()
  }

  res= list(to_plot, gg)
  res

}

# irb_df %>% LPA_BIC_plot2(n_profiles_range =  1:9,
#                          v=4,
#                          modelNames = c("EEV","EEI","EEE","EII","VEI"),
#                          l_linewidth=1.2,
#                          flip=TRUE)
  # modelNames = c("EEV","EEI","EEE","EII","VEI"))





# profile 의 개수를 결정하는 함수
LPA_BIC_plot3 <-  function(data,
                           n_profiles=1:9,
                           modelNames =NULL,
                           # c("EEV","EEI","EEE","EII"),
                           v=0
){
  library(mclust)
  bic_data <- mclustBIC(data, G = n_profiles, modelNames =modelNames)

  gg<- plot(bic_data )
  abline(v=v, lty=2, col="red")
  grid()
  res=list(bic_data, gg)
  res

}

 # irb_df %>% LPA_BIC_Plot3( modelNames = c("EEV","EEI","EEE","EII","VEI"),v= 4)
 # irb_df %>% LPA_BIC_Plot3(v= 4)

#모형의 BIC를 중심으로 4가지형태의 그래프 그리기
# df %>% LPA_BIC_plot(n_profiles_range = 1:4)

# 빨간색에서 보라색으로 갈수록 모델의 제약이 줄어듭니다(더 자유로워짐). 자유롭게 추정된 잔차 분산 및 공분산이 있는 2개 또는 3개의 프로파일(혼합 성분) 모델 또는 제한된 잔차 공분산 및 분산이 있는 4개 프로파일 모델이 가장 잘 맞는 것으로 보입니다(BIC 해석 기준).
# 이를 감안할 때 우리는 자유롭게 추정된 잔차 분산 및 공분산이 있는 3개 프로필 모델과 같은 모델을 적합(및 검사)할 수 있습니다.
# From red to purple, the models become less constrained (more free). It appears that a two or three profile (mixture component) model with freely-estimated residual variances and covariances, or a four profile model with constrained residual covariances and variances, fit best (based on interpreting the BIC).
# Given this, we can fit (and inspect) a model, say, the three profile model with freely-estimated residual variance and covariances.


# 03 profile 생성함수 --------------
create_profiles_mclust <- function(df,
                                   n_profiles=3,
                                   variance_structure = "freed",
                                   covariance_structure = "freed"){

  library(tidyverse, warn.conflicts = FALSE)
  library(mclust)
  library(hrbrthemes)

  if (variance_structure == "constrained" &
      covariance_structure == "fixed") {

    model_name <- "EEI"

  } else if (variance_structure == "freed" &
             covariance_structure == "fixed") {

    model_name <- "VVI"

  } else if (variance_structure == "constrained" &
             covariance_structure == "constrained") {

    model_name <- "EEE"

  } else if (variance_structure == "freed" &
             covariance_structure == "freed") {

    model_name <- "VVV"

  } else if (variance_structure == "fixed") {

    stop("variance_structure cannot equal 'fixed' using this function; change this to 'constrained' or 'freed' or try one of the models from mclust::Mclust()")

  }

  x <- Mclust(df, G = n_profiles, modelNames = model_name)

  print(summary(x))

  dff <- bind_cols(df, classification = x$classification)

  proc_df <- dff %>%
    mutate_at(vars(-classification), scale) %>%
    group_by(classification) %>%
    summarize_all(list(mean)) %>%
    mutate(classification = paste0("Profile ", 1:n_profiles)) %>%
    mutate_at(vars(-classification), function(x) round(x, 3)) %>%
    rename(profile = classification)

  return(proc_df)

}

# irb_df %>% create_profiles_mclust(n_profiles =  4,
#                   variance_structure = "constrained",
#                   covariance_structure = "constrained") %>% kable()

# NEW generarion ------
create_profiles_mclust2 <- function(df,
                                   n_profiles=3,
                                   model_name=NULL
                                   # ,
                                   # option=FALSE
                                   ){

  library(tidyverse, warn.conflicts = FALSE)
  library(mclust)
  library(hrbrthemes)


  x <- Mclust(df, G = n_profiles, modelNames = model_name)

  print(summary(x))

  dff <- bind_cols(df, classification = x$classification)

  proc_df <- dff %>%
    mutate_at(vars(-classification), scale) %>%
    group_by(classification) %>%
    summarize_all(list(mean)) %>%
    mutate(classification = paste0("Profile ", 1:n_profiles)) %>%
    mutate_at(vars(-classification), function(x) round(x, 3)) %>%
    rename(profile = classification)

  return(proc_df)

  # if(option==TRUE){
  #   res=list(proc_df, dff)
  #   res
  # }else if(option==FALSE){
  #  return(proc_df)
  # }

}


#LPA결과 데이터 생성
create_profiles_mclust3 <- function(df,
                                    n_profiles=3,
                                    model_name=NULL
                                    # ,
                                    # option=FALSE
){

  library(tidyverse, warn.conflicts = FALSE)
  library(mclust)
  library(hrbrthemes)


  x <- Mclust(df, G = n_profiles, modelNames = model_name)

  print(summary(x))

  dff <- bind_cols(df, classification = x$classification)

  proc_df <- dff %>%
    mutate_at(vars(-classification), scale) %>%
    group_by(classification) %>%
    summarize_all(list(mean)) %>%
    mutate(classification = paste0("Profile ", 1:n_profiles)) %>%
    mutate_at(vars(-classification), function(x) round(x, 3)) %>%
    rename(profile = classification)

  res=list(X, dff, proc_df)
  res

}

#LPA 결과 데이터 저장 ---------------
# irb_df %>% create_profiles_mclust()
# mod <- irb_df %>% create_profiles_mclust3(4,model_name = "EEE")
# write_csv(mod[[2]], file = "irb_lpa.csv")

# irb_df %>% create_profiles_mclust()
# irb_df %>% create_profiles_mclust2(4,model_name = "EEE")

# 실습 함수를 이용해서 그리기

# df %>% create_profiles_mclust( n_profiles= 3,
#                                variance_structure = "freed",
#                                covariance_structure = "freed")

# We can then plot the mean values for the variables used to estimate the model for each of the two profiles. Of course, there are other models that we may want to inspect with different covariance matrix structures or profile numbers.
# 그런 다음 두 프로필 각각에 대한 모델을 추정하는 데 사용되는 변수의 평균값을 그릴 수 있습니다. 물론 다른 공분산 행렬 구조나 프로필 번호로 검사할 수 있는 다른 모델이 있습니다.


#04 profile별 변수에 대한 plot ------------
profile_plot <- function(data, n_profiles=3,
                         var_str = "freed",
                         covar_str = "freed"
){
  library(tidyverse, warn.conflicts = FALSE)
  library(mclust)
  library(hrbrthemes)

  mean_data <-data %>% create_profiles_mclust( n_profiles=n_pro,
                                               variance_structure = var_str,
                                               covariance_structure = covar_str)

  gg  <-data %>% create_profiles_mclust( n_profiles=n_pro,
                                         variance_structure = var_str,
                                         covariance_structure = covar_str)%>%
    gather(key, val, -profile) %>%
    ggplot(aes(x = profile, y = val, fill = key, group = key)) +
    geom_col(position = "dodge") +
    ylab("Z-score") +
    xlab("") +
    scale_fill_discrete("") +
    theme_ipsum_rc()

  res=list(mean_data, gg)
  res

}

#새롭게 만들어진 함수 ---------
profile_plot2 <- function(data, n_profiles=3,
                          model_name=NULL,
                          view="pair"

                          ){
  library(tidyverse, warn.conflicts = FALSE)
  library(mclust)
  library(hrbrthemes)

  mean_data <-data %>%
    create_profiles_mclust2( n_profiles=n_profiles,
                            model_name = model_name)



  # Mclust(df, G = n_profiles, modelNames = model_name)

  gg  <- #data %>%
    # create_profiles_mclust2( n_profiles=n_profiles, model_name= model_name)
    mean_data %>%
    gather(key, val, -profile) %>%
    ggplot(aes(x = profile, y = val, fill = key, group = key)) +
    geom_col(position = "dodge") +
    # geom_line(aes(x = profile, y = val))+
    ylab("Z-score") +
    xlab("(b)") +
    theme(axis.text.x = element_text(size=14))+
    scale_fill_discrete("") +
    labs(title = "LPA모형추정 잠재Profile 평균의 표준화 점수")+
    theme_bw()

    #비표준화 데이터
  raw_data = Mclust(data, G=n_profiles, modelNames = model_name)
# raw_data <- mean_data
  raw_mean_data <- raw_data$parameters$mean %>%   as_tibble()

  names(raw_mean_data) = str_c("profile",1:n_profiles)
  raw_mean_data$factor= colnames(data)
  raw_mean_data<-raw_mean_data %>% dplyr::select(factor,1:ncol(raw_mean_data)-1)

  gg2<-raw_mean_data %>% pivot_longer(
    cols=-factor,
    names_to = "profile",
    values_to = "val" ) %>%
    ggplot(aes(x = profile, y = val, fill = factor, group = factor)) +
    geom_col(position = "dodge") +
    # geom_line(aes(x = profile, y = val))+
    ylab("Raw-score") +
    xlab("(a)") +
    theme(axis.text.x = element_text(size=14))+
    scale_fill_discrete("") +
    labs(title = " LPA모형추정 잠재Profile 평균의 원점수 ")+
    theme_bw()

  ggg<- gridExtra::grid.arrange(gg2, gg, ncol=2)


  if(view=="pair"){
  res=list(std=mean_data, est=raw_mean_data, graph=ggg)
   }else if(view=="each"){
  res=list(std=mean_data, est=raw_mean_data, gg2, gg)
   }
  res

  }
# profile plot :EEI:대각
# irb_df %>%profile_plot2(n_profiles = 4, model_name = "EEE")
#
#
#
# # profile plot :EEE: 타원 **
# irb_df %>%profile_plot(n_profiles = 4,
#                        var_str = "constrained",
#                        covar_str = "fixed")
# #NEW
# irb_df %>%profile_plot2(n_profiles  = 4,model_name = "EEE")
#
#

#
#
#
# #04 profile별 변수에 대한 plot ------------
# profile_plot <- function(data, n_profiles=3,
#                          var_str = "freed",
#                          covar_str = "freed"
# ){
#   library(tidyverse, warn.conflicts = FALSE)
#   library(mclust)
#   library(hrbrthemes)
#
#   mean_data <-data %>% create_profiles_mclust( n_profiles=n_pro,
#                                                variance_structure = var_str,
#                                                covariance_structure = covar_str)
#
#   gg  <-data %>% create_profiles_mclust( n_profiles=n_pro,
#                                          variance_structure = var_str,
#                                          covariance_structure = covar_str)%>%
#     gather(key, val, -profile) %>%
#     ggplot(aes(x = profile, y = val, fill = key, group = key)) +
#     geom_col(position = "dodge") +
#     ylab("Z-score") +
#     xlab("") +
#     scale_fill_discrete("") +
#     theme_ipsum_rc()
#
#   res=list(mean_data, gg)
#   res
#
# }
#
#새롭게 만들어진 함수 ---------
# profile_plot2 <- function(data, n_profiles=3,
#                           model_name=NULL){
#   library(tidyverse, warn.conflicts = FALSE)
#   library(mclust)
#   library(hrbrthemes)
#
#   mean_data <-data %>%
#     # create_profiles_mclust2( n_profiles=n_profiles,
#     #                         modelName = model_name)
#     Mclust(df, G = n_profiles, modelNames = model_name)
#
#   gg  <- data %>%
#     create_profiles_mclust2( n_profiles=n_profiles, modelNames= model_name)%>%
#     gather(key, val, -profile) %>%
#     ggplot(aes(x = profile, y = val, fill = key, group = key)) +
#     geom_col(position = "dodge") +
#     # geom_line(aes(x = profile, y = val))+
#     ylab("Z-score") +
#     xlab("") +
#     scale_fill_discrete("") +
#     theme_ipsum_rc()
#
#   res=list(mean_data, gg)
#   res
# }
# }#함수를 이용하여 그리기
# df %>% profile_plot()

# One big question: Are the residual covariance structures correctly specified? There are a lot of possible specifications (see help file here). I think they are right, based on their definitions, inspecting their covariance matrices, and inspecting their plots. But they might not be.
# 하나의 큰 질문: 잔차 공분산 구조가 올바르게 지정되었습니까? 가능한 많은 사양이 있습니다(여기 도움말 파일 참조). 정의에 따라 공분산 행렬을 검사하고 플롯을 검사하는 것이 옳다고 생각합니다. 하지만 그렇지 않을 수도 있습니다.


# iris LPA
# # model fit
# df %>% explore_model_fit( n_profiles_range = 1:3)
# # profile LPA plot
# df %>% LPA_BIC_plot()
# # profile generation
# df %>% create_profiles_mclust(n_profiles =  3,
#                               variance_structure = "freed",
#                               covariance_structure = "freed")
# # profile plot
# df %>%profile_plot(n_pro = 3)
#
#
#
#
# # mtcars LPA
#
# m_df<- mtcars %>% dplyr::select(disp, hp, drat, qsec,wt, mpg)
#
#
# # model fit
# m_df %>% explore_model_fit( n_profiles_range = 1:3)
# # profile LPA plot
# m_df %>% LPA_BIC_plot()
# # profile generation
# m_df %>% create_profiles_mclust(n_profiles =  3, variance_structure = "freed", covariance_structure = "freed")
# # profile plot
# m_df %>%profile_plot(n_pro = 3)



# 잠재계층분석 LCA --------------------------------------------------------------

#모델집계및 시각화 함수 -----------


vizLCA = function(model_lca, #poLCA 결과데이터 
                  type="res2",
                  text_angle=90,
                  class_name=NULL,
                  intercept = 10,
                  Break =c(0, 20,30, 50, 100),
                  size_y_text=10,
                  sel=1:20){
  
  library(tidyverse)
  
  model_lca  <-  model_lca
  # 문항 반응 확률 
  item_resp_prob = model_lca$probs
  
  class_number = nrow(item_resp_prob[[1]])
  
  
  # 시각화 ------- 클래스 체크 
  
  p = bind_rows(Class = paste0("Class",1:class_number),
                class_population_shares= model_lca$P) #%>% 
  # arrange(class_population_shares)
  
  
  #pattern 
  pattern_grp0 = model_lca$predcell
  pattern_grp = model_lca$predcell %>%
    arrange(desc(observed))
  
  
  #패턴만들기 
  ddd = list()
  for(i in 1:nrow(model_lca$predcell)){
    ddd = bind_rows(
      ddd, tibble( 
        pattern = str_c("(", 
                        paste0(model_lca$predcell[i,
                                                  1: (ncol(model_lca$predcell)-2)],  
                               collapse = ",") ,")"  )))
  }
  # ddd
  pattern_data = bind_cols(pattern = ddd, 
                           n = model_lca$predcell$observed) %>%
    arrange(desc(n)) %>%slice(sel)
  
  #visualization pattern 
  g_patten = pattern_data%>%
    ggplot(aes(x = reorder(pattern, n) , y = n))+
    geom_bar(stat="identity")+
    geom_hline(yintercept = intercept, color= "red", linetype=2)+
    geom_text(aes(label = n), hjust =-0.4)+
    scale_y_continuous(breaks = Break )+
    coord_flip()+
    labs(x="반응패턴\n", y = "빈도",
         title = "Frequency of occurrence by reaction pattern")+
    theme_bw()
  
  
  
  N = nrow(model_lca$predcell)
  log_likelihood =  model_lca$llik
  
  model_fit = bind_cols(AIC = model_lca$aic ,
                        BIC = model_lca$bic,
                        Gsq = model_lca$Gsq,
                        Chisq = model_lca$Chisq,
                        npar = model_lca$npar
  ) %>% mutate(LL2 = AIC -2*npar, #Log Likelood
               CAIC = LL2 + (1 + log(N ))*npar
  ) #%>%  dplyr::select(1,2,3,4,7)
  # # consistent AIC  CAIC
  interp = c("AIC는 값지 작을 수록, BIC도 값이 작을수록 좋다. 단, AIC는 표본의 크기가 반영되지않고, BIC는 표본크기가 반영된다. 따라서, 표보크기를 반영하여 계산한 AIC인 Consistence AIC(CAIC)의 값을 보게되고, 이또한 작은 값일 수록 더 좋은 모델이다. ")
  
  table_n = ncol(model_lca$y)
  # item_resp_prob
  # 문항항반응확률 집계 
  
  summary_t_1 = do.call(rbind, model_lca$probs) %>% 
    as_tibble() %>% 
    mutate(item = rep(names(model_lca$probs), 
                      each =  nrow(model_lca$probs[[1]])),
           Class = rep( paste0("Class",1:nrow(model_lca$probs[[1]])), 
                        ncol(model_lca$y) )) %>%
    `colnames<-`(c( paste0("r",1:ncol(model_lca$probs[[1]]) ),
                    "item","Class"))
  
  
  summary_t_2 <- summary_t_1 %>%
    mutate(class_name = rep(class_name, ncol(model_lca$y)),
           total_p =  rep( model_lca$P, ncol(model_lca$y)),
           Class_p = str_c(Class,": ",
                           class_name," (",round(total_p,4)*100,"%)")  )
  
  summary_t_3 = do.call(rbind, model_lca$probs) %>% 
    as_tibble() %>% 
    mutate(item = rep(names(model_lca$probs), 
                      each =  nrow(model_lca$probs[[1]])),
           Class = rep( paste0("Class",1:nrow(model_lca$probs[[1]])), 
                        ncol(model_lca$y) ))
  
  
  summary_t_4 <- summary_t_3 %>%
    mutate(class_name = rep(class_name, ncol(model_lca$y)),
           total_p =  rep( model_lca$P, ncol(model_lca$y)),
           Class_p = str_c(Class,": ",
                           class_name," (",round(total_p,4)*100,"%)")  )
  
  r_name = unique(model_lca$y[,1]) %>% as.character()
  
  # class type
  # wide data  summary
  wide_summary = summary_t_1 %>%
    # spread(key= Class, value = YES)
    pivot_wider(names_from = Class, 
                values_from  = -c("item","Class")) 
  # wide_summary <- wide_summary0 %>% 
  # dplyr::select(1: )
  if(is.null(model_lca$coeff)){
    logit_reg=NULL
  }else{
    if(class_number==3){
      logit_reg = rbind(
        cbind(model_lca$coeff[1] ,model_lca$coeff.se[1]) %>%  
          `colnames<-`(c("Coefficient","Std.error")) %>% 
          `rownames<-`(c("2/1 (intercept)")),
        cbind(model_lca$coeff[2] ,model_lca$coeff.se[2]) %>%  
          `colnames<-`(c("Coefficient","Std.error")) %>% 
          `rownames<-`(c("2/1 corvariance")),
        cbind(model_lca$coeff[3] ,model_lca$coeff.se[3]) %>%  
          `colnames<-`(c("Coefficient","Std.error")) %>% 
          `rownames<-`(c("3/1 (intercept)")),
        cbind(model_lca$coeff[4] ,model_lca$coeff.se[4]) %>%  
          `colnames<-`(c("Coefficient","Std.error")) %>% 
          `rownames<-`(c("3/1 corvariance"))
      ) %>%data.frame() %>%  rownames_to_column() %>% 
        mutate(t = Coefficient/Std.error,
               p.value = 1-pt(abs(t), model_lca$resid.df)) %>% 
        p_mark_sig()
    }
  }
  
  
  #시각화
  bar= apply(model_lca$posterior, 1, which.max) %>%
    table() %>%
    tibble() %>%
    `colnames<-`("freq") %>%
    mutate(freq = as.factor(freq),
           Class = str_c("Class", 1:ncol(model_lca$posterior) )) %>%
    ggplot()+
    geom_bar(aes(x=Class, y=freq), stat = "identity")+
    geom_text(aes(x=Class, y=freq, label = freq),
              vjust=-.5, size= 5)
  
  
  #막대그래프 
  g = summary_t_2 %>% 
    ggplot()+
    geom_bar(aes(x = item, y = r1, fill= item),
             stat = 'identity',
             show.legend = FALSE)+
    labs(x= "측정문항", y = "YES응답확률")+
    coord_flip()+
    theme_bw()+theme(legend.position = "top")+
    theme(axis.text.y = element_text(size= size_y_text))+
    facet_wrap(~ Class_p)
  
  
  #선그래프 
  
  g2 = summary_t_2 %>% ggplot()+
    # geom_bar(aes(x= item, y= YES), stat = 'identity')+
    geom_line(aes(x = item, y = r1), group = "Class",
              linewidth=1)+
    geom_point(aes(x= item, y= r1, color= item), 
               size=3, alpha= 0.8)+
    labs(x= "측정문항", y = "YES응답확률",
         caption = r_name[1])+
    theme_bw()+#ylim(0,1)+
    theme(legend.position = "top",
          axis.text.x = element_text(angle= text_angle,
                                     size = size_y_text)
    )+facet_wrap(~ Class_p)
  
  g3 = summary_t_2 %>% ggplot()+
    # geom_bar(aes(x= item, y= YES), stat = 'identity')+
    geom_line(aes(x = item, y = r2), group = "Class",
              linewidth=1)+
    geom_point(aes(x= item, y= r2, color= item), 
               size=3, alpha= 0.8)+
    labs(x= "측정문항", y = "YES응답확률",
         caption = r_name[2])+
    theme_bw()+#ylim(0,1)+
    theme(legend.position = "top",
          axis.text.x = element_text(angle= text_angle,
                                     size = size_y_text)
    )+facet_wrap(~ Class_p)
  
  if(is.null(summary_t_2$r3)){
    g4 = "변수없음"
    g4
  }else{
    g4 = summary_t_2 %>% ggplot()+
      # geom_bar(aes(x= item, y= YES), stat = 'identity')+
      geom_line(aes(x = item, y = r3), group = "Class",
                linewidth=1)+
      geom_point(aes(x= item, y= r3, color= item),
                 size=3, alpha= 0.8)+
      labs(x= "측정문항", y = "YES응답확률",
           caption = r_name[3])+
      theme_bw()+ylim(0,1)+
      theme(legend.position = "top",
            axis.text.x = element_text(angle= text_angle,
                                       size = size_y_text)
      )+facet_wrap(~ Class_p)
  }
  
  if(is.null(summary_t_2$r4)){
    g5 = "변수없음"
    g5
  }else{
    g5 = summary_t_2 %>% ggplot()+
      # geom_bar(aes(x= item, y= YES), stat = 'identity')+
      geom_line(aes(x = item, y = r4), group = "Class",
                linewidth=1)+
      geom_point(aes(x= item, y= r4, color= item),
                 size=3, alpha= 0.8)+
      labs(x= "측정문항", y = "YES응답확률",
           caption = r_name[4])+
      theme_bw()+ylim(0,1)+
      theme(legend.position = "top",
            axis.text.x = element_text(angle= text_angle,
                                       size = size_y_text)
      )+facet_wrap(~ Class_p)
  }
  
  
  # library(gridExtra)
  
  res1=list(
    Class_summary = wide_summary,
    source_rname =r_name,
    source = summary_t_1 ,
    # source2= summary_t_2,  # 출력안해야 함, 검토용
    ModelFit = model_fit,
    llik = log_likelihood,
    # explain = interp,
    # `Estimated class population shares`= p,
    # summary_t_2,
    # viz1_freq_bar = bar,
    pattren_data = pattern_data,
    pattern_graph = g_patten,
    # var_pattern=item_resp_prob,
    class_n =class_number,
    class_population= p,
    logit_reg = logit_reg,
    viz2_Class_group = g
  )
  res2=list(
    Class_summary = wide_summary,
    source_rname =r_name,
    source = summary_t_1 ,
    
    # source2= summary_t_2,  # 출력안해야 함, 검토용
    ModelFit = model_fit,
    llik = log_likelihood,
    # explain = interp,
    # `Estimated class population shares`= p,
    # summary_t_2,
    # viz1_freq_bar = bar,
    Number_of_patterns = N,
    pattren_data = pattern_data,
    pattern_graph = g_patten,
    # var_pattern=item_resp_prob,
    class_n =class_number,
    class_population= p,
    logit_reg = logit_reg,
    viz4_Class_group = g5,
    viz3_Class_group = g4,
    viz2_Class_group = g3,
    viz1_Class_main = g2
  )
  
  switch(type,
         res1 = res1,
         res2 = res2,
         class = wide_summary,
         model_fit = model_fit, 
         df1 = summary_t_1,
         df2 = summary_t_2,
         df3 = summary_t_3,
         df4 = summary_t_4,
         bar = bar,
         llik = log_likelihood,
         logit_reg = logit_reg,
         pattern =  model_lca$predcell,
         pattern_data = pattern_data,
         pattern_g = g_patten,
         g = g,
         g1 = g2,
         g2 = g3,
         g3 = g4,
         g4 = g5
  )  
  # res
  
}
# 
# 
# election %>% head()
# fm1 <- cbind(MORALG,CARESG,KNOWG,MORALB,CARESB,
#              KNOWB,LEADB,DISHONB,INTELB)~ PARTY
# 
# poLCA(formula=fm1,data=elec,nclass=3,nrep=10,graphs = T)%>% vizLCA()
# 
# poLCA(cbind(DEFECT, HLTH,RAPE,POOR,SINGLE, NOMORE)~ SEX, 
#       data = gss08na, nclass = 3, graphs = TRUE ) %>% vizLCA()
# 
# 
# # save(gss08na, file = "gss08na.RData")
# poLCA1
# poLCA1 %>% vizLCA()
# poLCA1 %>% vizLCA(class_name = c("A","B","C"))
# poLCA1 %>% vizLCA("df2") %>% print(n=Inf)
# poLCA1 %>% vizLCA("g")
# poLCA1 %>% vizLCA("pattern") %>% 
#   arrange( desc(observed)) %>% top_n(10)
# poLCA1 %>% plot()
# 
# 
# poLCA1 %>% vizLCA("g1")
# poLCA1 %>% vizLCA("g2")
# poLCA1 %>% vizLCA("g3")
# poLCA1 %>% vizLCA("g4")
# poLCA1 %>% vizLCA("class")
# poLCA1 %>% vizLCA("pattern")
# poLCA1 %>% vizLCA("pattern_data") %>% top_n(10)
# # poLCA1 %>% vizLCA("class") %>%
# #   dplyr::select(1,2,3,4,5,6,7,8,9)
# 
# model_n3
# model_n3 %>% vizLCA()
# model_n3 %>% vizLCA(class_name = c("낙태반대","조건부찬성","낙태찬성"))
# model_n3 %>% vizLCA("res1",size_y_text = 12)
# model_n3 %>% vizLCA("class")
# model_n3 %>% vizLCA("df1")
# model_n3 %>% vizLCA("df2")
# model_n3 %>% vizLCA("df3")
# model_n3 %>% vizLCA("df3")
# model_n3 %>% vizLCA("g")
# model_n3 %>% vizLCA("g1")
# model_n3 %>% vizLCA("g2")
# model_n3 %>% vizLCA("g3") #error nothing
# model_n3 %>% vizLCA("g4") #error  nothing 
# model_n3 %>% vizLCA("pattern_data") 






#PLS-SEM####
#Internal Reliability

# plspm 경로를 만들어 주는 함수  ---------
path <- function(from, to) {
  return(list(from = from,
              to = to))
}

## plspm행렬함수 -----------------------
# Function to create path
create_path <- function(row_names = c("r1","r2","r3"), 
                        col_names = NULL, 
                        relationship = NULL) {
  
  if(is.null(col_names)){
  col_names <- row_names
  num_rows <- length(row_names)
  num_cols <- length(col_names)
  
  }else{
  num_rows <- length(row_names)
  num_cols <- length(col_names)
  }
  
  path_matrix <- matrix(0, 
                        nrow = num_rows, 
                        ncol = num_cols, 
                        dimnames = list(row_names, col_names))
  
  if (!is.null(relationship)) {
    for (path in relationship) {
      from <- path$from
      to <- path$to
      path_matrix[from, to] <- 1
    }
  }
  
  return(path_matrix)
}

# 
# edu_path <- create_path(
#   row_names = c("Support", "Advising", "Tutoring", "Value", "Satisfaction", "Loyalty"),
#   col_names = c("Support", "Advising", "Tutoring", "Value", "Satisfaction", "Loyalty"),
#   relationship = list(
#     list(from = "Value", 
#          to = c("Support", "Advising", "Tutoring")),
#     path(from = "Satisfaction",
#          to = c("Support", "Advising", "Tutoring","Value")),
#     path(from = "Loyalty",
#          to =c("Satisfaction"))   
#   )
# )
# edu_path
# 

# Function to create blocks------------
item <- function(...) {
  return(c(...))
}

# 최종 결정
blocks <- function(..., dataset = NULL, output = "match") {
  block_list <- list(...)
  
  # Combine the blocks into a named list
  names_list <- names(block_list)
  if (length(names_list) == 0) {
    stop("Please provide names for the blocks.")
  }
  
  combined_blocks <- list()
  last_value <- 0  # Variable to keep track of the last value in a block
  
  for (i in seq_along(names_list)) {
    current_block <- block_list[[i]]
    
    # Check if all items are character
    is_char <- all(sapply(current_block, is.character))
    
    # Check if all items are numeric
    is_numeric <- all(sapply(current_block, is.numeric))
    
    # If items are character and output is "match", keep them as is
    if (is_char && output == "match") {
      combined_blocks[[names_list[i]]] <- current_block
    } else if (is_numeric && output == "match") {  # If items are numeric and output is "match", convert to numeric
      combined_blocks[[names_list[i]]] <- as.numeric(current_block)
    } else if (output == "match_colnames") {  # If output is "match_colnames", match items with column names
      # Check if current_block is numeric and retrieve column names from dataset
      if (is.numeric(current_block)) {
        current_block <- colnames(dataset)[current_block]
      }
      # Keep only the column names that exist in the dataset
      current_block <- current_block[current_block %in% colnames(dataset)]
      combined_blocks[[names_list[i]]] <- current_block
    } else {  # If items are mixed or output is not "match_colnames", convert to numeric
      current_block <- as.numeric(current_block)
      if (length(current_block) > 0) {
        combined_blocks[[names_list[i]]] <- (last_value + 1):(last_value + length(current_block))
        last_value <- max(combined_blocks[[names_list[i]]])
      } else {
        # Handle the case when the block is empty
        combined_blocks[[names_list[i]]] <- integer(0)
      }
    }
  }
  
  return(combined_blocks)
}


# Example usage
# edu_blocks_named3 <- blocks(
#   Support = item(1:4),
#   Advising = item(5:8),
#   Tutoring = item("tut.prof", "tut.sched", "tut.stud", "tut.qual"),
#   Value = item("val.devel", "val.deci", "val.meet", "val.info"),
#   Satisfaction = item(17:19),
#   Loyalty = item(20:23),
#   dataset = education  # Provide your dataset here
# )
# # Print the result
# edu_blocks_named3
# 
# # Example usage
# edu_blocks_named1 <- blocks(
#   Support = item("sup.help", "sup.under", "sup.safe", "sup.conc"),
#   Advising = item("adv.comp", "adv.acces", "adv.comm", "adv.qual"),
#   Tutoring = item("tut.prof", "tut.sched", "tut.stud", "tut.qual"),
#   Value = item("val.devel", "val.deci", "val.meet", "val.info"),
#   Satisfaction = item("sat.glad", "sat.expe", "sat.over"),
#   Loyalty = item("loy.proud", "loy.recom", "loy.asha", "loy.back"),
#   dataset = education , # Provide your dataset here
#   output = "아무거나입력해도 문자는 문자로 출력 "
# )
# # Print the result
# edu_blocks_named1
# 
# 
# # ?do.call
# 
# 
# edu_blocks_named2 <- blocks(
#   Support = item(1:4),
#   Advising = item(5:8),
#   Tutoring= item( 9:12),
#   Value = item(13:16),
#   Satisfaction = item(17:19),
#   Loyalty =item(20:23),
#   output = "아무것나 입력해도 숫자는 숫자로 출력"
# )
# 
# edu_blocks_named2

create_path_plot <- function(matrix, layout = 3, 
                             node_size = 30, node_color = "white", text_size = 1.3, text_color = "gray20", edge_width = 3, edge_color = "gray60", border_width = 2, border_color = "black") {
  # 행렬 크기 확인
  library(igraph)
  n <- nrow(matrix)
  
  # 노드 이름 추출
  nodes <- rownames(matrix)
  
  # 그래프 객체 생성
  graph <- graph_from_adjacency_matrix(t(matrix), mode = "directed")  # 수정된 부분
  
  # 노드 이름 설정
  V(graph)$name <- nodes
  
  # 레이아웃 설정
  if (layout == 1) {
    layout <- layout_nicely(graph)
  } else if (layout == 2) {
    layout <- layout_in_circle(graph)
  } else if (layout == 3) {
    layout <- layout_as_tree(graph)
  } else if (layout == 4) {
    layout <- layout_with_fr(graph)
  } else if (layout == 5) {
    layout <- layout_with_kk(graph)
  } else if (layout == 6) {
    layout <- layout_with_lgl(graph)
  }
  
  # 그래프 시각화
  plot(graph, layout = layout, vertex.size = node_size, vertex.color = node_color, vertex.label.cex = text_size, vertex.label.color = text_color, edge.width = edge_width, edge.color = edge_color, vertex.frame.color = border_color, vertex.frame.width = border_width)
}

# 함수 호출
# 레이아웃 옵션, 노드 색상, 텍스트 크기, 텍스트 색상, 엣지 두께, 엣지 색상, 테두리 두께, 테두리 색상 지정
# create_path_plot(matrix_data, layout = 3)

# edu_path <- create_path(
#   row_names = c("Support", "Advising", "Tutoring", "Value", "Satisfaction", "Loyalty"),
#   col_names = c("Support", "Advising", "Tutoring", "Value", "Satisfaction", "Loyalty"),
#   relationship = list(
#     list(from = "Value", 
#          to = c("Support", "Advising", "Tutoring")),
#     path(from = "Satisfaction",
#          to = c("Support", "Advising", "Tutoring","Value")),
#     path(from = "Loyalty",
#          to =c("Satisfaction"))   
#   )
# )
# edu_path %>% create_path_plot( layout_option = 3)

# create_path(
#   row_names = c("Support", "Advising", "Tutoring", 
#                 "Value", "Satisfaction", "Loyalty"),
#   # col_names = c("Support", "Advising", "Tutoring", 
#   #               "Value", "Satisfaction", "Loyalty"),
#   relationship = list(
#     list(from = "Value",
#          to = c("Support", "Advising", "Tutoring")),
#     path(from = "Satisfaction",
#          to = c("Support", "Advising", "Tutoring","Value")),
#     path(from = "Loyalty",
#          to =c("Satisfaction"))
#   )
# )%>% create_path_plot( layout = 2, text_size = 1, node_size = 60)

# https://rich-iannone.github.io/DiagrammeR/reference/index.html

# item 그리기 
draw_blocks <- function(data, latent_size= 20,
                        layout = 'nicely',
                        edge_width = 1, edge.alpha=0.7,
                        edge_colour = "darkgreen",
                        text.colour ="gray10",
                        nudge_y = 0.1,
                        nudge_x = -0.1,
                        title=" [Figure 1] Latent and items ") {
  library(igraph)
  library(ggraph)
  # Create a graph
  graph <- graph_from_data_frame(
    
    d = do.call(rbind, lapply(names(data), function(latent_var) {
      data.frame(from = latent_var, to = data[[latent_var]])
    })),
    directed = TRUE
  )
  
  # Set node types and labels
  V(graph)$type <- ifelse(V(graph)$name %in% unlist(data),
                          "item", "latent")
  V(graph)$label <- V(graph)$name
  
  
  # Set the layout
  if (layout == "kk") {
    layout_function <- layout_with_kk
  } else if (layout == "fr") {
    layout_function <- layout_with_fr
  } else if (layout == "drl") {
    layout_function <- layout_with_drl
  } else if (layout == "mds") {
    layout_function <- layout_with_mds
  } else if (layout == "nicely") {
    layout_function <- layout_with_mds
  } else {
    stop("Invalid layout. Please choose from 'kk', 'fr', 'drl', or 'mds','nicely'.")
  }
  
  
  # Plot the graph using ggraph
  gg=ggraph(graph, layout = layout) +
    # geom_node_point(aes(color = type), size = 1) +
    geom_node_point(aes(color = type),
                    size = ifelse(V(graph)$type == "latent", 
                                  latent_size, 5),
                    # shape = 21, fill="gray90",
                    show.legend = T) +
    geom_edge_link(
      aes(start_cap = label_rect(node1.type == 'latent'),
          end_cap = label_rect(node2.type == 'latent')),
      edge_width = edge_width , 
      alpha = edge.alpha,
       arrow = grid::arrow(length = unit(3, 'mm'), ends='last'),
      edge_colour = edge_colour
    ) +
    geom_node_text(aes(label = label), repel = TRUE,
                   colour = text.colour,
                   fontface='bold',
                   nudge_y = nudge_y,
                   nudge_x = nudge_x) +
    theme_void() +
    theme(legend.position='bottom')+
    ggtitle(title)
  
  res=list(graph, V(graph)$type,V(graph)$label,gg )
  # res=gg
  res
}
# draw_blocks(edu_blocks_named1)

# 예시: drawGraph 함수 사용
# draw_blocks(edu_blocks_named, latent_size = 20)
# draw_blocks(edu_blocks_named)#, latent_size = 20,layout="fr")




#plspm reliability---------
PLS.inter.reliability <- function(x_pls, 
                                  type="res",
                                  format = "markdown"){
  #internal reliability
  library(dplyr)
  library(knitr)
  library(ggplot2)

  pls.reliability <- x_pls$unidim
  colnames(pls.reliability)=c("Mode","MVs","Cronbach.alpha","C.R(=DG.rho)","Eig.1","Eig.2")
  
  a1 <-  pls.reliability
  
  a1_1 <- a1%>%  kable(format = format, digits = 3,
                       caption="Cronbach's alpha(>0.7) & Composite reliability(CR)=DG's rho(>0.7)")

  # check outer model
  a2 <- x_pls$outer_model %>% 
    mutate("> 0.7"= ifelse(loading > 0.7,"Y","Problem"))
  
  a2_1 <- a2%>% kable(format = format, digits = 3,
          caption = "지표 신뢰도(loading > 0.7, communality > 0.5)")



  # barchart of loadings
  g <-ggplot(data = x_pls$outer_model,
             aes(x = name, y = loading, fill = block)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    # threshold line (to peek acceptable loadings above 0.7)
    geom_hline(yintercept = 0.7, color = 'gray50') +
    # add title
    ggtitle("Barchart of Loadings") +
    # rotate x-axis names
    theme(axis.text.x = element_text(angle = 90))

  res2 <-list(Internal_consistency = a1_1,
            item_validigy = a2_1,
            loadins_graph = g)

  res=list(a1,a2,g)

  switch(type,
          res= res, 
          res2= res2, 
          CR= a1,
          CRrep= a1_1,
          loadings =a2,
          graph=g )
  
  
}
# edu_pls1 %>% PLS.inter.reliability("res")
# edu_pls1 %>% PLS.inter.reliability("res2")
# edu_pls1 %>% PLS.inter.reliability("CR")
# edu_pls1 %>% PLS.inter.reliability("loadings")
# edu_pls1 %>% PLS.inter.reliability("graph")


#convegent validity
PLS.Convergent.Validity <- function(x_boot){
  library(knitr)
  library(dplyr)
  #t value t>1.96 --> ok
  #loding signification
  x_boot$boot$loadings$t_value <- with(x_boot$boot$loadings, Original/Std.Error)
  bootloading<- x_boot$boot$loadings
  manifest_Vables <-rownames(bootloading)
  bootloading<- bootloading %>%
    filter(t_value > 0) %>%
    mutate(stars=ifelse(t_value > 3.29,"***",
                        ifelse(t_value > 2.56,"**",
                               ifelse(t_value> 1.96,"*",""))))
  bootloading<- cbind(manifest_Vables,bootloading)
  a1 <-bootloading %>% kable(format = "pandoc", digits = 3,
                             caption = "Convergent Validity-01:Indicator Loading with Bootsraping(>0.7), t > 1.96")


  #Structure.V
  # x_boot$inner_summary
  #AVE
  Structure.V <-rownames(x_boot$inner_summary)
  plsAVE <-x_boot$inner_summary%>% mutate(Accept=ifelse(AVE>0.5,"Accept","No"))

  rel1 <- cbind(Structure.V ,plsAVE)
  a2 <-rel1 %>%kable(format = "pandoc", digits = 3,
                     caption = "Convergent Validity-02: (AVE>0.5) ")


  all <- list(a1,a2)
  all
}


#discriminant Validity-cross loadings/Gefen-Straub(2005)
PLS.discriminant.Validity<- function(x_pls){
  #판별 타당성
  # check cross loadings#
  a1 <-  x_pls$crossloadings %>%
    kable(format = "pandoc", digits = 3, caption = "Cross loadings")

  # load ggplot2 and reshape
  library(ggplot2)
  library(reshape)

  # reshape crossloadings data.frame for ggplot
  xloads = melt(x_pls$crossloadings, id.vars = c("name", "block"),
                variable_name = "LV")

  # bar-charts of crossloadings by block
  g <-ggplot(data = xloads,
             aes(x = name, y = value, fill = block)) +
    # add horizontal reference lines
    geom_hline(yintercept = 0, color = "gray75") +
    geom_hline(yintercept = 0.5, color = "gray70", linetype = 2) +
    # indicate the use of car-charts
    geom_bar(stat = 'identity', position = 'dodge') +
    # panel display (i.e. faceting)
    # facet_wrap(block ~ LV) +
    facet_grid(block ~ LV) +
    # tweaking some grahical elements
    theme(axis.text.x = element_text(angle = 90),
          line = element_blank(),
          plot.title = element_text(size = 12)) +
    # add title
    ggtitle("Crossloadings")

  #판별타당도 2nd#
  #x_pls$scores

  # Gefen-Straub(2005): AVE의 제곱근이 그 잠재변수와 다른  잠재변수 사이의 상관계수보다 높아야 함#
  la.score = x_pls$scores
  la.cor = cor(la.score, use = "complete.obs", method = "pearson");la.cor
  sqr.AVE = with(x_pls$inner_summary, sqrt(AVE));sqr.AVE

  ##lower triangle matrix______________
  la.cor_lower <- la.cor  # 변환을 위해 기존것을 남기고 하나 더 복사
  upper.tri(la.cor_lower, diag=FALSE) # 0으로 만들 것들을 판단
  la.cor_lower[upper.tri(la.cor_lower, diag=FALSE)] <- c(0) # True를 모두 0으로 넣기
  round(la.cor_lower,3)
  #_____________________________________
  cbind(la.cor_lower, sqr.AVE)
  AVE2<- cbind(la.cor_lower, sqr.AVE)
  AVE3 <- round(AVE2,3)    # sqr.AVE  >  pearson coeff compara   ===> OK
  #round(AVE3,3)
  a2<-AVE3 %>% kable(format = "pandoc", digits = 3,
                     caption = "Discriminant Validity:
                     Gefen-Strab(2005)- correlation coeff(rho) < sqrt(AVE)")

  rel <- list(a1,g,a2)
  rel

}




#경로도표 diagram


PLS.str_plot <- function(x){
  op1=par(mar=c(1,1,1,1), oma=c(0,0,0,0)+0.1)
  # plotting results (inner model)
  # plot(x)
  # matrix of path coefficients
  #x$path_coefs

  # plotting results (inner model):위치 조절
  #plot(x, arr.pos = 0.35)

  # matrix of path coefficients
  Paths = x$path_coefs
  # matrix with values based on path coeffs
  arrow_lwd = 10 * round(Paths, 2)
  # how does it look like?
  arrow_lwd
  par(mfrow=c(1,1))
  # arrows of different sizes reflecting the values of the path coeffs

  plot(tam_song_pls_boot, what="inner",arr.pos = 0.6, arr.lwd = arrow_lwd,
       box.size = 0.1, box.col = "gray85", lcol = "gray10", box.lwd=0.1,
       box.prop = 0.3, box.cex = 1.5, shadow.size = 0.005,
       txt.col = "gray20",cex.txt =1)
  title("구조모형의 경로계수(path coefficeint)")
}


#loadings diagram
PLS.outer_plot <- function(x){

  #par(mfrow=c(1,1))
  plot(x, what="loadings",arr.pos = 0.35, arr.lwd = arrow_lwd,
       box.size = 0.1, box.col = "gray90", lcol = "gray10", box.lwd=0.1,
       box.prop = 0.3, box.cex = 1.3, shadow.size = 0.005,
       txt.col = "gray20",cex.txt =1.3)
}

PLS.weights_plot <- function(x){

  #par(mfrow=c(1,1))
  # arrows of different sizes reflecting the values of the path coeffs

  plot(x, "weights",arr.lwd = 0.9, cex.txt = 1,
       box.size = 0.1, box.col = "Gold", box.prop = 0.5,
       box.cex = 1.5, txt.col = "black")
}




##PLS연구결과#####
PLS_effec_table<- function(x_pls){
  x_pls$effects %>%
    filter(total>0) %>%
    mutate(DE=ifelse(direct > 0,round(direct,3),""),
           IE=ifelse(indirect > 0,round(indirect,3),""),
           Effect=ifelse(indirect == 0,"Direct",
                         ifelse(direct ==0, "Indirect","Total"))) %>%
    dplyr::select(relationships,DE,IE, total,Effect) %>%
    arrange(Effect) %>%
    kable(format="pandoc", digits = 3,
          caption="직접효과,간접효과, 총효과 ")
}


#effect barplot and paper form
PLS_effectBar<- function(x_pls){
  #논문에 넣기 위해 엑셀로 편집하기 쉽도록 구성한 표
  library(dplyr)

  a1 <-x_pls$effects %>%
    # filter(total>0) %>%
    mutate(DE=ifelse(direct > 0,round(direct,3),""),
           IE=ifelse(indirect > 0,round(indirect,3),""),
           Effect=ifelse(indirect == 0,"Direct",
                         ifelse(direct ==0, "Indirect","Total"))) %>%
    dplyr::select(relationships,DE,IE, total,Effect) %>%
    arrange(Effect) %>%
    kable(format="pandoc", digits = 3,
          caption="직접효과,간접효과, 총효과 ")


  # generate plot data
  path_effs <- x_pls$effects %>% filter(total>0)
  path_effs1 <- as.matrix(path_effs[,2:3])
  # add rownames to path_effs
  rownames(path_effs1) = path_effs[,1]

  # setting margin size
  op = par(mar = c(8, 3, 1, 0.5))
  # barplots of total effects (direct + indirect)
  barplot(t(path_effs1), border = NA, col = c("#9E9AC8", "#DADAEB"),
          las = 2, cex.names = 0.8, cex.axis = 0.8,
          legend = c("Direct", "Indirect"),
          args.legend = list(x = "top", ncol = 2, border = NA,
                             bty = "n", title = "Effects"))
  # resetting default margins
  par(op)

  return(a1)

}




#PLS bootstrap total effect paper form and visualization###
PLS_boot.effect_sig<- function(edu_val){
  #bootsrap effect signification
  edu_val$boot$total.efs <- edu_val$boot$total.efs %>% filter(Original>0)

  #t= 부트스르랩 결과로 분석
  path.tvalue = with(edu_val$boot$total.efs,Mean.Boot/Std.Error)
  #path.tvalue
  cbind(edu_val$boot$total.efs, path.tvalue)
  boottotal.efs<- round(cbind(edu_val$boot$total.efs, path.tvalue),3)
  boottotal.efs
  #boottotal.efs[is.na(boottotal.efs)]<-"0"

  #t.value에 star * 표시
  boottotal.efs[is.na(boottotal.efs)]<- "0"
  boottotal.efs$star <- ""  #변수 생성
  boottotal.efs[boottotal.efs$path.tvalue > 1.96,"star"]= "*"
  boottotal.efs[boottotal.efs$path.tvalue > 2.58,"star"]= "**"
  boottotal.efs[boottotal.efs$path.tvalue > 3.29 ,"star"]= "***"
  boottotal.efs
  #t-value에 선택에 ㄷ애한 것
  #boottotal.efs
  boottotal.efs[,"Accept"]="No"
  boottotal.efs[boottotal.efs$path.tvalue >= 1.96,"Accept"]="Yes"

  a2<- boottotal.efs %>%
    kable(format = "pipe",
          caption = "Bootstrap-직접, 간접, 총효과의 유의성 검정 ")
  a1<- edu_val$effects %>%
    filter(total>0) %>%
    mutate(DE=ifelse(direct > 0,round(direct,3),""),
           IE=ifelse(indirect > 0,round(indirect,3),""),
           Effect=ifelse(indirect == 0,"Direct",
                         ifelse(direct ==0, "Indirect","Total"))) %>%
    dplyr::select(relationships,DE,IE, total,Effect) %>%
    arrange(Effect) %>%
    kable(format="pandoc", digits = 3,
          caption="직접효과,간접효과, 총효과 ")


  res <- list(Effect=a1,Significant=a2)
  res
}

{
  #path coefficeint siginification

  #path coefficeint siginification
  # PLS_boot.paths1 <- function(x_pls){
  #   #경로계수의 유의성
  #   path.tvalue = with(x_pls$boot$paths, Original/Std.Error)
  #   #path.tvalue
  #   #cbind(x_pls$boot$paths, path.tvalue)
  #   boot.paths<- round(cbind(x_pls$boot$paths, path.tvalue),3)
  #   boot.paths
  #   #boot.paths[is.na(boot.paths)]<-"0"
  #
  #   #t.value에 star * 표시
  #   boot.paths[is.na(boot.paths)]<- "0"
  #   boot.paths$star <- ""  #변수 생성
  #   boot.paths[boot.paths$path.tvalue > 1.96,"star"]= "*"
  #   boot.paths[boot.paths$path.tvalue > 2.58,"star"]= "**"
  #   boot.paths[boot.paths$path.tvalue > 3.29 ,"star"]= "***"
  #   boot.paths
  #   #t-value에 선택에 ㄷ애한 것
  #   #boot.paths
  #   boot.paths[,"Accept"]="No"
  #   boot.paths[boot.paths$path.tvalue >= 1.96,"Accept"]="Yes"
  #
  #   res<-boot.paths %>%
  #     kable(format = "pipe", caption = "Bootstrap 경로계수의 유의성 검정 ")
  #
  #   plot(x_pls, arr.pos = 0.6,
  #        box.size = 0.1, box.col = "gray85", box.lwd=0.1,
  #        box.prop = 0.3, box.cex = 1.3, shadow.size = 0.005,
  #        cex.txt = 1, lcol="black",
  #        txt.col = "black",
  #        main="Bootstrap Paths coefficient by Park Joonghee")
  #   res
  #
  #   return(res)
}
#새로 제작한 코드
PLS_boot.paths <- function(x_pls){
  res<- x_pls$boot$paths %>%
    mutate(t=Original/Std.Error,
           sig=cut(t,breaks=c(-Inf,1.96,2.58,3.29,Inf),
                   labels=c("","*","**","***"))) %>%
    kable(format = "pipe",digits = 3,
          caption = "Bootstrap 경로계수의 유의성 검정 ")
  #diagram
  plot(x_pls, arr.pos = 0.6,
       box.size = 0.1, box.col = "gray85", box.lwd=0.1,
       box.prop = 0.3, box.cex = 1.3, shadow.size = 0.005,
       cex.txt = 1, lcol="black",
       txt.col = "black",
       main="Bootstrap Paths coefficient by Park Joonghee")
}



#Bootstrap를 안한경우 경로계수와 효과(effects)
PLS.path.coef <-function(x_pls){
  #bootstrap를 안한 경우 경로계수와 effect
  #경로계수 나타내기
  library(dplyr)
  library(knitr)
  a1 <- x_pls$effects %>%
    filter(total>0) %>%
    filter(direct>0)  %>%
    dplyr::select(relationships ,path.coeff=direct) %>%
    kable(format="pandoc", digits = 3,
          caption="direct = 경로계수")

  #effects
  a2<- x_pls$effects %>% filter(total>0) %>%
    mutate(구분=ifelse(indirect == 0,".",
                     ifelse(direct ==0, "간접효과","."))) %>%
    kable(format="pandoc", digits = 3,
          caption="직접효과,간접효과, 총효과 ")

  #regression 유의성 검정
  options(scipen = 100)
  a3 <- x_pls$inner_model

  #x_pls$path_coefs
  #library(Diagram)
  #par(mar=c(1,1,1,1))
  #x_pls$path_coefs %>% round(4) %>% plotmat(box.size = 0.08, box.prop = 0.5,box.col = "Gold",box.lcol = 0,
  #                         cex.txt = 1, curve = 0.01, arr.pos = 0.45, arr.lcol = "gray70", arr.col = "gray60",
  #                                             shadow.size = 0)
  res <- list(a1,a2)

  plot(x_pls, arr.pos = 0.6,
       box.size = 0.1, box.col = "gray85", box.lwd=0.1,
       box.prop = 0.3, box.cex = 1.3, shadow.size = 0.005,
       cex.txt = 1, lcol="black",
       txt.col = "black",
       main="Regression coefficient")
  res
}




#부트스트랩 없는 효과, 경로 검정
#그룹별 비교할 대 그룹을 분리하고 사용
PLS.path.coef_sig <-function(x_pls, color="Gold"){
  #bootstrap를 안한 경우 경로계수와 effect
  #경로계수 나타내기
  library(dplyr)
  library(knitr)

  a1 <- x_pls$effects %>% filter(total>0) %>% filter(direct>0)  %>%
    dplyr::select(relationships ,path.coeff=direct) %>%
    kable(format="markdown", digits = 3, caption="direct = 경로계수")

  #effects
  # a2<- x_pls$effects %>% filter(total>0) %>%
  #   mutate(구분=ifelse(indirect == 0,".",ifelse(direct ==0, "간접효과","direct"))) %>%
  #   kable(format="markdown", digits = 3,caption="직접효과,간접효과, 총효과 ")

  a2 <-  x_pls$effects %>%
    filter(total>0) %>%
    mutate(DE=ifelse(direct > 0,round(direct,3),""),
           IE=ifelse(indirect > 0,round(indirect,3),""),
           Effect=ifelse(indirect == 0,"Direct",
                         ifelse(direct ==0, "Indirect","Total"))) %>%
    dplyr::select(relationships,DE,IE, total,Effect) %>%
    arrange(Effect) %>%
    kable(format="markdown", digits = 3,
          caption="직접효과,간접효과, 총효과 ")


  #regression 유의성 검정
  options(scipen = 1)
  # a3 <- x_pls$inner_model
  a3 <- x_pls$boot$paths %>%
    mutate(t=Original/Std.Error,
           sig=cut(t,breaks=c(-Inf,1.96,2.58,3.29,Inf),
                   labels=c("","*","**","***"))) %>%
    kable(format = "markdown",digits = 3,
          caption = "Bootstrap 경로계수의 유의성 검정 ")
  a4<- x_pls$boot$paths %>%
    mutate(t=Original/Std.Error,
           sig=cut(t,breaks=c(-Inf,1.96,2.58,3.29,Inf),
                   labels=c("","*","**","***"))) %>%
    dplyr::select(Mean.Boot,sig) %>%
    kable(format="markdown", digits = 3, caption="Bootstrap path ")
  #x_pls$path_coefs
  #library(Diagram)
  #par(mar=c(1,1,1,1))
  #x_pls$path_coefs %>% round(4) %>%
  #   plotmat(box.size = 0.08, box.prop = 0.5,box.col = "Gold",box.lcol = 0,
  # cex.txt = 1, curve = 0.01, arr.pos = 0.45,
  #   arr.lcol = "gray70", arr.col = "gray60",
  #
  #   shadow.size = 0)
  plot(x_pls,arr.pos = 0.35, box.col = color,
       box.prop=.3,cex.txt = 1,
       txt.col = "black", lcol= "black",
       main="Regression coefficient")
  res <- list(path=a1,effect=a2,sig=a3,boot_path=a4)
  res
}


#멀티그룹비교 boostratp t-test 시각화
#Bootstrap T test - Comparing Group : plspm.group()####

# gpa_edu.ttest <- plspm.groups(edu_plsv, factor(education$gender),
#                               method="bootstrap", reps = 2000)

PLS.groupcompare_ttest_0 <- function(x_ttest){
  a1 <- x_ttest
  a2<- x_ttest$test %>% kable(format="pandoc", digits = 3)
  res <- list(a1,a2)

  par(mar=c(7,5,4,5), oma=c(0,0,0,0)+0.1) #inner margin , outer margin
  barplot(t(as.matrix(x_ttest$test[,2:3])), beside = TRUE,
          col = c("orange","skyblue"), las = 2, ylim = c(-0.1, 1),
          cex.names = 0.8, col.axis = "gray30", cex.axis = 0.8)
  abline(h=0, col="gray50")
  title(" Path coefficient of Female and Male ")
  legend("top", legend=c("female","male"),col = c("orange","skyblue"),ncol=2,bty="n",pch=22,
         pt.bg = c("orange","skyblue"))
  par(mar=c(1,1,1,1), oma=c(0,0,0,0)+0.1) #원래대로
  res
}


PLS.groupcompare_ttest <- function(x_ttest,com1="Female", com2="Male",add=""){
  a1 <- x_ttest
  a2<- x_ttest$test %>%
    kable(format="markdown", digits = 3,
          caption =paste0("그룹비교 : ",com1,"과 ",com2,"의 Bootstrap t-test결과 "))
  res <- list(a1,a2,group_Plot=cat(""))



  par(mar=c(7,5,4,5), oma=c(0,0,0,0)+0.1) #inner margin , outer margin
  barplot(t(as.matrix(x_ttest$test[,2:3])), beside = TRUE,
          col = c("tomato","skyblue"), las = 2, ylim = c(-0.1, 1),
          cex.names = 0.8, col.axis = "gray30", cex.axis = 0.8)
  abline(h=0, col="gray50")
  title(paste(" Path coefficient of", com1, "and", com2,add))
  legend("top", legend=c(com1, com2),col = c("tomato","skyblue"),
         ncol=2,bty="n",pch=22,
         pt.bg = c("tomato","skyblue"))
  par(mar=c(1,1,1,1), oma=c(0,0,0,0)+0.1) #원래대로
  res
}




#그룹비교 시각화
PLS.groupcompare_barplot <- function(x,com1="Female", com2="Male",add=""){

  par(mar=c(7,5,4,5), oma=c(0,0,0,0)+0.1) #inner margin , outer margin
  barplot(t(as.matrix(x$test[,2:3])), beside = TRUE,
          col = c("tomato","skyblue"), las = 2, ylim = c(-0.1, 1),
          cex.names = 0.8, col.axis = "gray30", cex.axis = 0.8)
  abline(h=0, col="gray50")
  title(paste(" Path coefficient comparison of", com1, "and",com2,add))
  legend("top", legend=c(com1, com2),col = c("tomato","skyblue"),
         ncol=2,bty="n",pch=22,
         pt.bg = c("tomato","skyblue"))
  par(mar=c(1,1,1,1), oma=c(0,0,0,0)+0.1) #원래대로

}




#CFA function ---------
pls_cfa = function(pls_data,
                   res="all",
                   diagram=FALSE,
                   reliability_plot=FALSE,
                   digits=3,
                   boot=FALSE,
                   nboot=100,
                   rename=FALSE,
                   var_name=NULL,
                   vjust=-0.5
){
  
  library(seminr)
  library(reshape)
  library(tidyverse)
  
  #overall data 
  preview = pls_data
  # generate summary data
  pls_summary <- summary(pls_data)
  
  
  #simplt bootstrap 
  if(boot==TRUE){
    set.seed(2023)
    boot_data <- bootstrap_model(pls_data, nboot = nboot, cores = 4)
    pls_boot_summary <- summary(boot_data)
    # boot plot 
    Plot <-  plot(boot_data)
    
    # semPaths = browse_plot(boot_data)
    
    if(diagram ==TRUE){
      semPaths = browse_plot(boot_data)
    }else{
      semPaths =NULL
    }
    #bootstrap data list 
    loadings_boot0 <-   pls_boot_summary$bootstrapped_loadings[,c(1,4,5,6)] %>%               as.data.frame() %>% 
      mutate(commulality = (`Original Est.`)^2) %>% 
      dplyr::select(1,5,2,3,4)%>% 
      mutate(check= `2.5% CI`*`97.5% CI`) %>% 
      mutate(sig=ifelse(check > 0,"*","ns")) %>% 
      arrange(desc(subfactor))%>%   
      dplyr::mutate(Accept=ifelse(loadings > 0.7,"good",
                                  ifelse(loadings >0.5,"fair","")))
    
    paths_boot <- pls_boot_summary$bootstrapped_total_paths[,c(1,4,5,6)] %>%
      as.data.frame() %>% 
      mutate(check= `2.5% CI`*`97.5% CI`) %>% 
      mutate(sig=ifelse(check > 0,"*","ns")) %>% 
      dplyr::select(1,2,3,4,6)
    
    htmt_boot <-   pls_boot_summary$bootstrapped_HTMT[,c(1,4,5,6)] %>% 
      as.data.frame() %>% 
      dplyr::rename(htmt_est= `Original Est.`, t= `T Stat.`,
                    lowerCI=`2.5% CI`, upperCI=`97.5% CI`) %>% 
      mutate(sig = ifelse(htmt_est <0.9, "*",
                          ifelse(htmt_est < 1, ".","ns")))
    
  }else if(boot ==FALSE){
    pls_data <- pls_data
    Plot = plot(pls_data)   
    semPaths_boot = NULL
    plot_boot = NULL
    paths_boot = NULL
    loadings_boot0 = NULL
    htmt_boot = NULL
    
    # semPaths = browse_plot(pls_data)
    if(diagram ==TRUE){
      semPaths = browse_plot(pls_data)
      
    }else{
      semPaths =NULL
    }
  }
  
  
  
  
  #loadings 
  loadings0 = pls_data$outer_loadings %>% 
    as.data.frame() %>%
    mutate(var= rownames(pls_data$outer_loadings)) %>% # 변수맞추기
    melt() %>% 
    dplyr::rename(subfactor=var, loadings = value) %>% 
    mutate( communality = loadings^2) %>% 
    dplyr::select(variable, subfactor, loadings, communality) %>% 
    filter( loadings !=0) %>%   
    dplyr::mutate(Accept=ifelse(loadings > 0.7,"good",
                                ifelse(loadings >0.5,"fair","")))
  
  
  varname = loadings0 %>% dplyr::select(subfactor) %>% as.vector()
  
  
  
  # crossloadings = pls_summary$validity$cross_loadings
  
  crossloadings0 = pls_data$outer_loadings %>% as.data.frame() %>%
    mutate(var= rownames(pls_data$outer_loadings)) %>% # 변수맞추기
    melt() %>% 
    dplyr::rename(item=var, loadings = value, latent= variable) %>%
    filter( loadings !=0) %>% 
    dplyr::select(latent, item ) %>% 
    left_join( #데이터합치기 
      cbind.data.frame(
        item=pls_data$mmVariable,
        pls_summary$validity$cross_loadings)
    )
  
  
  
  
  
  
  if(rename==TRUE){
    loadings <- cbind.data.frame(loadings0, subFactor = var_name)  
    loadings <- loadings[,c(1,2,6,3,4,5)] 
    loadings_new <- loadings[,c(1,3,4,5,6)] 
    # dplyr::select(variable, subfactor,subFactor, loadings, communality)
    # loadings_boot0
    # loadings_boot <- cbind.data.frame(loadings_boot0, subFactor = var_name)  
    # loadings_boot <- loadings[,c(1,2,6,3,4,5)] 
    # 
    # 
    #loadinsg bar plot 
    loading_bar <- loadings %>% 
      ggplot(aes(x=subFactor, #변경된 것을 사용
                 y=loadings))+
      geom_bar(stat = "Identity", aes(fill= variable), 
               show.legend = FALSE)+
      geom_text(aes(label= round(loadings,2)), vjust= vjust)+
      ylim(0,1.1)+
      theme_bw()+
      geom_hline(yintercept = 0.7, linetype=1, 
                 color="gray30", linewidth= 0.8)+
      geom_hline(yintercept = 0.5, linetype="dashed",
                 color="gray50", linewidth=0.8)+
      theme(axis.text.x = element_text(size=13, angle=90))
    # scale_fill_grey(start=0.4, end=0.6)
    
    #cross loadings + var name 
    crossloadings <- cbind.data.frame(
      Latent = crossloadings0[,1],
      Item = var_name, 
      crossloadings0[,-1])
    
    loadings <- loadings %>% arrange(subFactor)
    loadings_new <- loadings %>% arrange(subFactor)
    
  }else if(rename==FALSE){
    loadings <- loadings0 #원래대로 
    # 
    # if(boot==TRUE){
    #   loadings_boot <- loadings_boot0  #원래대로 
    # }
    
    #loadinsg bar plot 
    loading_bar <- loadings0 %>% 
      ggplot(aes(x=subfactor, y=loadings))+
      geom_bar(stat = "Identity", aes(fill= variable),
               show.legend = FALSE)+
      geom_text(aes(label= round(loadings,2)), vjust= vjust)+
      ylim(0,1.1)+
      theme_bw()+
      geom_hline(yintercept = 0.7, linetype=1,
                 color="gray30", linewidth= 0.8)+
      geom_hline(yintercept = 0.5, linetype="dashed",
                 color="gray50", linewidth=0.8)+
      theme(axis.text.x = element_text(size=13, angle=90))
    # scale_fill_grey(start=0.4, end=0.6)
    
    
    #crossloadings petsistance 
    crossloadings <- crossloadings0
    
    loadings <- loadings %>% arrange(subfactor)
    loadings_new <- loadings %>% arrange(subfactor)
    
  }
  
  
  
  # #loadinsg bar plot 
  # loading_bar <- loadings %>% 
  #   ggplot(aes(x=subfactor, y=loadings))+
  #   geom_bar(stat = "Identity", aes(fill= variable), show.legend = FALSE)+
  #   theme_bw()+
  #   geom_hline(yintercept = 0.7, linetype=1, color="black", linewidth= 0.8)+
  #   geom_hline(yintercept = 0.5, linetype="dashed",
  #              color="gray20", linewidth=0.8)+
  #   theme(axis.text.x = element_text(size=13, angle=90))+
  #   scale_fill_grey(start=0.4, end=0.6)
  # 
  
  
  
  # This Option is  for viewing reliabiilty plots 
  if(reliability_plot==TRUE){
    reliability_plot = pls_summary$reliability %>% plot()
  }else{
    reliability_plot = NULL
  }
  
  # reliabiltiy index 
  reliability = pls_summary$reliability 
  
  # reliability acceptance criteria
  reliability_sig <- pls_summary$reliability %>% 
    as.data.frame() %>% 
    mutate(
      a_sig = ifelse(alpha> 0.7,"*","ns"),
      CR_sig = ifelse(rhoC > 0.7,"*","ns"),
      rhoA_sig = ifelse(rhoA > 0.7,"*","ns"),
      AVE_sig = ifelse(AVE > 0.5,"*","ns")
    ) %>% 
    dplyr::select(alpha, a_sig, 
                  rhoC, CR_sig, 
                  AVE, AVE_sig,
                  rhoA,rhoA_sig)
  
  #discriminant valididty first 
  fl_matrix = pls_summary$validity$fl_criteria
  # Discriminat validity second 
  htmt = pls_summary$validity$htmt
  
  
  
  
  
  
  #f2 effect size
  f2_effect = pls_summary$fSquare %>% 
    as.data.frame() %>%
    mutate(exogenous = rownames(pls_summary$fSquare )) %>% 
    melt() %>% 
    dplyr::rename(endogenous = variable) %>% 
    filter(value != 0) %>%
    mutate(EffectSize = ifelse(value > 0.35,"large(>0.35)",
                               ifelse(value > 0.15,"medium(>0.15)",
                                      ifelse(value > 0.02,"small(>0.02)","")))) %>% 
    dplyr::rename(f2=value)
  
  #paths coerficient 
  pathcoeff= pls_summary$paths
  
  
  
  
  # pls_summary
  # Generation of mediation effect data-- 직접효과
  #직접효과, 간접효과 총량, 총효과-간접효과를 해서 직접효과 계산 
  # mediation_effect = cbind.data.frame(
  #   pls_summary$total_effects,
  #   direct_total= ( 
  #     pls_summary$total_effects[,ncol(pls_summary$total_effects)] - pls_summary$total_indirect_effects[,ncol(pls_summary$total_indirect_effects)]  ),
  #   
  #   Indirect_total = pls_summary$total_indirect_effects[,ncol(pls_summary$total_indirect_effects)]
  # ) %>% 
  #   round(digits)
  
  mediation_effect=cbind.data.frame(
    effec= c(
      rep("Direct",nrow(pls_data$path_coef)),
      rep("Indirect",nrow(pls_summary$total_indirect_effects)),
      rep("Total",nrow(pls_summary$total_indirect_effects))),
    variable=c(
      rownames(pls_data$path_coef),
      rownames(pls_summary$total_effects),
      rownames(pls_summary$total_indirect_effects)),
    rbind.data.frame(
      pls_data$path_coef,
      pls_summary$total_indirect_effects ,
      pls_summary$total_effects
    )) %>% tibble()
  
  
  
  #간접효과 
  Indirect_effect= pls_summary$total_indirect_effects
  Total_effect= pls_summary$total_effects
  Direct_effect= pls_summary$total_effects-pls_summary$total_indirect_effects
  #구조모형 상관계수
  str_cor = pls_summary$descriptives$correlations$constructs
  descriptives = pls_summary$descriptives
  #설명력
  r2= pls_data$rSquared
  
  # VIF=pls_summary$vif_antecedents
  #데이터 프레임으로 전환환
  VIF= as.data.frame(do.call(cbind,
                             pls_summary$vif_antecedents))
  VIFr= as.data.frame(do.call(rbind,
                             pls_summary$vif_antecedents))
  VIFc= as.data.frame(do.call(rbind,
                             pls_summary$vif_antecedents))
  
  
  #전체 결과   
  results = list(
    Loadings = loadings, 
    loadings_boot = loadings_boot0,
    # cR_AVE = reliability,
    reliability_plot = reliability_plot,
    CRAVE_sig = reliability_sig,
    FL_matrix = fl_matrix,
    HTMT = htmt,
    htmt_boot = htmt_boot,
    crossloadings=crossloadings,
    f2_effectsize = f2_effect,
    
    loading_bar = loading_bar,
    browse_Viewer = semPaths,
    total_effect = mediation_effect,
    # Indirect_effect=Indirect_effect,
    # r2 = r2,
    # str_cor= str_cor,
    paths = pathcoeff,
    paths_boot = paths_boot,
    plot = Plot,
    varname=varname
  )
  # results
  switch(res, 
         preview=preview,
         all = results,
         loadings = loadings, 
         loadings_rename= loadings_new,
         CR_AVE = reliability,
         CR_AVE_sig = reliability_sig,
         FL_criteria = fl_matrix,
         HTMT = htmt,
         crossloadings=crossloadings,
         f2 = f2_effect,
         reliability_plot = reliability_plot,
         loading_bar= loading_bar,
         browse_Viewer= semPaths,
         
         effect = mediation_effect,
         mediation = mediation_effect,
         indirect_effect=Indirect_effect,
         Indirect_effect=Indirect_effect,
         Total_effect = Total_effect,
         total_effect = Total_effect,
         Direct_effect = Direct_effect,
         direct_effect = Direct_effect,
         
         r2 = r2,
         str_cor= str_cor,
         paths = pathcoeff,
         loadings_boot = loadings_boot0,
         paths_boot = paths_boot,
         htmt_boot = htmt_boot,
         vif=VIF,
         vifr=VIFr,
         vifc=VIFc,
         summarise=descriptives
         
  )
  
}
# View result 
# srlapp_pls %>% pls_cfa(rename = F, var_name = varName)
# srlapp_pls %>% pls_cfa(rename = T, var_name = varName)
# srl_boot3 %>% pls_boot_summary()
# varName = c( "A_upgrade","A_satisfy",
#              "S_Review","S_Add_learn","S_Feedback","S-Focus_on",
#              "SE_place","SE_time",
#              "On_joy","On_easy","On_satisfy","On_paticipate" ,
#              "IntensionUse")

#str estimate bootstrap 
pls_boot_summary <- function(pls_boot,
                             res="all",
                             title="",
                             boot=FALSE,
                             nboot= 100,
                             diagram=FALSE,
                             rename=FALSE,
                             var_name=NULL,
                             vjust=-0.5){
  
  library(tidyverse)
  library(kableExtra)
  library(seminr)
  if(boot==TRUE){
    set.seed(2023)
    boot_data <- bootstrap_model(pls_boot, nboot = nboot, cores = 4)
    pls_boot_summary <- summary(boot_data)
    
  }else if(boot==FALSE){
    boot_data <- pls_boot
    #summary 
    pls_boot_summary <- summary(boot_data)
  }
  
  # plot =   plot(boot_data, title = title)
  
  if(diagram==TRUE){
    browse_plot =  browse_plot(boot_data)
    plot =   plot(boot_data, title = title)
    
  }else{
    browse_plot =NULL
    plot =   plot(boot_data, title = title)
    # loadings <-   pls_boot_summary$bootstrapped_loadings
    #   paths <-   pls_boot_summary$bootstrapped_total_paths
    
  } 
  # data summary 
  varNames =rownames(pls_boot_summary$bootstrapped_loadings)
  
  loadings_boot0 <- pls_boot_summary$bootstrapped_loadings[,c(1,4,5,6)]%>% 
    as.data.frame() %>% 
    dplyr::rename( loadings = `Original Est.`) %>% 
    mutate( communality = loadings^2,
            variable=rownames(pls_boot_summary$bootstrapped_loadings),
            check=  `2.5% CI`*`97.5% CI`,
            sig = ifelse(check>0,"*","ns")) %>% 
    dplyr::select(variable, loadings, communality,`T Stat.`, 
                  `2.5% CI`,  `97.5% CI`, sig ) %>% tibble()
  
  
  # 
  if(rename==TRUE){
    loadings_boot <-  cbind.data.frame(var_name, loadings_boot0)
    
    loadings_boot <-  loadings_boot %>% 
      dplyr::select(variable, loadings, communality,`T Stat.`, 
                    `2.5% CI`,  `97.5% CI`, sig ) %>% tibble() %>% 
      separate(variable,c("old","latent"), " -> ") %>% 
      mutate(item = var_name) %>% 
      dplyr::select(latent,item, loadings, communality, 
                    `T Stat.`,`2.5% CI`, `97.5% CI`, sig  ) %>%   
      dplyr::mutate(Accept=ifelse(loadings > 0.7,"good",
                                  ifelse(loadings >0.5,"fair","")))
    
    
    
    #loadinsg bar plot 
    loading_bar <- loadings_boot %>% 
      ggplot(aes(x=item, #변경된 것을 사용
                 y=loadings))+
      geom_bar(stat = "Identity", aes(fill= item),
               show.legend = FALSE)+
      geom_text(aes(label= round(loadings,2)), vjust= vjust)+
      ylim(0,1.1)+
      theme_bw()+
      geom_hline(yintercept = 0.7, linetype=1, 
                 color="gray30", linewidth= 0.8)+
      geom_hline(yintercept = 0.5, linetype="dashed",
                 color="gray50", linewidth=0.8)+
      theme(axis.text.x = element_text(size=13, angle=90))
    
    
  }else if(rename==FALSE){
    # loadings_boot <- loadings_boot0%>%   
    #   dplyr::mutate(Accept=ifelse(loadings > 0.7,"good",
    #                               ifelse(loadings >0.5,"fair","")))
    
    loadings_boot <-  loadings_boot0 %>% 
      dplyr::select(variable, loadings, communality,`T Stat.`, 
                    `2.5% CI`,  `97.5% CI`, sig ) %>% tibble() %>% 
      separate(variable,c("item","latent"), " -> ") %>% 
      dplyr::select(latent,item, loadings, communality, 
                    `T Stat.`,`2.5% CI`, `97.5% CI`, sig  ) %>%   
      dplyr::mutate(Accept=ifelse(loadings > 0.7,"good",
                                  ifelse(loadings >0.5,"fair","")))
    
    
    loading_bar <- loadings_boot %>% 
      ggplot(aes(x=item, y=loadings))+
      geom_bar(stat = "Identity", aes(fill= item), 
               show.legend = FALSE)+
      geom_text(aes(label= round(loadings,2)), vjust= vjust)+
      ylim(0,1.1)+
      theme_bw()+
      geom_hline(yintercept = 0.7, linetype=1, 
                 color="gray30", linewidth= 0.8)+
      geom_hline(yintercept = 0.5, linetype="dashed",
                 color="gray50", linewidth=0.8)+
      theme(axis.text.x = element_text(size=13,
                                       angle=90))
  }
  
  loadings_boot <- loadings_boot %>% arrange(item)
  
  
  paths_boot <- pls_boot_summary$bootstrapped_total_paths[,c(1,4,5,6)] %>%
    as.data.frame() %>% 
    mutate(check= `2.5% CI`*`97.5% CI`) %>% 
    mutate(sig=ifelse(check > 0,"*","ns")) %>% 
    dplyr::select(1,2,3,4,6)
  
  
  htmt_boot <-   pls_boot_summary$bootstrapped_HTMT[,c(1,4,5,6)] %>% 
    as.data.frame() %>% 
    dplyr::rename(htmt_est= `Original Est.`, t = `T Stat.`,
                  lowerCI=`2.5% CI`, upperCI=`97.5% CI`) %>% 
    mutate(sig = ifelse(htmt_est <0.9, "*",
                        ifelse(htmt_est < 1, ".","ns")))
  
  
  result= list(
    loadings = loadings_boot, 
    paths = paths_boot,
    loading_bar=loading_bar,
    plot = plot,
    htmt_boot = htmt_boot,
    browse_plot = browse_plot,
    varNames
  )
  
  
  switch(res, 
         all=result,
         loadings = loadings_boot, 
         loadings0 = loadings_boot0,
         loading_bar=loading_bar,
         paths = paths_boot,
         plot = plot,
         htmt = htmt_boot,
         browse_plot = browse_plot,
         varName = varNames
  )
}


# mediation_effect=cbind.data.frame(
#   effec= c(
#     rep("Direct",nrow(pls_data$path_coef)),
#     rep("Indirect",nrow(pls_summary$total_indirect_effects)),
#     rep("Total",nrow(pls_summary$total_indirect_effects))),
#   variable=c(
#     rownames(pls_data$path_coef),
#     rownames(pls_summary$total_effects),
#     rownames(pls_summary$total_indirect_effects)),
#   rbind.data.frame(
#     pls_data$path_coef,
#     pls_summary$total_indirect_effects ,
#     pls_summary$total_effects
#   )) %>% tibble()



# boot_srlapp_pls <- bootstrap_model(srlapp_pls, nboot = 10000, cores = 4)
# boot_srlapp_pls
# boot_srlapp_pls %>%  pls_boot_summary()
# 
# boot_srlapp_pls %>%  pls_boot_summary(rename = T, var_name = varName_boot)
# 
# varName_boot = c( 
#              "S_Review","S_Add_learn","S_Feedback","S-Focus_on",
#              "SE_place","SE_time",
#              "On_joy","On_easy","On_satisfy","On_paticipate" ,
#              "IntensionUse",
#              "A_upgrade","A_satisfy")
# 
# 
# #loading table 
# boot_srlapp_pls %>% 
#   pls_boot_summary("loadings",rename = T, var_name = varName) %>% 
#   markdown_table()
# 
# 
# #original variable name 
# summary(boot_srlapp_pls)$bootstrapped_loadings[,c(1,4,5,6)]






# latent growth modeling - basicL variable define -  Slope, Inter

lgm <- function(x){
  library(knitr)
  #estimate
  parameterEstimates(x,standardized = T)


  #filter latent growth data
  parameterEstimates(x,standardized = T) %>% filter(op=="~~"|op=="~1") %>% filter(est>0)
  #define data
  para.data <- parameterEstimates(x,standardized = T) %>%
    filter(op=="~~"|op=="~1") %>% filter(est>0)



  #Extraction Slope
  SLOPTE<- para.data %>% filter(lhs=="Slope") %>%
    filter(op=="~1")%>% dplyr::select(est)

  #Extraction intercept
  INTERCEPT<- para.data %>% filter(lhs=="Inter")%>%
    filter(op=="~1")%>% dplyr::select(est)


  #Extraction Slope
  SLOPTE.1<- para.data %>% filter(lhs=="Slope") %>%
    filter(op=="~1")%>% dplyr::select(est, pvalue)

  #Extraction intercept
  INTERCEPT.1<- para.data %>% filter(lhs=="Inter")%>%
    filter(op=="~1")%>% dplyr::select(est, pvalue)



  #slope  intercept
  cov_T_S <- parameterEstimates(x,standardized = T) %>%
    filter(lhs=="Inter") %>% filter(op=="~~") %>% filter(rhs=="Slope") %>% dplyr::select(est,pvalue)

  #resute
  data_1<- cbind(INTERCEPT.1,SLOPTE.1,cov_T_S)
  colnames(data_1)=c("Intercepts","p.value(Inter)", "Slope", "p.value(Slope)","Cov(I<->S)","p.value(Cov)")
  data_2 <- data_1 %>% kable(digits = 3, format = "pandoc", caption="Slope & intercept & Covariance sig.")

  #extraction 0,1,2,3 time ####
  TIME <- parameterEstimates(x,standardized = T) %>% filter(lhs=="Slope") %>% filter(op=="=~") %>% dplyr::select(est)

  #1~4 ; error
  ERROR <- para.data %>% slice(1:4) %>% filter(op=="~~")%>% dplyr::select(est)
  varname <- c("1st","2nd","3rd","4st")

  #cbind data
  DATA.TOTAL <- cbind(varname , INTERCEPT,TIME ,SLOPTE,  ERROR)

  DATA.TOTAL<- as.data.frame(DATA.TOTAL)
  DATA.TOTAL<-DATA.TOTAL[,1:5]
  #names
  colnames(DATA.TOTAL)=c("Measure","Intercepts", "time","Slope","error")

  #DATA.TOTAL
  DATA.TOTAL_1 <- DATA.TOTAL %>% mutate(Mean_with_Error=Intercepts+time*Slope+error) %>% mutate(Mean_No_Error=Intercepts+time*Slope)%>%  kable(digits=3, format="pandoc", caption="latent growth data and mean result")

  #des.data <- des(x)[[3]]
  #des.data1 <-des.data[,c("vars","mean")]

  all.r <- list(data_2, DATA.TOTAL_1 )
  all.r

}






#잠재성장모델LGM mean plot#####
# https://blog.naver.com/shoutjoy/222031441467
lgm.plot <- function(x){

  #estimate
  parameterEstimates(x,standardized = T)


  #filter latent growth data
  parameterEstimates(x,standardized = T) %>%
    filter(op=="~~"|op=="~1") %>% filter(est>0)
  #define data
  para.data <- parameterEstimates(x,standardized = T) %>%
    filter(op=="~~"|op=="~1") %>% filter(est>0)

  #Extraction Slope
  SLOPTE<- para.data %>% filter(lhs=="Slope") %>%
    filter(op=="~1")%>% dplyr::select(est)

  #Extraction intercept
  INTERCEPT<- para.data %>% filter(lhs=="Inter")%>%
    filter(op=="~1")%>% dplyr::select(est)

  #slope  intercept
  cov_T_S <- parameterEstimates(x,standardized = T) %>%
    filter(lhs=="Inter") %>% filter(op=="~~") %>% filter(rhs=="Slope") %>% dplyr::select(est,std.all,pvalue)

  #resute
  data_1<- cbind(INTERCEPT,SLOPTE,cov_T_S)
  colnames(data_1)=c("Intercepts", "Slope","covariance","correlation","p.value")
  data_2 <- data_1 %>% kable(digits = 3, format = "pandoc", cpation="Slope & intercept & Covariance")

  #extraction 0,1,2,3 time ####
  TIME <- parameterEstimates(x,standardized = T) %>%
    filter(lhs=="Slope") %>% filter(op=="=~") %>% dplyr::select(est)

  #1~4 ; error
  ERROR <- para.data %>% slice(1:4) %>% filter(op=="~~")%>% dplyr::select(est)
  varname <- para.data %>% slice(1:4) %>% filter(op=="~~")%>% dplyr::select(lhs)

  #cbind data
  DATA.TOTAL <- cbind(varname , INTERCEPT,TIME ,SLOPTE,  ERROR)

  DATA.TOTAL<- as.data.frame(DATA.TOTAL)
  DATA.TOTAL<-DATA.TOTAL[,1:5]
  #names
  colnames(DATA.TOTAL)=c("Measure","Intercepts", "time","Slope","error")

  #DATA.TOTAL
  DATA.TOTAL_1 <- DATA.TOTAL %>% mutate(result_with_Error=Intercepts+time*Slope+error) %>% mutate(result_No_Error=Intercepts+time*Slope)

  #des.data <- des(x)[[3]]
  #des.data1 <-des.data[,c("vars","mean")]

  plot(result_No_Error ~ time, data=DATA.TOTAL_1,type="b", col="red", pch=16,cex=1.5,main="잠재성장모델 분석결과 예측된 평균변화 plot" )
  abline(v=0,lty=3,col=3,lwd=1)
  abline(v=1,lty=3,col=4,lwd=1)
  abline(v=2,lty=3,col=5,lwd=1)
  abline(v=3,lty=3,col=6,lwd=1)
  text(0.1,DATA.TOTAL_1[1,7],"1st")
  text(1.1,DATA.TOTAL_1[2,7],"2nd")
  text(2.1,DATA.TOTAL_1[3,7],"3rd")
  text(2.9,DATA.TOTAL_1[4,7],"4st")

}


#one variable lgm #####

lgm_1 <-function(x,Gen_name){
  library(knitr)
  # #################################

  #estimate
  #parameterEstimates(x,standardized = T)

  #filter latent growth data
  #parameterEstimates(x,standardized = T) %>% filter(op=="~~"|op=="~1") %>% filter(est>0)
  #define data
  para.data1 <- parameterEstimates(x,standardized = T) %>%
    filter(op=="~~"|op=="~1") %>% filter(est>0)
  #para.data1
  #Extraction Slope
  SLOPTE<- para.data1 %>% filter(lhs=="Slope") %>%
    filter(op=="~1")%>% dplyr::select(est)
  #SLOPTE
  #Extraction intercept
  INTERCEPT <- para.data1 %>% filter(lhs=="Inter")%>%
    filter(op=="~1")%>% dplyr::select(est)
  #INTERCEPT

  SLOPTE.1<- para.data1 %>% filter(lhs=="Slope") %>%
    filter(op=="~1")%>% dplyr::select(est,pvalue)
  #SLOPTE
  #Extraction intercept
  INTERCEPT.1 <- para.data1 %>% filter(lhs=="Inter")%>%
    filter(op=="~1")%>% dplyr::select(est,pvalue)
  #INTERCEPT



  #slope  intercept
  cov_T_S <- parameterEstimates(x,standardized = T) %>%
    filter(lhs=="Inter") %>% filter(op=="~~") %>%
    filter(rhs=="Slope") %>% dplyr::select(est)
  #cov_T_S

  #slope  intercept
  cov_T_S.1 <- parameterEstimates(x,standardized = T) %>%
    filter(lhs=="Inter") %>% filter(op=="~~") %>% filter(rhs=="Slope") %>%
    dplyr::select(est,std.all ,pvalue)
  #cov_T_S.1


  #cov_G_N<- parameterEstimates(x,standardized = T) %>% filter(lhs=="Gender") %>% filter(op=="~~") %>% filter(rhs=="NR") %>% dplyr::select(est)
  #cov_G_N

  #cov_G_N.1<- parameterEstimates(x,standardized = T) %>% filter(lhs=="Gender") %>% filter(op=="~~") %>% filter(rhs=="NR") %>% dplyr::select(est, pvalue)
  #cov_G_N.1

  #resute
  data_1 <- cbind(INTERCEPT.1,SLOPTE.1,cov_T_S.1)
  colnames(data_1)=c("Intercepts","p (Inter)", "Slope","p (Slope)", " Inter <-> Slope","cor(I-S)","p-value(I_S)")
  data_2 <- data_1 %>% kable(digits = 3, format = "pandoc", caption="Slope & intercept & Covariance")

  #data_1
  #data_2  #결과


  #extraction 0,1,2,3 time ####
  TIME <- parameterEstimates(x,standardized = T) %>%
    filter(lhs=="Slope") %>% filter(op=="=~") %>%
    dplyr::select(est)
  ##TIME


  #1~4 ; error
  ERROR <- para.data %>% slice(1:4) %>%
    filter(op=="~~")%>%
    dplyr::select(est)
  varname <- c("1st","2nd","3rd","4st")
  #ERROR
  #varname


  #parameterEstimates(x,standardized = T)

  #regression
  Inter_Gen <- parameterEstimates(x,standardized = T) %>%
    filter(lhs=="Inter") %>% filter(op=="~") %>%
    filter(rhs==Gen_name) %>% dplyr::select(est)
  Slope_Gen <- parameterEstimates(x,standardized = T) %>%
    filter(lhs=="Slope") %>% filter(op=="~") %>%
    filter(rhs==Gen_name)%>% dplyr::select(est)

  #Inter_NR <- parameterEstimates(x,standardized = T) %>% filter(lhs=="Inter") %>% filter(op=="~") %>% filter(rhs=="NR")%>% dplyr::select(est)
  #Slope_NR <- parameterEstimates(x,standardized = T) %>% filter(lhs=="Slope") %>% filter(op=="~") %>% filter(rhs=="NR")%>% dplyr::select(est)



  #Inter_Gen
  #Slope_Gen

  Gen_intercept <- parameterEstimates(x,standardized = T) %>%
    filter(lhs==Gen_name) %>% filter(op=="~1")%>%
    dplyr::select(est)
  #NR_intercept <- parameterEstimates(x,standardized = T) %>% filter(lhs=="NR") %>% filter(op=="~1")%>% dplyr::select(est)

  #Gen_intercept
  #NR_intercept

  #cbind data
  DATA.TOTAL.m <- cbind(varname, INTERCEPT ,TIME ,SLOPTE,  ERROR, Inter_Gen,Slope_Gen,Gen_intercept)

  #DATA.TOTAL.m

  #DATA.TOTAL.m<- as.data.frame(DATA.TOTAL.m)
  #DATA.TOTAL.m<-DATA.TOTAL.m[,1:5]

  #names
  colnames(DATA.TOTAL.m)=c("name", "Intercepts", "time","Slope","error","Grp_inter","Grp_slope","Grp_intercept")
  #DATA.TOTAL.m

  #plot ####
  #dt_1 <- DATA.TOTAL.m %>% mutate(Resuilt_Mean = (Intercepts + Gen_inter*Gen_intercept) + (time)*(Slope+ Gen_slope*Gen_intercept ) +  error)
  #plot(Resuilt_Mean ~ time, dt_1, type="b", pch=16,cex=1.5,  col="red", main="평균변화")
  #dt_1


  #DATA.TOTAL_1 ####
  DATA.TOTAL_1 <- DATA.TOTAL.m %>%
    mutate(Mean_with_Error = (Intercepts + Grp_inter*Grp_intercept ) + time*(Slope + Grp_slope*Grp_intercept ) +  error) %>%
    mutate(Mean_No_error = (Intercepts + Grp_inter*Grp_intercept ) + time*(Slope + Grp_slope*Grp_intercept )) %>%
    kable(digits=3, format="pandoc", caption="latent growth data of Group variable")

  #DATA.TOTAL_1

  result=list(data_2 , DATA.TOTAL_1)
  result
}














#one variable lgm #####
#https://blog.naver.com/shoutjoy/222031547222
lgm_1.plot <-function(x,Gen_name){
  library(knitr)


  #estimate
  #parameterEstimates(x,standardized = T)

  #filter latent growth data
  #parameterEstimates(x,standardized = T) %>% filter(op=="~~"|op=="~1") %>% filter(est>0)
  #define data
  para.data1 <- parameterEstimates(x,standardized = T) %>% filter(op=="~~"|op=="~1") %>% filter(est>0)
  #para.data1
  #Extraction Slope
  SLOPTE<- para.data1 %>% filter(lhs=="Slope") %>% filter(op=="~1")%>% dplyr::select(est)
  #SLOPTE
  #Extraction intercept
  INTERCEPT <- para.data1 %>% filter(lhs=="Inter")%>% filter(op=="~1")%>% dplyr::select(est)
  #INTERCEPT

  SLOPTE.1<- para.data1 %>% filter(lhs=="Slope") %>% filter(op=="~1")%>% dplyr::select(est,pvalue)
  #SLOPTE
  #Extraction intercept
  INTERCEPT.1 <- para.data1 %>% filter(lhs=="Inter")%>% filter(op=="~1")%>% dplyr::select(est,pvalue)
  #INTERCEPT



  #slope  intercept
  cov_T_S <- parameterEstimates(x,standardized = T) %>% filter(lhs=="Inter") %>% filter(op=="~~") %>% filter(rhs=="Slope") %>% dplyr::select(est)
  #cov_T_S

  #slope  intercept
  cov_T_S.1 <- parameterEstimates(x,standardized = T) %>% filter(lhs=="Inter") %>% filter(op=="~~") %>% filter(rhs=="Slope") %>% dplyr::select(est, pvalue)
  #cov_T_S.1


  #cov_G_N<- parameterEstimates(x,standardized = T) %>% filter(lhs=="Gender") %>% filter(op=="~~") %>% filter(rhs=="NR") %>% dplyr::select(est)
  #cov_G_N

  #cov_G_N.1<- parameterEstimates(x,standardized = T) %>% filter(lhs=="Gender") %>% filter(op=="~~") %>% filter(rhs=="NR") %>% dplyr::select(est, pvalue)
  #cov_G_N.1

  #resute
  data_1 <- cbind(INTERCEPT.1,SLOPTE.1,cov_T_S.1)
  colnames(data_1)=c("Intercepts","p (Inter)", "Slope","p (Slope)", " Inter <-> Slope","p-value(I_S)")
  data_2 <- data_1 %>% kable(digits = 3, format = "pandoc", caption="Slope & intercept & Covariance")

  #data_1
  #data_2  #결과




  #extraction 0,1,2,3 time ####
  TIME <- parameterEstimates(x,standardized = T) %>% filter(lhs=="Slope") %>% filter(op=="=~") %>% dplyr::select(est)
  ##TIME


  #1~4 ; error
  ERROR <- para.data %>% slice(1:4) %>% filter(op=="~~")%>% dplyr::select(est)
  varname <- c("1st","2nd","3rd","4st")
  #ERROR
  #varname


  #parameterEstimates(x,standardized = T)

  #regression
  Inter_Gen <- parameterEstimates(x,standardized = T) %>% filter(lhs=="Inter") %>% filter(op=="~") %>% filter(rhs==Gen_name) %>% dplyr::select(est)
  Slope_Gen <- parameterEstimates(x,standardized = T) %>% filter(lhs=="Slope") %>% filter(op=="~") %>% filter(rhs==Gen_name)%>% dplyr::select(est)

  #Inter_NR <- parameterEstimates(x,standardized = T) %>% filter(lhs=="Inter") %>% filter(op=="~") %>% filter(rhs=="NR")%>% dplyr::select(est)
  #Slope_NR <- parameterEstimates(x,standardized = T) %>% filter(lhs=="Slope") %>% filter(op=="~") %>% filter(rhs=="NR")%>% dplyr::select(est)



  #Inter_Gen
  #Slope_Gen

  Gen_intercept <- parameterEstimates(x,standardized = T) %>% filter(lhs==Gen_name) %>% filter(op=="~1")%>% dplyr::select(est)
  #NR_intercept <- parameterEstimates(x,standardized = T) %>% filter(lhs=="NR") %>% filter(op=="~1")%>% dplyr::select(est)

  #Gen_intercept
  #NR_intercept

  #cbind data
  DATA.TOTAL.m <- cbind(varname, INTERCEPT ,TIME ,SLOPTE,  ERROR, Inter_Gen,Slope_Gen,Gen_intercept)

  #DATA.TOTAL.m

  #DATA.TOTAL.m<- as.data.frame(DATA.TOTAL.m)
  #DATA.TOTAL.m<-DATA.TOTAL.m[,1:5]

  #names
  colnames(DATA.TOTAL.m)=c("name", "Intercepts", "time","Slope","error","Grp_inter","Grp_slope","Grp_intercept")
  #DATA.TOTAL.m

  #plot ####
  #dt_1 <- DATA.TOTAL.m %>% mutate(Resuilt_Mean = (Intercepts + Gen_inter*Gen_intercept) + (time)*(Slope+ Gen_slope*Gen_intercept ) +  error)
  #plot(Resuilt_Mean ~ time, dt_1, type="b", pch=16,cex=1.5,  col="red", main="평균변화")
  #dt_1


  #DATA.TOTAL_1 ####
  DATA.TOTAL_1 <- DATA.TOTAL.m %>% mutate(Mean_with_Error = (Intercepts + Grp_inter*Grp_intercept ) + time*(Slope + Grp_slope*Grp_intercept ) +  error) %>% mutate(Mean_No_error = (Intercepts + Grp_inter*Grp_intercept ) + time*(Slope + Grp_slope*Grp_intercept ))

  #DATA.TOTAL_1


  plot(Mean_No_error ~ time, data=DATA.TOTAL_1,type="b", col="red", pch=16,cex=1.5,main="잠재성장모델 분석결과 예측된 평균변화 plot" )
  abline(v=0,lty=3,col=3,lwd=1)
  abline(v=1,lty=3,col=4,lwd=1)
  abline(v=2,lty=3,col=5,lwd=1)
  abline(v=3,lty=3,col=6,lwd=1)
  text(0.1,DATA.TOTAL_1[1,7],"1st")
  text(1.1,DATA.TOTAL_1[2,7],"2nd")
  text(2.1,DATA.TOTAL_1[3,7],"3rd")
  text(2.9,DATA.TOTAL_1[4,7],"4st")
}












# 2variable lgm #################################
#잠재성장 모델
lgm_2 <- function(x,NR_name){
  library(knitr)
  #estimate
  #parameterEstimates(x,standardized = T)

  #filter latent growth data
  #parameterEstimates(x,standardized = T) %>% filter(op=="~~"|op=="~1") %>% filter(est>0)
  #define data
  para.data1 <- parameterEstimates(x,standardized = T) %>% filter(op=="~~"|op=="~1") %>% filter(est>0)
  #para.data1
  #Extraction Slope
  SLOPTE<- para.data1 %>% filter(lhs=="Slope") %>% filter(op=="~1")%>% dplyr::select(est)
  #SLOPTE
  #Extraction intercept
  INTERCEPT <- para.data1 %>% filter(lhs=="Inter")%>% filter(op=="~1")%>% dplyr::select(est)
  #INTERCEPT
  #slope  intercept
  cov_T_S <- parameterEstimates(x,standardized = T) %>% filter(lhs=="Inter") %>% filter(op=="~~") %>% filter(rhs=="Slope") %>% dplyr::select(est)
  #cov_T_S

  #slope  intercept
  cov_T_S.1 <- parameterEstimates(x,standardized = T) %>% filter(lhs=="Inter") %>% filter(op=="~~") %>% filter(rhs=="Slope") %>% dplyr::select(est,std.all, pvalue)
  #cov_T_S.1


  cov_G_N<- parameterEstimates(x,standardized = T) %>% filter(lhs=="Gender") %>% filter(op=="~~") %>% filter(rhs==NR_name) %>% dplyr::select(est)
  #cov_G_N

  cov_G_N.1<- parameterEstimates(x,standardized = T) %>% filter(lhs=="Gender") %>% filter(op=="~~") %>% filter(rhs==NR_name) %>% dplyr::select(est, std.all, pvalue)
  #cov_G_N.1

  #resute
  data_1 <- cbind(INTERCEPT,SLOPTE,cov_T_S.1, cov_G_N.1)
  colnames(data_1)=c("Intercepts", "Slope", " Inter <-> Slope","cor(I-S)","p-value(I_S)","Gender <-> NR","cor(G-N)","I_S.p-value(G_N)")
  data_2 <- data_1 %>% kable(digits = 3, format = "pandoc", caption="Slope & intercept & Covariance")

  #data_2  #결과
  #extraction 0,1,2,3 time ####
  TIME <- parameterEstimates(x,standardized = T) %>% filter(lhs=="Slope") %>% filter(op=="=~") %>% dplyr::select(est)
  #TIME


  #1~4 ; error
  ERROR <- para.data %>% slice(1:4) %>% filter(op=="~~")%>% dplyr::select(est)
  varname <- para.data %>% slice(1:4) %>% filter(op=="~~")%>% dplyr::select(lhs)
  #ERROR
  #varname

  #
  #parameterEstimates(x,standardized = T)

  #regression
  Inter_Gen <- parameterEstimates(x,standardized = T) %>% filter(lhs=="Inter") %>% filter(op=="~") %>% filter(rhs=="Gender") %>% dplyr::select(est)
  Slope_Gen <- parameterEstimates(x,standardized = T) %>% filter(lhs=="Slope") %>% filter(op=="~") %>% filter(rhs=="Gender")%>% dplyr::select(est)

  Inter_NR <- parameterEstimates(x,standardized = T) %>% filter(lhs=="Inter") %>% filter(op=="~") %>% filter(rhs==NR_name)%>% dplyr::select(est)
  Slope_NR <- parameterEstimates(x,standardized = T) %>% filter(lhs=="Slope") %>% filter(op=="~") %>% filter(rhs==NR_name)%>% dplyr::select(est)



  #Inter_Gen
  #Slope_Gen#
  #Inter_NR
  #Slope_NR

  Gen_intercept <- parameterEstimates(x,standardized = T) %>% filter(lhs=="Gender") %>% filter(op=="~1")%>% dplyr::select(est)
  NR_intercept <- parameterEstimates(x,standardized = T) %>% filter(lhs==NR_name) %>% filter(op=="~1")%>% dplyr::select(est)

  Gen_intercept_0 <- 0
  Gen_intercept_1 <- 1
  #Gen_intercept
  #NR_intercept

  DATA.TOTAL.grp0 <- cbind(varname, INTERCEPT ,TIME ,SLOPTE,  ERROR, Inter_Gen,Slope_Gen,Inter_NR,Slope_NR,Gen_intercept_0,NR_intercept )

  DATA.TOTAL.grp1 <- cbind(varname, INTERCEPT ,TIME ,SLOPTE,  ERROR, Inter_Gen,Slope_Gen,Inter_NR,Slope_NR,Gen_intercept_1,NR_intercept )



  NR_intercept_0 <- 0
  NR_intercept_1 <- 1


  #cbind data
  DATA.TOTAL.NR0 <- cbind(varname, INTERCEPT ,TIME ,SLOPTE,  ERROR, Inter_Gen,Slope_Gen,Inter_NR,Slope_NR,Gen_intercept,NR_intercept_0  )

  #cbind data
  DATA.TOTAL.NR1 <- cbind(varname, INTERCEPT ,TIME ,SLOPTE,  ERROR, Inter_Gen,Slope_Gen,Inter_NR,Slope_NR,Gen_intercept,NR_intercept_1  )


  #cbind data
  DATA.TOTAL.m <- cbind(varname, INTERCEPT ,TIME ,SLOPTE,  ERROR, Inter_Gen,Slope_Gen,Inter_NR,Slope_NR,Gen_intercept,NR_intercept )

  #DATA.TOTAL.m

  #DATA.TOTAL.m<- as.data.frame(DATA.TOTAL.m)
  #DATA.TOTAL.m<-DATA.TOTAL.m[,1:5]

  #names
  colnames(DATA.TOTAL.m)=c("name", "Intercepts", "time","Slope","error","Gen_inter","Gen_slope","NR_Inter","NR_Slope","Gen_intercept","NR_intercept" )

  colnames(DATA.TOTAL.grp0)=c("name", "Intercepts", "time","Slope","error","Gen_inter","Gen_slope","NR_Inter","NR_Slope","Gen_intercept","NR_intercept" )
  colnames(DATA.TOTAL.grp1)=c("name", "Intercepts", "time","Slope","error","Gen_inter","Gen_slope","NR_Inter","NR_Slope","Gen_intercept","NR_intercept" )


  colnames(DATA.TOTAL.NR0)=c("name", "Intercepts", "time","Slope","error","Gen_inter","Gen_slope","NR_Inter","NR_Slope","Gen_intercept","NR_intercept" )
  colnames(DATA.TOTAL.NR1)=c("name", "Intercepts", "time","Slope","error","Gen_inter","Gen_slope","NR_Inter","NR_Slope","Gen_intercept","NR_intercept" )


  #DATA.TOTAL.m

  #plot ####
  #dt_1 <- DATA.TOTAL.m %>% mutate(Resuilt_Mean = (Intercepts + Gen_inter*Gen_intercept + NR_Inter*NR_intercept) + time*(Slope+ Gen_slope*Gen_intercept +  NR_Slope*NR_intercept) +  error)
  #plot(Resuilt_Mean ~ time, dt_1, type="b", pch=16,cex=1.5,  col="red", main="평균변화")
  #dt_1
  DATA.TOTAL_grp0 <- DATA.TOTAL.grp0 %>% mutate(grp_0_Mean = (Intercepts + Gen_inter*Gen_intercept + NR_Inter*NR_intercept) + time*(Slope+ Gen_slope*Gen_intercept +  NR_Slope*NR_intercept) +  error) %>% kable(digits=3, format="pandoc", caption="latent growth data of Gender ==0 (female)")


  DATA.TOTAL_grp1 <- DATA.TOTAL.grp1 %>% mutate(grp_1_Mean = (Intercepts + Gen_inter*Gen_intercept + NR_Inter*NR_intercept) + time*(Slope+ Gen_slope*Gen_intercept +  NR_Slope*NR_intercept) +  error) %>% kable(digits=3, format="pandoc", caption="latent growth data of Gender == 1 (male)")




  DATA.TOTAL_NR0 <- DATA.TOTAL.NR0 %>% mutate(NR_0_Mean = (Intercepts + Gen_inter*Gen_intercept + NR_Inter*NR_intercept) + time*(Slope+ Gen_slope*Gen_intercept +  NR_Slope*NR_intercept) +  error) %>% kable(digits=3, format="pandoc", caption="latent growth data of NR ==0 (CASE-1")


  DATA.TOTAL_NR1 <- DATA.TOTAL.NR1 %>% mutate(NR_1_Mean = (Intercepts + Gen_inter*Gen_intercept + NR_Inter*NR_intercept) + time*(Slope+ Gen_slope*Gen_intercept +  NR_Slope*NR_intercept) +  error) %>% kable(digits=3, format="pandoc", caption="latent growth data of NR == 1 (CASE-2)")






  #DATA.TOTAL_1 ####
  DATA.TOTAL_1 <- DATA.TOTAL.m %>% mutate(Result_Mean = (Intercepts + Gen_inter*Gen_intercept + NR_Inter*NR_intercept) + time*(Slope+ Gen_slope*Gen_intercept +  NR_Slope*NR_intercept) +  error) %>% kable(digits=3, format="pandoc", caption="latent growth data of Gender and NR(New ratio)")

  result <- list(data_2,  DATA.TOTAL_grp0,  DATA.TOTAL_grp1,DATA.TOTAL_NR0 ,DATA.TOTAL_NR1, DATA.TOTAL_1)
  result
}

#정준상관분석####


#정준상관분석 ####
cancor.plot <- function(data,r,c){


  X=data[r]
  X1 <- scale(X, scale=F)
  #class(X)

  Y=data[c]
  Y1 <- scale(Y, scale=F)


  # cancor()
  # X,Y는 표준화한 데이터 사용 X1,Y1
  cancor.head <- cancor(X1,Y1)



  #row and column coordinates
  Rx <- X1%*%cancor.head$xcoef
  Cx <- cancor.head$xcoef
  Ry <- Y1%*%cancor.head$ycoef
  Cy <- cancor.head$ycoef
  colnames(Cx) <- colnames(X1)
  colnames(Cy) <- colnames(Y1)

  x11()
  par(mfrow=c(1,2))

  #first son
  biplot(Rx,Cx,cex=1, pch=16,
         xlim=c(-0.5,0.5), ylim = c(-0.5,0.5),
         xlab = "1st Dimension", ylab = "2nd Dimension", main = "(a) first Variable")
  abline(v=0,h=0,lty=2, col=4)

  #second son
  biplot(Ry,Cy,cex=1, pch=16,
         xlim=c(-0.5,0.5), ylim = c(-0.5,0.5),
         xlab = "1st Dimension", ylab = "2nd Dimension", main = "(b) second Variable")
  abline(v=0,h=0,lty=2, col=4)
  par(mfrow=c(1,1))



  return( cancor.head )
}
#실행방법
#cancor.plot(data[,1:2], data[,3:4])

#library(MVT)
#data(examScor)
#examScor[,1:2]
#examScor[,3:5]
#02
#cancor.plot(examScor[,1:2], examScor[,3:5])
#CCA TEST significant
cca_test <- function(data,r,c){
  library(knitr)
  # Sets of Variables : X, Y
  X1=data[,r] # Closed books
  X=scale(X1, scale=T)
  Y1=data[,c] # Opened books
  Y=scale(Y1, scale=T)
  n=nrow(X)
  p=ncol(X)
  q=ncol(Y)

  #[Step 2] Covariance Matrix S(or Correlation Matix R)
  R=round(cor(data),3)
  #R

  Rxx=R[r, r]
  Ryy=R[c, c]
  Rxy=R[r, c]
  Ryx=t(Rxy)

  # Rx ^ Ry
  Exx <- eigen(Rxx)
  Eyy <- eigen(Ryy)
  Rx <- Exx$vectors %*% diag(sqrt(Exx$values)) %*% t(Exx$vectors)
  Ry <- Eyy$vectors %*% diag(sqrt(Eyy$values)) %*% t(Eyy$vectors)

  #[Step 3] Spectral Decomposition : M=PDP
  M=solve(Rx)%*%Rxy%*%solve(Ryy)%*%Ryx%*%solve(Rx)
  #round(M, 3)
  eigen.M=eigen(M)
  eig=eigen.M$values
  round(eig, 3) # Eigenvalues
  rho=round(sqrt(eig), 3) #Canonical Correlation


  #[data analysis ]
  P=eigen.M$vectors
  U=round(solve(Rx)%*%P, 3) # Eigenvectors
  rownames(U)<-colnames(X)
  #U  #제1 정준변수쌍 axis 1

  V=solve(Ryy)%*%Ryx%*%U%*%diag(1/sqrt(eig))
  #V   #제2 정준변수쌍 axis 2

  #[Step 4] Canonical Varables Scores
  Zx=X%*%U
  #Zx
  Zy=Y%*%V
  #Zy



  #[Step 4] Testing Significant Canonical Correlations
  ev <- (1 - eigen.M$values)
  s <- min(p, q)
  #s
  w=n - 3/2 - (p + q)/2
  Lambda <- rev(cumprod(rev(ev)))
  # initialize
  d1 <- d2 <- f <- vector("numeric", s)
  for (i in 1:s) {
    t <- sqrt((p^2 * q^2 - 4)/(p^2 + q^2 - 5))
    ti <- 1/t
    d1[i] <- p * q
    d2[i] <-w * t - p * q/2 + 1
    r <- (1 - Lambda[i]^ti)/Lambda[i]^ti
    f[i] <- r * d2[i]/d1[i]
    p <- p - 1
    q <- q - 1
  }

  p_value <- pf(f, d1, d2, lower.tail = FALSE)

  cancor.s <-cancor(X,Y)
  cancor.cor <- cancor.s$cor

  dmat <- cbind(Eigen_Value=eig,Canonical_Corr = cancor.cor,WilksLambda = Lambda, F = f, df_1 = d1, df_2 = d2, p_value = p_value)
  rownames(dmat)=c("1st_axis", "2nd_axis")

  # scienct numeric

  damat.re <-kable(dmat, format = "pandoc", digits=3, caption = "CCA TEST F- significant")


  cca_result<- rbind(U,V,Eigen_Value=eig,Canonical_Correlation=rho,WilksLambda = Lambda, F = f, df_1 = d1, df_2 = d2, p_value = p_value)
  colnames(cca_result)=c("1st pair", "2nd pair")
  secondResult<- kable(cca_result, format = "pandoc", digits = 3, caption = "Canonical Variables & Correlations & sigification")





  Re<- list(damat.re, secondResult)
  Re
}
cca_Matirx_test <- function(data,r,c,sample_N){
  library(knitr)
  # Sets of Variables : X, Y
  #   X=data[,r] # Closed books
  #  X=scale(X, scale=T)
  # Y=data[,c] # Opened books
  #  Y=scale(Y, scale=T)
  n=sample_N
  p=length(r)
  q=length(c)

  #[Step 2] Covariance Matrix S(or Correlation Matix R)
  R=round(data,3)
  #R

  Rxx=R[r, r]
  Ryy=R[c, c]
  Rxy=R[r, c]
  Ryx=t(Rxy)

  # Rx ^ Ry
  Exx <- eigen(Rxx)
  Eyy <- eigen(Ryy)
  Rx <- Exx$vectors %*% diag(sqrt(Exx$values)) %*% t(Exx$vectors)
  Ry <- Eyy$vectors %*% diag(sqrt(Eyy$values)) %*% t(Eyy$vectors)

  #[Step 3] Spectral Decomposition : M=PDP
  M=solve(Rx)%*%Rxy%*%solve(Ryy)%*%Ryx%*%solve(Rx)
  #round(M, 3)
  eigen.M=eigen(M)
  eig=eigen.M$values
  round(eig, 3) # Eigenvalues
  rho=round(sqrt(eig), 3) #Canonical Correlation


  #[data analysis ]#######################################
  P=eigen.M$vectors

  U=round(solve(Rx)%*%P, 3) # Eigenvectors
  rownames(U)<-colnames(X)
  #U  #제1 정준변수쌍 axis 1

  V=solve(Ryy)%*%Ryx%*%U%*%diag(1/sqrt(eig))
  #V   #제2 정준변수쌍 axis 2

  #[Step 4] Canonical Varables Scores
  Zx=X%*%U
  #Zx
  Zy=Y%*%V
  #Zy





  #[Step 4] Testing Significant Canonical Correlations
  ev <- (1 - eigen.M$values)
  s <- min(p, q)
  #s
  w=n - 3/2 - (p + q)/2
  Lambda <- rev(cumprod(rev(ev)))
  # initialize
  d1 <- d2 <- f <- vector("numeric", s)
  for (i in 1:s) {
    t <- sqrt((p^2 * q^2 - 4)/(p^2 + q^2 - 5))
    ti <- 1/t
    d1[i] <- p * q
    d2[i] <-w * t - p * q/2 + 1
    r <- (1 - Lambda[i]^ti)/Lambda[i]^ti
    f[i] <- r * d2[i]/d1[i]
    p <- p - 1
    q <- q - 1
  }

  p_value <- pf(f, d1, d2, lower.tail = FALSE)

  cancor.s <-cancor(X,Y)
  cancor.cor <- cancor.s$cor

  dmat <- cbind(WilksLambda = Lambda, F = f, df_1 = d1, df_2 = d2, p_value = p_value)
  rownames(dmat)=c("1st_axis", "2nd_axis")

  # scienct numeric

  damat.re <-kable(dmat, format = "pandoc", caption = "CCA TEST F- significant")


  # second paper data
  cca_result<- rbind(U,V,Eigen_Value=eig,Canonical_Correlation=rho,WilksLambda = Lambda, F = f, df_1 = d1, df_2 = d2, p_value = p_value)
  colnames(cca_result)=c("1st pair", "2nd pair")
  secondResult<- kable(cca_result, format = "pandoc", digits = 3, caption = "Canonical Variables & Correlations & sigification ")


  Re<- list(damat.re, secondResult)
  Re
}


#평균중심변환 함수2020.11.17 ####
#변수 Centering 평균중심변환 함수

#변수 Centering 평균중심변환 함수
Mean_center1 <-function(dataset,variable){
  if(!is.numeric(variable)){return("연속형변수가 아닙니다. 데이터, `데이터$변수명`을 정확히 입력해주세요 ")}

  library(dplyr)
  data<-dataset %>% as.data.frame()
  data$x<-variable
  cen.data <- data %>% mutate(x_m=mean(x), x_Center = x - mean(x),SD_x=sd(x),sd_low=x_Center+sd(x),sd_high=x_Center-sd(x)) %>% round(2)
  cn<- cen.data[,c(1,2,3,7,8)]
  #aa<-list(centered=cn)#,full_data=cen.data)
  cn
}

Mean_center <-function(dataset,x){
  #  attach(dataset)
  library(dplyr)
  if(!is.numeric(x)){return("연속형변수가 아닙니다. 데이터, `데이터$변수명`을 정확히 입력해주세요 ")}

  dataset<-dataset %>% as.data.frame()
  #dataset$x <- x
  row_name<-rownames(dataset)
  cen.data <- dataset %>% mutate(x_m=mean(x), x_Center = x - mean(x),SD_x=sd(x),sd_low=x_Center+sd(x),sd_high=x_Center-sd(x)) %>% round(2)
  rownames(cen.data)=row_name
  res<-cen.data[,c(1,2,3,5,7,8)]
  res
}

#Mean_center(women,women$weight)



#simple 기술통계 aggregate####
#평균과 표준편차를 빠르게 구하기 aggregate####
dataMSD <- function(data,form){
  cbind(Mean=aggregate(form ,data, mean),SD=aggregate(form,data, sd)[,-1]) %>% round(2)
}

#dataMSD(mtcars, mpg~am)
#dataMSD(mtcars, mpg~am+vs)
# count(mtcars,am,vs)


#평균과 표준편차, 샘플수 구하기 데이터, 형식(종속변수~그룹변수, 그룹분류(데이터프레임))
dataMSDN <- function(data,form, ...){
  #counta <-table(data$x)
  library(dplyr)
  library(knitr)

  n<-count(data, ...)
  agg_mean <-aggregate(form ,data, mean)
  casemean <- agg_mean[,ncol(n)]
  agg_sd <- aggregate(form ,data, sd)
  a<-cbind(Case=agg_mean[,-ncol(n)], M=casemean,SD=agg_sd[,ncol(n)],N=n[,ncol(n)]) %>% round(2)
  #s=list(a,n);s
  a<-a %>% as.data.frame() %>% kable("pandoc",caption = "그룹별 기술통계")
  a
}
#실행
#dataMSDN(mtcars, mpg~am,am)
#dataMSDN(mtcars, mpg~am+vs,am,vs)
#dataMSDN(mtcars, mpg~am+vs+cyl,am,vs,cyl)

#form에서 나타난 만큼 독립변수를  ,로 구분하여 입력 해야 함.



#interCal:::interaction계산기 #######################################
#데이터 생성  2x2
# 2by 2 생성
grp22<-function(xx,...){
  gr <-rbind(xx,...)
  colnames(gr)=c("M","SD","N")
  rownames(gr)=c("grp1_ap","grp2_aq","grp3_bp","grp4_bq")
  gr<-gr
  gr
}

# 데이터가 많은 경우 생성
grpdata<-function(xx,...){
  gr <-rbind(xx,...)
  colnames(gr)=c("M","SD","N")
  rownames(gr)=paste0(rep("grp_",nrow(gr)),1:nrow(gr))
  gr<-gr
}

#gr<-grp22(c(24.75,7.44831,12),c(23.5833,7.85,12),c(23.3333,7.36495,12),c(12.9167,6.40253,12))
#gr %>% class
#grpdata(c(24.89,2.37,9),c(21.44,2.79,9),c(14.56,2.46,9),c(12,2.69,9),c(1,2,3),c(34,3.2,1))

# MSDN으로 interaction 계산######
#데이터 생성  2x2
# 2by 2 생성
grp22<-function(xx,...){
  gr <-rbind(xx,...)
  colnames(gr)=c("M","SD","N")
  rownames(gr)=c("grp1_ap","grp2_aq","grp3_bp","grp4_bq")
  gr<-gr
  gr
}
#interCal #####
interCal <- function(MSDN, text="",sel=1){
  library(dplyr, warn.conflicts = FALSE)
  # Suppress summarise info
  options(dplyr.summarise.inform = FALSE)

  if(!is.character(MSDN)){if(!is.data.frame(MSDN)){
    if(!is.matrix(MSDN)){return("정확한 데이터인 dataframe 이나 (2X2)matrix를 입력해주세요.grp22를 이용하여 데이터를 생성하세요")}}
  }else{gdata <- MSDN}

  #데이터 입력
  gdata <- MSDN

  #구분변수 입력 파트
  if(is.character(text)){
    gl <-strsplit(text, split= "\\,") %>% unlist()
    grp1.ap <-paste("m1 =", gl[1], gl[3])
    grp2.aq <-paste("m2 =", gl[1], gl[4])
    grp3.bp <-paste("m3 =", gl[2], gl[3])
    grp4.bq <-paste("m4 =", gl[2], gl[4])
    ggt <- rbind(grp1.ap,grp2.aq ,grp3.bp,grp4.bq)
    gdata.total <-cbind(ggt,gdata)}
  else(return("문자변수로 `,로 구분해서입력하세요. 문자변수가 없으면 sel=1 or sel=2로 입력하세요"))

  library(knitr)
  gdata.total<-gdata.total %>% kable("pandoc", caption = "입력된 기술통계 데이터 M,SD,N ")


  #자유도 및 합동분산 계산
  M<-gdata[,1]
  SD<-gdata[,2]
  N<-gdata[,3]

  df1<-N-1
  df1
  var=SD^2
  var
  spsq <-sum(df1*var)/sum((df1))
  #gdata$con<-c("m1","m2","m3","m4")
  #합동분산 pooled variance


  #contrast setting
  # ap aq bp bq
  contr1= cbind("inter:ap/aq/bp/bq:m1-m2-m3+m4=0"= c(1,-1,-1,1), #interaction
                "me_of_ab:(m1+m2)-(m3+m4)=0"    =c(1,1,-1,-1), # (m1+m2)-(m3+m4)=0
                "sme.of.ab_at.p:m1-m3=0"        =c(1,0,-1,0),  # m1-m3=0
                "sme.of.ab_at.q:m2-m4=0"        =c(0,1,0,-1),  # m2-m4=0

                "me_of_pq:(m1+m3)-(m2+m4)=0"=c(1,-1,1,-1),   #(m1+m3)-(m2-m4) =0
                "sme.of.pq_at.a:m1-m2=0"    =c(1,-1,0,0),    # m1-m2=0
                "sme.of.pq_at.b:m3-m4=0"    =c(0,0,1,-1))    # m3-m4=0
  rownames(contr1)=c("m1","m2","m3","m4")

  contr2= cbind("inter:ap/aq/bp/bq:m1-m2-m3+m4=0"= c(1,-1,-1,1),
                #"me_of_ab:(m1+m2)-(m3+m4)=0"=c(1,1,-1,-1), # (m1+m2)-(m3+m4)=0
                "sme.of.ab_at.p:m1-m3=0"     =c(1,0,-1,0),  # m1-m3=0
                "sme.of.ab_at.q:m2-m4=0"     =c(0,1,0,-1),  # m2-m4=0

                #"me_of_pq:(m1+m3)-(m2+m4)=0"=c(1,-1,1,-1),   #(m1+m3)-(m2-m4) =0
                "sme.of.pq_at.a:m1-m2=0"   =c(1,-1,0,0),    # m1-m2=0
                "sme.of.pq_at.b:m3-m4=0"   =c(0,0,1,-1))    # m3-m4=0
  rownames(contr2)=c("m1","m2","m3","m4")


  if(sel==1){contr<- contr1} #total 출력
  else{if(sel==2){contr <-contr2}   #simple main effect만 출력
    else{return("selcon = 1과 selcon = 2중에서 선택하세요")}}



  #행렬 계수*평균
  cim<- contr * M
  #cim

  #평균합
  cim_sum <-apply(cim, 2, sum)
  #cim_sum

  #등분산인 경우
  ci2sd2<- contr^2*spsq  #계수제곱* pooled variance
  ci2sd2_N<- ci2sd2/N    #N을 나눈 것
  SE <- sqrt(apply(ci2sd2_N,2,sum))
  #SE  #standard Erroe

  #등분산이 아닌 경우
  #{ci2sd2_notvar <- (contr1^2 *gdata[,2]^2)/N
  #SE.notvar<- sqrt(apply(ci2sd2_notvar,2,sum))
  #SE.notvar}


  df <- sum(N)-nrow(gdata)

  t <- cim_sum/SE
  cri.t <- abs(qt(0.05/2,df))
  p.val<- 2*(1-pt(abs(t),df))
  #p.val
  LowerCI <- cim_sum - SE * cri.t
  UpperCI <- cim_sum + SE * cri.t
  star <- ifelse(p.val<0.001,"***",ifelse(p.val<0.01,"**",ifelse(p.val<0.05,"*","NotSig")))
  options(scipen = 100)
  table <- cbind(cim_sum, SE, t, cri.t,df,LowerCI,UpperCI,p.val) %>% round(3)
  #pooled.var=spsq,
  table1<- cbind(table, star)
  tab <- table1 %>% as.data.frame


  res<-list(gdata.total,"대비검정"=tab,"대비계수"=t(contr), "합동분산"=spsq)
  res#결과

}

#사용방법

#실행하기gl

#a1 =c(24.75,   7.45 ,  12)
#a2 =c(23.58,   7.86,   12)
#a3=c( 23.33,   7.36 ,  12)
#a4=c(12.92 ,  6.40,   12)

#gr<-grp22(a1,a2,a3,a4)


#interCal(grp22(a1,a2,a3,a4))
#interCal(grp22(c(24.75,   7.45 ,  12),
#               c(23.58,   7.86,   12),
#               c( 23.33,   7.36 ,  12),
#               c(12.92 ,  6.40,   12)  ))
#interCal(grp22(a1,a2,a3,a4),"aa,bb,pp,qq",2)
#interCal(grp22(a1,a2,a3,a4),1)
#interCal(grp22(a1,a2,a3,a4),1,222)


#interCal(gr) #maine
# interCal(gr,sel=2)
# interCal(gr,"")
#interCal(gr,2) #sme
#interCal(gr,sel=1) #maine
# interCal(gr,3) #오류 메시지 출력
# interCal(c(12,3,2),c(1,2,3))  # 오류메시지 확인
# interCal(c("al dadas",1))  #오류 메지시 확인


#################################

#t test를 SPSS양식으로 ####
ttestspss <- function(formula,data){
  library(dplyr)
  library(knitr)
  library(car)
  levent<-leveneTest (formula, data ,center=mean)
  ff<-levent$`F value`[1]
  pp<-levent$`Pr(>F)`[1]


  #student ttest
  mtc_vs_ttest <-t.test(formula, data, var.equal=T)

  t_val<- mtc_vs_ttest$statistic #t value
  t_df<-mtc_vs_ttest$parameter #df
  t_pval<- mtc_vs_ttest$p.value
  t_mean<- mtc_vs_ttest$estimate #mean
  t_SE<-mtc_vs_ttest$stderr
  t_CI<- t(mtc_vs_ttest$conf.int) #CI

  #Whechl t test
  ttest <-t.test(formula, data, var.equal=F)

  t_val1<- ttest$statistic #t value
  t_df1<-ttest$parameter #df
  t_pval1<- ttest$p.value
  t_mean1<- ttest$estimate #mean
  t_SE1<-ttest$stderr
  t_CI1<- t(ttest$conf.int) #CI

  #Levene적용
  result1<-cbind(Levene_F=ff,Leven_p=pp,t=t_val,df=t_df,p=t_pval,SE=t_SE,CI=t_CI) %>% round(3)
  result2<- cbind(Levene_F=ff,Leven_p=pp,t=t_val1,df=t_df1,p=t_pval1,SE=t_SE1,CI=t_CI1) %>% round(3)


  #result1<-cbind(t=t_val,df=t_df,p=t_pval,SE=t_SE,CI=t_CI) %>% round(4)
  #result2<- cbind(t=t_val1,df=t_df1,p=t_pval1,SE=t_SE1,CI=t_CI1) %>% round(4)
  result<-rbind(result1, result2)
  #colnames(result)=c("t","df","p","Std.Error","95%CI lower","95%CI upper" )
  colnames(result)=c("Levene's F","Sig","t","df","p","Std.Error","95%CI lower","95%CI upper" )
  rownames(result)=c("Equal var. assumed","Equal var. Not assumed")
  result# %>% kable("", caption = "Independent t-test of SPSS type table By park Joonghee(2020)")
}
#독립변수는 factor로 할 것
# ttestspss(mpg~factor(am),mtcars)
# ttestspss(mpg~am,mtcars)  #error


#유의성 별 만들기 #####
star_make<-function(data,p.value){
  ndata<- data %>% as.data.frame() %>%
    mutate(stars=ifelse(p.value < 0.001, "***",
                        ifelse(p.value < 0.01, "**",
                               ifelse(p.value < 0.05, "*", ""))))
  ndata
}

star<- function(p.value){
  ifelse(p.value<0.001,"***",
         ifelse(p.value<0.01,"**",
                ifelse(p.value<0.05,"*","")))
}

#사용
#star_make(mmm,v2)

#v1=c(1,2,3,4)
#v2=c(0.1,0.04,0.003,0.00001)
#mmm<-cbind(v1,v2)
#mmm
#star_make(mmm,v2)





#카이제곱 검정::오류메시지 해결하는 함수 제작 ####
#The code for Monte Carlo simulation is a C translation of the Fortran algorithm of Patefield (1981).
#Hope, A. C. A. (1968). A simplified Monte Carlo significance test procedure. Journal of the Royal Statistical Society Series B, 30, 582–598. http://www.jstor.org/stable/2984263.
#Agresti, A. (2007). An Introduction to Categorical Data Analysis, 2nd ed. New York: John Wiley & Sons. Page 38.
advanced.chisq.test<-function(cri, option=TRUE){
  cc <- cri[rowSums(cri)>0,]
  if(option == TRUE){
    chisq.test(cc, simulate.p.value = TRUE)}
  else{chisq.test(cc)}
}

#실행
#advanced.chisq.test(online_freq)














#sobel test my function 박중희 #####
#sobel test my function 박중희 #####
sobel.test <- function(x,m,y){
  options(scipen=10)
  library(multilevel)
  sob.model<- sobel(pred=x,med=m,out=y)
  p_value<-  2*(1-pnorm(abs(sob.model$z.value)))
  library(bda)
  bda <-bda::mediation.test(m,x,y)
  z.value<-bda$Sobel[1]
  p.value<-bda$Sobel[2]
  sig <-ifelse(p_value < 0.05,paste0("간접효과(indirect effect=ab)는 통계적으로 유의미하다,"," z = ",
                                     round( z.value,2),", p = ",
                                     round(p.value,4),"."),
               "간접효과(indirect effect)는통계적으로 유의미하지 않다")
  res<-list(c(Sobel_z=sob.model$z.value,p.value=p_value,ind.effect=sob.model$Indirect.Effect),sig, bda)
  res
}

#sobel test my function 박중희 #####
BKsobel.test <- function(x,m,y){
  options(scipen=10)
  library(multilevel)
  sob.model<- sobel(pred=x,med=m,out=y)
  p_value<-  2*(1-pnorm(abs(sob.model$z.value)))
  library(bda)
  bda <-bda::mediation.test(m,x,y)
  z.value<-bda$Sobel[1]
  p.value<-bda$Sobel[2]
  #step1 total effect
  c.lm <- lm(y ~ x)
  a.lm <- lm(m ~ x)
  cp.b.lm <-lm(y~x+m)

  a<- coef(summary(a.lm))[2,1]
  SEa<- coef(summary(a.lm))[2,2]
  a_p.value <- coef(summary(a.lm))[2,4]
  b <- coef(summary(cp.b.lm))[3,1]
  SEb <-coef(summary(cp.b.lm))[3,2]
  b_p.value <- coef(summary(cp.b.lm))[3,4]
  cp<-coef(summary(cp.b.lm))[2,1]
  SEcp<-coef(summary(cp.b.lm))[2,2]
  cp_p.value<-coef(summary(cp.b.lm))[2,4]

  c<-coef(summary(c.lm))[2,1]
  SEc<-coef(summary(c.lm))[2,2]
  c_p.value<-coef(summary(c.lm))[2,4]

  Direct_effect.cp<-cp
  Total_effect.c<-c
  Indirect_effect.ab=a*b

  #BK step causal step
  coeff.c<-cbind(c,SEc,c_p.value ) #1
  coeff.a<-cbind(a,SEa,a_p.value) #2
  coeff.b<-cbind(b,SEb, b_p.value)  #3
  coeff.cp<-cbind(cp,SEcp,cp_p.value ) #3
  coeff=rbind(coeff.c,coeff.a,coeff.b,coeff.cp)
  name=c("BK:step1_X->Y(c)","BK:step2_X->M(a)" ,"BK:step3_M->Y(b)", "BK:step3_X->Y(c')")
  cname=c("B","SE","p.value","stars")

  #coeff<-data.frame(coeff)
  library(dplyr)
  coeff <- coeff %>% as.data.frame() %>%
    mutate(stars=ifelse(p.value < 0.001, "***",
                        ifelse(p.value < 0.01, "**",
                               ifelse(p.value < 0.05, "*", ""))))
  rownames(coeff)=name
  colnames(coeff)=cname

  effect<- cbind(Total_effect.c, Direct_effect.cp, Indirect_effect.ab)

  sig <-ifelse(p_value < 0.05,paste0("간접효과(indirect effect=ab)는 통계적으로 유의미하다,"," z = ",
                                     round( z.value,2),", p = ",
                                     round(p.value,4),"."),
               "간접효과(indirect effect)는통계적으로 유의미하지 않다")
  res<-list(coeff,effect,
            c(Sobel_z=sob.model$z.value,p.value=p_value,ind.effect=sob.model$Indirect.Effect),sig, bda)
  res
}

#각각의 변수를 넣어야 하는 함수X,M,Y
# sobel.test2(tm7$facultysupport,tm7$achievementmotivation, tm7$publication)
#


#mtcars

# BKsobel.test(mtcars$disp, mtcars$wt, mtcars$mpg)
# sobel.test(mtcars$disp, mtcars$wt, mtcars$mpg)

#사용법:: 각의 변수를 넣어야 하는 함수
# sobel.test(tm7$achievementmotivation,tm7$facultysupport, tm7$publication)

#직접 값을 넣는 함수
SOBEL<-function(a,sa,b,sb){
  options("scipen"=10)
  AB=a*b
  A=a^2
  SA=Sa^2
  B=b^2
  SB=Sb^2

  rootSSC= sqrt(A*SB+B*SA)
  z=AB/rootSSC  #z value
  pval <- 2*(1-pnorm(abs(z))) #p value
  sig <-ifelse(pval < 0.05,"간접효과(indirect effect = ab)는 통계적으로 유의미하다","간접효과(indirect effect)는통계적으로 유의미하지 않다")
  list(c(z.value= z,p.value= pval), sig)
}


#a= 0.6694448
#Sa=0.09998841
#b=.8506020
#Sb=0.1437364

# SOBEL(a,sa,b,sb)
#
#
#

#다차원척도법 plot####

#character datq tranfomation double
Char2num <- function(data, colnumber ="auto", iter=NULL){
  if(colnumber =="auto"){
    iter=1:ncol(data)
  }else if(colnumber =="manual"){
    iter = iter
  }

  for( i in iter){
    data[[i]] <- suppressWarnings(as.numeric(data[[i]]))
    data[[i]][is.na(data[[i]])  ] <- 0
  }

  data
}


#함수그리기 자동화

mdsplot<- function(point,
                   xlim=c(-2,2),
                   ylim=c(-2,2),
                   bg = c(1:ncol(point)),
                   text.cex=2,
                   name="auto",
                   text.name=NULL,
                   pch=21,
                   vjust=1,
                   point.cex=2,
                   type = "p",
                   col="black",
                   title="다차원척도 2차원 plot by JH Park",
                   abline.col="darkred"

){
  #par(mar=c(4,4,4,4))
  plot(point[,1],point[,2], type=type,
       xlim= xlim ,
       ylim= ylim ,
       pch=pch,
       bg = bg,
       cex=point.cex,
       main= title,
       xlab="variable1",
       ylab="variable2",
       col=col
  )
  grid()
  if(name=="auto"){
    text(point + vjust, rownames(point), cex=text.cex )
  } else if(name=="manaual"){
    text(point + vjust, labels=text.name, cex=text.cex )
  }

  abline(v=0,lty=2, col=abline.col)
  abline(h=0,lty=2, col=abline.col)
}




#다차원 척도법 실행
MDS <- function(data, char2num=FALSE, result.all=FALSE){

  #character데이터를 numeric 데이터로 변환환
  if(char2num==TRUE){
    for( i in 1:ncol(data)){
      data[[i]] <- suppressWarnings( as.numeric(data[[i]]) )
      # data[[i]] <- as.numeric(data[[i]])
      data[[i]][is.na(data[[i]])  ] <- 0
    }
  }

  # Dissimilarity matrix
  X <- data
  n <- nrow(X)
  p <- ncol(X)
  m <-  as.matrix(
    dist(X, method="euclidean", diag = T) )

  D1 <- round(m^2,3)/p   # 비유사성

  cmd <- cmdscale(D1, k=2,eig = T) #metric MDS
  cmdplot<-cmd$points
  cmd_Gof<-cmd$GOF

  if(result.all==TRUE){
    res=list(distMatrix=m, dissimilarty=D1, cmd)

  }else{
    res=cmd$points

  }
  res
}
# # 결과보기기
# Ohhaeun2023_matrix_2_data %>%  MDS(result.all = FALSE)
# Ohhaeun2023_matrix_2_data %>%  MDS()
# #모든 데이터보기
# Ohhaeun2023_matrix_2_data %>%  MDS(result.all = TRUE)
#
#
#
# #같은 결과 데이터를 numerric변환후 MDS실행 결과
# Ohhaeun2023_matrix_2[,-c(1,2)] %>%char2num()%>% MDS()
#
# Ohhaeun2023_matrix_2[,-c(1,2)]%>% MDS(char2num = TRUE) %>% cmdscale_plot()
# Ohhaeun2023_matrix_2[,-c(1,2)]%>% MDS(char2num = TRUE) %>% mdsplot()
#
#




cmdscale_plot <- function(con,
                          text.cex=1,
                          text.position=3,
                          text.col=2,
                          range="auto",
                          xlim=c(-1,1),
                          ylim=c(-1,1),
                          bg="red",
                          point.cex=1,
                          pch=21,
                          text_name="auto",
                          text.name=NULL,
                          title="MDS plot "
){
  x <- con[, 1]
  y <- con[, 2]


  if(range=="auto"){
    lim <- c(-max(abs(con)), max(abs(con)))


    plot(x, y, pch=pch, bg=bg,
         xlim = lim,
         ylim = lim,
         xlab="Dim1", ylab = "Dim2",
         main=title,
         cex=point.cex
    )


  }else if(range=="manaual"){
    xlim = xlim
    ylim = ylim

    plot(x, y, pch=pch, bg=bg,
         xlim = xlim,
         ylim = ylim,
         xlab="Dim1", ylab = "Dim2",
         main=title,
         cex=point.cex
    )
  }


  grid()

  if(text_name=="input"){
    text(x, y,
         labels = text.name,
         cex = text.cex,
         pos =text.position,
         col=text.col)
  }else if(text_name=="auto"){
    text(x, y,
         cex = text.cex,
         pos =text.position,
         col=text.col)
  }


  abline(v=0, h=0, lty=2)

}





# 표제분석 데이터 ---
# options(scipen = 10)
# title_points <- Ohhaeun2023_matrix_2_data %>%  MDS()
# title_points
# title_points%>% cmdscale_plot()
# title_points%>% cmdscale_plot(
#   range = "manaual", xlim=c(-0.1, 0.1), ylim=c(-0.1, 0.1))
#






# mdsplot1<- function(point,variable=NULL,main_Title=NULL){
#   #par(mar=c(4,4,4,4))
#   plot(point[,1],point[,2], type = "p",
#        xlim=c(-3,3),ylim=c(-3,3),pch=21,bg=c(1:ncol(point)),cex=2,
#        main=main_Title)
#   #     xlab="편리성",ylab="mobile")
#   grid()
#   text(point+.2,rownames(variable),cex=1)
#   abline(v=0,lty=2,col="red")
#   abline(h=0,lty=2,col="red")
# }
# meanings = c("방향", "경로", "결과", "재료", "원인",
#              "자격", "간접", "이동", "장소", "시간",
#              "부류", "감정", "범위", "따름", "이음",
#              "소속", "비롯", "소유", "조건", "기준",
#              "함께", "더함", "수량", "시작", "단위",
#              "주격", "목적", "보격", "호격", "대조",
#              "화제", "강조", "초과", "비슷", "같음", "수용",
#              "의외", "놀람", "반복", "한정", "최소", "모두")
# # oh_ass_mat %>% dist() %>% cmdscale(k=2,eig=T)
# par(mfrow=c(1,1))
# cmdplot %>% mdsplot(xlim=c(-110, 101),
#                     ylim = c(-90, 120),
#                     bg="white",
#                     pch=3,
#                     type = "p",
#                     col="steelblue",
#                     text.cex = 1,
#                     name="manaual",
#                     text.name= meanings,
#                     title = "OHHaeun(2023):두번째표를 이용한 MDS plot",
#                     vjust=-3)

mdsplot_edit<- function(point,
                        variable="",
                        main_Title="",
                        xname="",
                        yname=""){
  par(mar=c(5,5,5,5))
  plot(point[,1],point[,2], type = "p",
       xlim=c(-3,3),ylim=c(-3,3),pch=21,bg=c(1:ncol(point)),cex=2,
       main=main_Title,
       xlab=xname,ylab=yname)
  #     xlab="편리성",ylab="mobile")
  grid()
  text(point+.2,rownames(variable),cex=1)
  abline(v=0,lty=2,col="red")
  abline(h=0,lty=2,col="red")
}



#MDS에 대한 데이터 isoMDS- data가 fulldata인 경우
#0을 포함한 자료인 경우
mdsDisMat<-function(data){
  library(dplyr)
  library(psych)
  library(lavaan)
  library(MASS)
  #  data<-mds_onl
  des.data <- data %>% as.data.frame() %>% describe()
  mat <- des.data[,3] %>% lav_matrix_upper2full() #행렬
  dist<- mat %>% scale()%>% dist() #표준화 및 거리 함수
  iso<- dist %>% isoMDS(k=2) #MDS분석
  isoplot <- iso$points #MDS결과
  colnames(isoplot )=c("x","y")
  #shepard plot
  dist_sh <- Shepard(mat[lower.tri(mat)], iso$points)
  cdist_sh <-cbind(dist_sh$x, dist_sh$y, dist_sh$yf)
  colnames(cdist_sh)=c("x","y","yf")
  result<-list(Matrix=mat,Distance=dist,isoMDS_Result=iso,Plot_data=isoplot,Shepard_data=cdist_sh)
  result
}
#실행
#mdsDisMat(mds_onl)


#6개에 맞추어 둠 행렬제작 ####
#MDS데이터에 0이 안들어간 경우 만들어주기

#6개에 맞추어 둠 행렬제작
#MDS데이터에 0이 안들어간 경우 만들어주기
mdsDataMaker <- function(data){
  library(psych)
  library(lavaan)
  library(dplyr)

  mds_data <- data %>% as.data.frame()
  mds0 =rep(0, nrow(mds_data)) %>% as.data.frame()

  dms_raw <-cbind(mds0,mds_data[,1:5],mds0,mds_data[,6:9],mds0,mds_data[,10:12],mds0,mds_data[,13:14],mds0,mds_data[,15],mds0)

  des.data <- describe(dms_raw)
  des.data <- des.data %>% as.data.frame()
  result.matrix <-  lav_matrix_upper2full(des.data[,3])
  colnames(result.matrix)=c("V1","V2","V3","V4","V5","V6")
  rownames(result.matrix)=c("V1","V2","V3","V4","V5","V6")
  #result.matrix
  result.matrix
}


#행렬의 열과 행의 이름 넣기####
matrix_name<- function(Matrix, name){
  colnames(Matrix)=name
  rownames(Matrix)=name
  Matrix
}




#SDS:scale -dist-shepard  plot####
#행렬을 입력하여 사용


mds_SDS_plot_data <-function(mat_data, opt="cmd"){
  library(dplyr)
  library(psych)
  # library(lavaan)
  library(MASS)

  #des.data <- data %>% describe(data)
  #mat <- des.data[,3] #행렬

  if(opt=="iso"){

    mat<- mat_data %>% scale() #표준화
    dist<-mat %>% dist() #거리함수
    iso <-isoMDS(dist, k=2) #MDS분석
    isoplot <- iso$points #MDS결과plot자료
    stress_value <-iso$stress


    #shepard plot
    dist_sh <- Shepard(dist, iso$points) #shepard plot 데이터
    cdist_sh=cbind(dist_sh$x, dist_sh$y, dist_sh$yf)
    colnames(cdist_sh)=c("원거리", "FitDist", "FitDATA")

    res=list(dist,
             # cmdscale= cmdplot,
             # cmdGOF = cmd_Gof,
             isoMDS=iso,
             Shepard=cdist_sh
    )

  }else if(opt=="cmd"){

    cmd<-cmdscale(dist,k=2,eig = T) #metric MDS
    cmdplot<-cmd$points
    cmd_Gof<-cmd$GOF


    res=list(dist,
             cmdscale= cmdplot,
             cmdGOF = cmd_Gof
             # ,
             # isoMDS=iso,
             # Shepard=cdist_sh
    )
  }
  res
}

#행렬만 가지고 분석
#mds_SDS_plot(on_mat)

#전체분석 도구 : :image를 순차적으로 분리함 #####

mds_SDS_plot_step <-function(mat_data,opt=TRUE,Cex=2){
  library(dplyr)
  library(psych)
  # library(lavaan)
  library(MASS)

  #des.data <- data %>% describe(data)
  #mat <- des.data[,3] #행렬


  mat<- mat_data %>% scale() #표준화
  dist<-mat %>% dist() #거리함수

  #isoMDS data
  iso <-isoMDS(dist, k=2) #MDS분석
  isoplot <- iso$points #MDS결과plot자료
  stress_value <-iso$stress

  #cmdscale MDS data
  cmd<-cmdscale(dist,k=2,eig = T) #metric MDS
  cmdplot<-cmd$points
  cmd_Gof<-cmd$GOF

  #shepard plot data
  dist_sh <- Shepard(dist, iso$points) #shepard plot 데이터
  cdist_sh=cbind(dist_sh$x, dist_sh$y, dist_sh$yf)
  colnames(cdist_sh)=c("원거리", "FitDist", "FitDATA")


  #par(mfrow=c(2,2))

  #cmdscale plot
  #바탕만들기

  x1<-cmdplot[,1]
  y1<-cmdplot[,2]
  limx1<-c(-max(abs(x1)),max(abs(x1)))
  limy1<-c(-max(abs(y1)),max(abs(y1)))

  plot(cmdplot[,1],cmdplot[,2],type = "n",#pch=21,bg=c(1:6),
       xlim=ifelse(limx1<0,limx1-0.5,limx1+0.5), ylim =ifelse(limy1<0,limy1-0.5,limy1+0.5),
       xlab="",ylab="",axes=FALSE)
  rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4], border = FALSE, col="gray80")
  #par("usr")
  #abline(h=-3:3,col="white")
  #abline(v=-3:3, col="white")
  par(new=TRUE)

  plot(cmdplot[,1],cmdplot[,2], type = "p",
       xlim=ifelse(limx1<0,limx1-0.5,limx1+0.5), ylim =ifelse(limy1<0,limy1-0.5,limy1+0.5),
       pch=21,bg="white",#c(1:ncol(cmdplot)),
       cex=2,
       main="Metric cmdscale  Perceptual Map ",
       xlab= paste("Goodnes Of Fit =",round(cmd_Gof[1]*100,2),collapse=""),ylab="y variable ")
  grid(col = "white")
  text(cmdplot +c(.1,-.1),
       rownames(cmdplot),cex= Cex,col = "black") #font size Cex
  abline(v=0,lty=2,col="gray40")
  abline(h=0,lty=2,col="gray40")
  mtext("Yong & Householder(1938)")



  #isoMDS plot
  #colnames(isoplot)=c("X-Rename Variable set","Y-Rename Variable set")
  #바탕을 회색으로 만들기
  x2<-isoplot[,1]
  y2<-isoplot[,2]
  limx2<-c(-max(abs(x2)),max(abs(x2)))
  limy2<-c(-max(abs(y2)),max(abs(y2)))

  plot(isoplot[,1],isoplot[,2],type = "n",pch=21,bg=c(1:6),
       xlim=ifelse(limx1<0,limx1-0.8,limx1+0.8), ylim =ifelse(limy1<0,limy1-0.8,limy1+0.8),
       xlab="",ylab="",axes=FALSE)
  rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4], border = FALSE, col="gray80")
  #par("usr")
  #abline(h=-3:3,col="white")
  #abline(v=-3:3, col="white")
  par(new=TRUE)

  plot(isoplot[,1],isoplot[,2], type = "p",
       xlim=ifelse(limx1<0,limx1-1,limx1+1), ylim =ifelse(limy1<0,limy1-0.5,limy1+0.5),
       pch=21,bg="white",#c(1:ncol(isoplot)),
       cex=2,
       main="Nonmetric isoMDS Perceptual Map ",
       xlab= paste("stress =",round(stress_value,5),collapse="") ,ylab="y variable")
  grid(col = "white")
  text(isoplot+c(.1,-.1),
       rownames(cmdplot),cex= Cex,col = "black")  #Cex: font size
  abline(v=0,lty=2,col="gray40")
  abline(h=0,lty=2,col="gray40")
  mtext("Shepard(1962a,b), Kruskal(19641,b)")

  #shepard plot
  plot(cdist_sh[,1],cdist_sh[,3], pch=".",
       xlim=range(cdist_sh[,1]), ylim = range(cdist_sh[,3]),
       xlab="Dissimilarity", ylab="Distance",main="MDS_Guttman(1968) Shepard Diagram  ")#,
  #xlim=range(cdist_sh[,1]), ylim = range(cdist_sh[,1]))
  grid()
  lines(cdist_sh[,1],cdist_sh[,3], type="p",pch=1,col="red")
  lines(cdist_sh[,1],cdist_sh[,3], type="S", col="red")
  abline(lm(cdist_sh[,3]~cdist_sh[,1]),lty=2,col="gray70")#regression line
  #abline(0,1,lty=2,col="gray70")
  mtext("45도에 가까운 선형이 나타나면 적합")

  #image plot
  plot(cdist_sh[,2],cdist_sh[,3],xlim=range(cdist_sh[,2]), ylim = range(cdist_sh[,3]),
       xlab="Fit.Dissimilarity", ylab="Distance",main="MDS_Residual plot(image plot)")
  grid()
  lines(cdist_sh[,2],cdist_sh[,3], type="p",pch=21, bg=c(1:10),cex=1.3)
  lines(cdist_sh[,2],cdist_sh[,3], type="S",lty=2,col="gray70")

  abline(lm(cdist_sh[,3]~cdist_sh[,2]),lty=2,col="red")#regression line
  #par(mfrow=c(1,1))


  res=list(dist, cmdscale=cmdplot,cmd_Gof, isoMDS=iso, Shepard=cdist_sh)
  if(opt==TRUE)  {print(res)}else{return("결과를 보려면 opt=TRUE or T를 입력하세요 ")}
}


#
# # mds_SDS_plot_step <-function(mat_data,opt=FALSE){
# library(dplyr)
# library(psych)
# # library(lavaan)
# library(MASS)
#
# #des.data <- data %>% describe(data)
# #mat <- des.data[,3] #행렬
#
#
# mat<- mat_data %>% scale() #표준화
# dist<-mat %>% dist() #거리함수
#
# #isoMDS data
# iso <-isoMDS(dist, k=2) #MDS분석
# isoplot <- iso$points #MDS결과plot자료
# stress_value <-iso$stress
#
# #cmdscale MDS data
# cmd<-cmdscale(dist,k=2,eig = T) #metric MDS
# cmdplot<-cmd$points
# cmd_Gof<-cmd$GOF
#
# #shepard plot data
# dist_sh <- Shepard(dist, iso$points) #shepard plot 데이터
# cdist_sh=cbind(dist_sh$x, dist_sh$y, dist_sh$yf)
# colnames(cdist_sh)=c("원거리", "FitDist", "FitDATA")
#
#
# #par(mfrow=c(2,2))
#
# #cmdscale plot
# #바탕만들기
#
# x1<-cmdplot[,1]
# y1<-cmdplot[,2]
# limx1<-c(-max(abs(x1)),max(abs(x1)))
# limy1<-c(-max(abs(y1)),max(abs(y1)))
#
# plot(cmdplot[,1],cmdplot[,2],type = "n",#pch=21,bg=c(1:6),
#      xlim=ifelse(limx1<0,limx1-0.5,limx1+0.5), ylim =ifelse(limy1<0,limy1-0.5,limy1+0.5),
#      xlab="",ylab="",axes=FALSE)
# rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4], border = FALSE, col="gray80")
# #par("usr")
# #abline(h=-3:3,col="white")
# #abline(v=-3:3, col="white")
# par(new=TRUE)
#
# plot(cmdplot[,1],cmdplot[,2], type = "p",
#      xlim=ifelse(limx1<0,limx1-0.5,limx1+0.5), ylim =ifelse(limy1<0,limy1-0.5,limy1+0.5),
#      pch=21,bg="white",#c(1:ncol(cmdplot)),
#      cex=2,
#      main="Metric cmdscale  Perceptual Map ",
#      xlab= paste("Goodnes Of Fit =",round(cmd_Gof[1]*100,2),collapse=""),ylab="y variable ")
# grid(col = "white")
# text(cmdplot +c(.1,-.1),
#      rownames(cmdplot),cex=1,col = "black")
# abline(v=0,lty=2,col="gray40")
# abline(h=0,lty=2,col="gray40")
# mtext("Yong & Householder(1938)")
#
#
#
# #isoMDS plot
# #colnames(isoplot)=c("X-Rename Variable set","Y-Rename Variable set")
# #바탕을 회색으로 만들기
# x2<-isoplot[,1]
# y2<-isoplot[,2]
# limx2<-c(-max(abs(x2)),max(abs(x2)))
# limy2<-c(-max(abs(y2)),max(abs(y2)))
#
# plot(isoplot[,1],isoplot[,2],type = "n",pch=21,bg=c(1:6),
#      xlim=ifelse(limx1<0,limx1-0.8,limx1+0.8), ylim =ifelse(limy1<0,limy1-0.8,limy1+0.8),
#      xlab="",ylab="",axes=FALSE)
# rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4], border = FALSE, col="gray80")
# #par("usr")
# #abline(h=-3:3,col="white")
# #abline(v=-3:3, col="white")
# par(new=TRUE)
#
# plot(isoplot[,1],isoplot[,2], type = "p",
#      xlim=ifelse(limx1<0,limx1-0.5,limx1+0.5), ylim =ifelse(limy1<0,limy1-0.5,limy1+0.5),
#      pch=21,bg="white",#c(1:ncol(isoplot)),
#      cex=2,
#      main="Nonmetric isoMDS Perceptual Map ",
#      xlab= paste("stress =",round(stress_value,5),collapse="") ,ylab="y variable")
# grid(col = "white")
# text(isoplot+c(.1,-.1),
#      rownames(cmdplot),cex=1,col = "black")
# abline(v=0,lty=2,col="gray40")
# abline(h=0,lty=2,col="gray40")
# mtext("Shepard(1962a,b), Kruskal(19641,b)")
#
# #shepard plot
# plot(cdist_sh[,1],cdist_sh[,3], pch=".",
#      xlim=range(cdist_sh[,1]), ylim = range(cdist_sh[,3]),
#      xlab="Dissimilarity", ylab="Distance",main="MDS_Guttman(1968) Shepard Diagram  ")#,
# #xlim=range(cdist_sh[,1]), ylim = range(cdist_sh[,1]))
# grid()
# lines(cdist_sh[,1],cdist_sh[,3], type="p",pch=1,col="red")
# lines(cdist_sh[,1],cdist_sh[,3], type="S", col="red")
# abline(lm(cdist_sh[,3]~cdist_sh[,1]),lty=2,col="gray70")#regression line
# #abline(0,1,lty=2,col="gray70")
# mtext("45도에 가까운 선형이 나타나면 적합")
#
# #image plot
# plot(cdist_sh[,2],cdist_sh[,3],xlim=range(cdist_sh[,2]), ylim = range(cdist_sh[,3]),
#      xlab="Fit.Dissimilarity", ylab="Distance",main="MDS_Residual plot(image plot)")
# grid()
# lines(cdist_sh[,2],cdist_sh[,3], type="p",pch=21, bg=c(1:10),cex=1.3)
# lines(cdist_sh[,2],cdist_sh[,3], type="S",lty=2,col="gray70")
#
# abline(lm(cdist_sh[,3]~cdist_sh[,2]),lty=2,col="red")#regression line
# #par(mfrow=c(1,1))
#
#
# res=list(dist, cmdscale=cmdplot,cmd_Gof, isoMDS=iso, Shepard=cdist_sh)
# if(opt==TRUE)  {print(res)}else{return("결과를 보려면 opt=TRUE or T를 입력하세요 ")}

#mds_SDS_plot_step(on_mat,T)

#mds_SDS_plot_step(on_mat,F)

#연구결과 제출시 활용할 데이터 ####
mds_SDS_plot2by2 <-function(mat_data, opt=FALSE){
  library(dplyr)
  library(psych)
  library(lavaan)
  library(MASS)

  #des.data <- data %>% describe(data)
  #mat <- des.data[,3] #행렬


  mat<- mat_data %>% scale() #표준화
  dist<-mat %>% dist() #거리함수

  #isoMDS data
  iso <-isoMDS(dist, k=2, trace = FALSE) #MDS분석
  isoplot <- iso$points #MDS결과plot자료
  stress_value <-iso$stress

  #cmdscale MDS data
  cmd<-cmdscale(dist,k=2,eig = T) #metric MDS
  cmdplot<-cmd$points
  cmd_Gof<-cmd$GOF

  #shepard plot data
  dist_sh <- Shepard(dist, iso$points) #shepard plot 데이터
  cdist_sh=cbind(dist_sh$x, dist_sh$y, dist_sh$yf)
  colnames(cdist_sh)=c("원거리", "FitDist", "FitDATA")


  par(mfrow=c(2,2))

  #cmdscale plot
  #바탕만들기

  x1<-cmdplot[,1]
  y1<-cmdplot[,2]
  limx1<-c(-max(abs(x1)),max(abs(x1)))
  limy1<-c(-max(abs(y1)),max(abs(y1)))

  plot(cmdplot[,1],cmdplot[,2],type = "n",#pch=21,bg=c(1:6),
       xlim=limx1, ylim = limy1,
       xlab="",ylab="",axes=FALSE)
  rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4], border = FALSE, col="gray80")
  #par("usr")
  #abline(h=-3:3,col="white")
  #abline(v=-3:3, col="white")
  par(new=TRUE)

  plot(cmdplot[,1],cmdplot[,2], type = "p",
       xlim=limx1, ylim = limy1,
       pch=21,bg="white",#c(1:ncol(cmdplot)),
       cex=2,
       main="Metric cmdscale  Perceptual Map ",
       xlab= paste("Goodnes Of Fit =",round(cmd_Gof[1]*100,2),collapse=""),ylab="y variable ")
  grid(col = "white")
  text(cmdplot +c(.1,-.1),
       rownames(cmdplot),cex=1,col = "black")
  abline(v=0,lty=2,col="gray40")
  abline(h=0,lty=2,col="gray40")
  mtext("Yong & Householder(1938)")



  #isoMDS plot
  #colnames(isoplot)=c("X-Rename Variable set","Y-Rename Variable set")
  #바탕을 회색으로 만들기
  x2<-isoplot[,1]
  y2<-isoplot[,2]
  limx2<-c(-max(abs(x2)),max(abs(x2)))
  limy2<-c(-max(abs(y2)),max(abs(y2)))

  plot(isoplot[,1],isoplot[,2],type = "n",pch=21,bg=c(1:6),
       xlim=limx2, ylim = limy2,
       xlab="",ylab="",axes=FALSE)
  rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4], border = FALSE, col="gray80")
  #par("usr")
  #abline(h=-3:3,col="white")
  #abline(v=-3:3, col="white")
  par(new=TRUE)

  plot(isoplot[,1],isoplot[,2], type = "p",
       xlim=limx2, ylim = limy2,
       pch=21,bg="white",#c(1:ncol(isoplot)),
       cex=2,
       main="Nonmetric isoMDS Perceptual Map ",
       xlab= paste("stress =",round(stress_value,5),collapse="") ,ylab="y variable")
  grid(col = "white")
  text(isoplot+c(.1,-.1),
       rownames(cmdplot),cex=1,col = "black")
  abline(v=0,lty=2,col="gray40")
  abline(h=0,lty=2,col="gray40")
  mtext("Shepard(1962a,b), Kruskal(19641,b)")

  #shepard plot
  plot(cdist_sh[,1],cdist_sh[,3], pch=".",
       xlim=range(cdist_sh[,1]), ylim = range(cdist_sh[,3]),
       xlab="Dissimilarity", ylab="Distance",main="MDS_Guttman(1968) Shepard Diagram  ")#,
  #xlim=range(cdist_sh[,1]), ylim = range(cdist_sh[,1]))
  grid()
  lines(cdist_sh[,1],cdist_sh[,3], type="p",pch=1,col="red")
  lines(cdist_sh[,1],cdist_sh[,3], type="S", col="red")
  abline(lm(cdist_sh[,3]~cdist_sh[,1]),lty=2,col="gray70")#regression line
  #abline(0,1,lty=2,col="gray70")
  mtext("45도에 가까운 선형이 나타나면 적합")

  #image plot
  plot(cdist_sh[,2],cdist_sh[,3],xlim=range(cdist_sh[,2]), ylim = range(cdist_sh[,3]),
       xlab="Fit.Dissimilarity", ylab="Distance",main="MDS_#Residual plot(image plot)")
  grid()
  lines(cdist_sh[,2],cdist_sh[,3], type="p",pch=21, bg=c(1:10),cex=1.3)
  lines(cdist_sh[,2],cdist_sh[,3], type="S",lty=2,col="gray70")

  abline(lm(cdist_sh[,3]~cdist_sh[,2]),lty=2,col="red")#regression line
  par(mfrow=c(1,1))


  res<- list(dist, cmdscale=cmdplot,cmd_Gof, isoMDS=iso, Shepard=cdist_sh)
  if(opt==TRUE)print(res)
}

#Classical multidimensional scaling (MDS) of a data matrix. Also known as principal coordinates analysis (Gower, 1966).

#Usage
#mds_SDS_plot2by2(on_mat, opt=TRUE)
#mtext("전체데이터")
#mds_SDS_plot2by2(gachon,TRUE)
#mtext("가천대 대학원생 ")
#mds_SDS_plot2by2(yonsei,F)
#mtext("연세대 대학원생")
#mds_SDS_plot2by2(academy)
#mtext("평생교육원 수강생")


#cmdcale and isoMDS plot ####
cmdscale_plot <- function(mds_data, title="",line=1:6,...) {
  if(is.numeric(line)==FALSE){
    stopifnot("입력란이 비었습니다. 숫자로다시입력하세요 ")
  }
  library(shapes)

  cmdplot_d <- mds_data
  cmdplot   <- cmdplot_d$cmdscale
  cmd_Gof   <- cmdplot_d$cmdGOF

  mds_data_d <- mds_data
  mds_data1  <- mds_data_d$cmdscale
  mds_stress <- mds_data_d$cmdGOF

  par(mfrow=c(1,2))
  #order
  plot(cmdplot[,1],cmdplot[,2], type = "b",
       xlim=c(-2.5,2.5),
       ylim=c(-2.5,2.5),lty=2,col=1,
       pch=21,bg=c(1:nrow(cmdplot)),cex=0.9,
       main= title ,
       xlab= paste("GOF =",round(cmd_Gof[1],3),collapse=""),ylab="y variable - Must Rename"
  )
  grid()
  text(cmdplot+c(-.2, .3),rownames(cmdplot),cex=.9, col = c(1,1,1,2,2,2))
  abline(v=0,lty=2,col="gray50")
  abline(h=0,lty=2,col="gray60")


  #shape 1,4,2,5,3,6
  plotshapes(mds_data1, joinline=c(line,...),color="black",symbol = 1)#mteric MDS 형상
  abline(v=0,h=0,col="gray50")
  points(mds_data1,pch=21,bg=c(3,3,3,6,6,6),cex=1)
  grid()
  text(mds_data1+c(-.2, .3),rownames(mds_data1),cex=0.9, col = c(rep("blue",3), rep("red",3)))
  title(paste("NonMetric MDS Shape of", title),paste("GOF(%) =",round(mds_stress*100,2)))
  par(mfrow=c(1,1))

}
#실행
# cmdscale_plot(mds_online)
# cmdscale_plot(mds_online,"Total MDS")
# cmdscale_plot(mds_online,"Total MDS", 1,6,3,2,4,5,1)
# cmdscale_plot(mds_gachon,"gachon student",1,6,3,2,4,5,1)
# cmdscale_plot(mds_yonsei,"yonsei student",1,6,3,2,4,5,1)
# cmdscale_plot(mds_academy,"Lecture",1,6,3,2,4,5,1)



#compare metric and nonmetric MDS
#계량형과 비계량형을 비교
MDS_plot <- function(mds_data,title="",
                     line=c(1:6,1),...) {
  if(is.character(title)==FALSE){
    stopifnot("input title ")
  }
  library(shape)

  cmdplot_d <- mds_data
  cmdplot   <- cmdplot_d$cmdscale
  cmd_Gof   <- cmdplot_d$cmdGOF

  mds_data_d <- mds_data
  mds_data1  <- mds_data_d$isoMDS$points
  mds_stress <- mds_data_d$isoMDS$stress

  #par(bg = "gray90")
  par(mfrow=c(1,2)) #1x2 figuare
  #Metric MDS shape
  plotshapes(cmdplot, joinline=c(line,...),color="red",symbol = 1)#mteric MDS 형상
  abline(v=0,h=0)
  points(cmdplot,pch=21,bg=c(1,1,1,6,6,6),cex=1)
  grid()
  text(cmdplot+c(-.2, .3),rownames(cmdplot),cex=0.9, col = c(rep("blue",3), rep("red",3)))
  title(paste("Metric MDS of", title),paste("GOF(%) =",round(cmd_Gof*100),2))

  #Nonmetric shape coordinated
  plotshapes(mds_data1, joinline=c(line,...),color="red",symbol = 1)#mteric MDS 형상
  abline(v=0,h=0)
  points(mds_data1,pch=21,bg=c(1,1,1,6,6,6),cex=1)
  grid()
  text(mds_data1+c(-.2, .3),rownames(mds_data1),cex=0.9, col = c(rep("blue",3), rep("red",3)))
  title(paste("NonMetric MDS  of", title),paste("stress =",round(mds_stress/100,4)))

  par(mfrow=c(1,1))


}

#실행
#par(bg = "white")
# MDS_plot(mds_online)
# MDS_plot(mds_online,"total")
# MDS_plot(mds_online,"total", 1,6,2,3,4,5,1)
# MDS_plot(mds_gachon,"Gachon",1,5,4,2,3,6,1)
# MDS_plot(mds_yonsei,"yonsei",1,5,4,3,6,2,1)
# MDS_plot(mds_academy,"lecture",1,6,3,2,4,5,1)



#각각 하나만을 그려야 하는 경우
cmd_single_plot <- function(mds_data,title="",line=c(1:6,1),...) {
  if(is.character(title)==FALSE){
    stopifnot("input title ")
  }
  library(shape)

  cmdplot_d <- mds_data
  cmdplot   <- cmdplot_d$cmdscale
  cmd_Gof   <- cmdplot_d$cmdGOF

  mds_data_d <- mds_data
  mds_data1  <- mds_data_d$isoMDS$points
  mds_stress <- mds_data_d$isoMDS$stress

  #par(bg = "gray90")
  #par(mfrow=c(1,2)) #1x2 figuare
  #Metric MDS shape
  plotshapes(cmdplot, joinline=c(line,...),color="red",symbol = 1)#mteric MDS 형상
  abline(v=0,h=0)
  points(cmdplot,pch=21,bg=c(1,1,1,6,6,6),cex=1)
  grid()
  text(cmdplot+c(-.2, .3),rownames(cmdplot),cex=0.9, col = c(rep("blue",3), rep("red",3)))
  title(paste("Metric MDS of", title),paste("GOF(%) =",round(cmd_Gof*100),2))

  par(mfrow=c(1,1))

}

iso_single__plot <- function(mds_data,title="",line=c(1:6,1),...) {
  if(is.character(title)==FALSE){
    stopifnot("input title ")
  }
  library(shape)

  cmdplot_d <- mds_data
  cmdplot   <- cmdplot_d$cmdscale
  cmd_Gof   <- cmdplot_d$cmdGOF

  mds_data_d <- mds_data
  mds_data1  <- mds_data_d$isoMDS$points
  mds_stress <- mds_data_d$isoMDS$stress



  #Nonmetric shape coordinated
  plotshapes(mds_data1, joinline=c(line,...),color="red",symbol = 1)#mteric MDS 형상
  abline(v=0,h=0)
  points(mds_data1,pch=21,bg=c(1,1,1,6,6,6),cex=1)
  grid()
  text(mds_data1+c(-.2, .3),rownames(mds_data1),cex=0.9, col = c(rep("blue",3), rep("red",3)))
  title(paste("NonMetric MDS  of", title),paste("stress =",round(mds_stress/100,4)))

  par(mfrow=c(1,1))
}

#형상비교를 위한 plot함수
plotShapes<- function (A, B = 0, joinline = c(1, 1), orthproj = c(1, 2), color = 1,
                       symbol = 1) {
  CHECKOK <- TRUE
  if (is.array(A) == FALSE) {
    if (is.matrix(A) == FALSE) {
      cat("Error !! argument should be an array or matrix \n")
      CHECKOK <- FALSE
    }
  }
  if (CHECKOK) {
    k <- dim(A)[1]
    m <- dim(A)[2]
    kk <- k
    if (k >= 15) {
      kk <- 1
    }

    par(pty = "s")
    if (length(c(B)) != 1) {
      par(mfrow = c(1, 2))
    }
    if (length(dim(A)) == 3) {
      A <- A[, orthproj, ]
    }
    if (is.matrix(A) == TRUE) {
      a <- array(0, c(k, 2, 1))
      a[, , 1] <- A[, orthproj]
      A <- a
    }

    out <- defplotsize2(A)
    width <- out$width
    if (length(c(B)) != 1) {
      if (length(dim(B)) == 3) {
        B <- B[, orthproj, ]
      }
      if (is.matrix(B) == TRUE) {
        a <- array(0, c(k, 2, 1))
        a[, , 1] <- B[, orthproj]
        B <- a
      }
      ans <- defplotsize2(B)
      width <- max(out$width, ans$width)
    }
    n <- dim(A)[3]
    lc <- length(color)
    lt <- k * m * n/lc
    color <- rep(color, times = lt)
    lc <- length(symbol)
    lt <- k * m * n/lc
    symbol <- rep(symbol, times = lt)

    plot(A[, , 1], xlim = c(out$xl, out$xl + width), ylim = c(out$yl,
                                                              out$yl + width), type = "n", xlab = " ",
         ylab = " ",pch=21,bg=(1:ncol(A)))
    grid()
    abline(v=0,h=0,col="gray50",lty=2)


    for (i in 1:n) {
      dplyr::select <- ((i - 1) * k * m + 1):(i * k * m)
      points(A[, , i], pch = symbol[dplyr::select], col = color[dplyr::select])
      lines(A[joinline, , i])
      # text(A + c(-.2, .3),rownames(A),cex=0.9)
    }
    if (length(c(B)) != 1) {
      A <- B
      if (is.matrix(A) == TRUE) {
        a <- array(0, c(k, 2, 1))
        a[, , 1] <- A
        A <- a
      }
      out <- defplotsize2(A)
      n <- dim(A)[3]
      plot(A[, , 1], xlim = c(ans$xl, ans$xl + width),
           ylim = c(ans$yl, ans$yl + width), type = "n",
           xlab = " ", ylab = " ",pch=21,bg=(1:nrow(A)))
      grid()
      abline(v=0,h=0,col="gray50",lty=2)



      for (i in 1:n) {
        points(A[, , i], pch = symbol[dplyr::select], col = color[dplyr::select])
        lines(A[joinline, , i])
        #text(A+c(-.2, .3),rownames(A),cex=0.9, col = c(1:nrow(A)))
      }
    }
  }
}
#사용자 함수
# plotShapes(mds_online$cmdscale,mds_gachon$cmdscale,color=2,symbol = 2,joinline=c(1,4,5,2,3,6,1))

#원래함수
# library(shapes)
# plotshapes(mds_online$cmdscale,mds_gachon$cmdscale,color=3,symbol = 21,joinline=c(1,4,5,2,3,6,1))





#형상 분석
#nonmetric- metric shape analysis ~ procrustes analysis
#이름을 지정해주어야 함.
vv=c("ZOOM","Webex","googleMEET","Youtube","naveBand","prism")
OPA_Plot <- function(metricD,nonmetricD,title="",row_name,line=c(1:6,1),...) {
  if(is.character(title)==FALSE){
    stopifnot("input title ")
  }
  library(shape)
  #metric(뒤) onto nonmetirc(앞)
  pnm<- procOPA(nonmetricD, metricD)
  #nonmetric on metirc
  pmn<- procOPA(metricD, nonmetricD)


  OPA_data_nm <- pnm
  OPA_data1  <- OPA_data_nm$Bhat
  OPA_OSS_nm    <- OPA_data_nm$OSS
  OPA_rmsd_nm   <- OPA_data_nm$rmsd

  OPA_data_mn <- pmn
  OPA_data2  <- OPA_data_mn$Bhat
  OPA_OSS_mn   <- OPA_data_mn$OSS
  OPA_rmsd_mn  <- OPA_data_mn$rmsd

  rownames(OPA_data1 )= row_name
  rownames(OPA_data2 )= row_name


  #metric shape coordinated
  par(mfrow = c(1,2))
  plotshapes( OPA_data1, joinline=c(line,...),color="red",symbol = 1)#mtericOPA 형상
  abline(v=0,h=0)
  points(OPA_data1,pch=21,bg=c(line,...),cex=1.5)
  grid()
  text(OPA_data1+c(-.2, .1),rownames(OPA_data1),cex=0.9, col = c(rep("blue",3), rep("red",3)))
  title(paste("shape analysis::", title),paste("OPSS =", round(OPA_OSS_nm),2),
        paste("RMSD = ",round(OPA_rmsd_nm,2)))


  #Non-metric shape coordinated
  plotshapes( OPA_data2, joinline=c(line,...),color="red",symbol = 1)#mtericOPA 형상
  abline(v=0,h=0)
  points(OPA_data2,pch=21,bg=c(line,...),cex=1.5)
  grid()
  text(OPA_data2+c(-.2, .1),rownames(OPA_data2),cex=0.9, col = c(rep("blue",3), rep("red",3)))
  title(paste("shape analysis::", title),paste("OPSS =", round(OPA_OSS_mn),2),
        paste("RMSD = ",round(OPA_rmsd_mn,2)))





  par(mfrow=c(1,1))

  tabnm <-cbind(OPA_OSS_nm,OPA_rmsd_nm)
  tabmn <-cbind(OPA_OSS_mn,OPA_rmsd_mn)
  tablefull<-rbind(tabnm, tabmn)
  rownames(tablefull)=c("n -> m", "m -> n")
  colnames(tablefull)=c("OPSS","RMSD")

  res<-list(PNM=OPA_data1,PMN=OPA_data2,OPSS_RMSD=tablefull)
  res
}

#metric MDS와 Nonmetric MDS형상 비교 ######
#자료를 활용할 것 MDS분석
#mds_SDS_plot(on_mat) 를 이용하여 MDS시행
#match A onto B
# OPA_Plot(mds_online$cmdscale, mds_online$isoMDS$points,"Total OPA",vv,1,2,5,4,6,3,1)
# OPA_Plot(mds_gachon$cmdscale, mds_gachon$isoMDS$points,"gachon OPA",vv,1,2,5,4,6,3,1)
# OPA_Plot(mds_yonsei$cmdscale, mds_yonsei$isoMDS$points,"yonsei OPA",vv,1,2,5,4,6,3,1)
# OPA_Plot(mds_academy$cmdscale, mds_academy$isoMDS$points,"academy OPA",vv,1,2,5,4,6,3,1)
# OPA_Plot(mds_grad$cmdscale, mds_grad$isoMDS$points,"grad OPA",vv,1,2,5,4,6,3,1)


#카이제곱 오류해결######
#The code for Monte Carlo simulation is a C translation of the Fortran algorithm of Patefield (1981).
#Hope, A. C. A. (1968). A simplified Monte Carlo significance test procedure. Journal of the Royal Statistical Society Series B, 30, 582–598. http://www.jstor.org/stable/2984263.
#Agresti, A. (2007). An Introduction to Categorical Data Analysis, 2nd ed. New York: John Wiley & Sons. Page 38.

advanced.chisq.test<-function(cri, option=TRUE, ...){
  cc <- cri[rowSums(cri)>0,]
  if(option == TRUE){
    cat(paste0("정확검정을 위해 Monte Carlo simulation이 ",...,"회 실행되었습니다(박중희, 2020).","\n"))
    chisq.test(cc, simulate.p.value = TRUE, ...)
  }
  else{chisq.test(cc)
    cat(" Monte Carlo simulation, option=TRUE, B=회수로 실행하세요. \n")}
}
#advanced.chisq.test(online_freq,F, B=5000)


# #Dplyr해결 #####
# library(dplyr, warn.conflicts = FALSE)
# # Suppress summarise info
# options(dplyr.summarise.inform = FALSE)
#




#MDS data output 
ma_data <- function(mds_data,
                    show="all",
                    variable= c("ZOOM","Webex","googleMEET",
                                "Youtube","NaverBand","Prism"),
                    addxy=0.5,
                    linetype=1, linecolor='gray40', linewidth= 0.8,
                    xl=-3, xh=3, 
                    yl=-2.3, yh=2,
                    text_size=5,
                    size.x=14,
                    
                    caption="MDS"
){
  library(psych)
  library(lavaan)
  library(tidyverse)
  library(MASS)
  library(ggrepel)
  library(knitr)
  # library(cluster)
  # Set it globally:
  options(ggrepel.max.overlaps = Inf)
  
  #박중희 2023을 위해 필요 
  load(file ="mds_list.RData")
  index = mds_list
  
  if(is.character(variable ==FALSE)){
    stopifnot("input again")
  }
  
  
  
  var <- variable
  
  
  mds_data <- mds_data %>% as.data.frame()
  
  mds0 = rep(0, nrow(mds_data)) %>% as_tibble()
  
  dms_raw <- cbind(mds0,mds_data[,1:5],
                   mds0,mds_data[,6:9],
                   mds0,mds_data[,10:12],
                   mds0,mds_data[,13:14],
                   mds0,mds_data[,15],mds0)
  
  des.data <- psych::describe(dms_raw) %>% as.data.frame()
  
  result.matrix <- des.data[,3] %>% lav_matrix_upper2full()
  
  
  
  
  if(length(variable) != 0){
    colnames(result.matrix)=variable
    rownames(result.matrix)=variable}
  #result.matrix
  
  cmd.data <- result.matrix %>%
    scale() %>% 
    dist() %>% 
    cmdscale(k=2, eig = T)
  
  cmdplot <- cmd.data$points
  cmdgof <- cmd.data$GOF[1]
  
  
  iso.data <- result.matrix %>%
    scale() %>% 
    dist() %>%
    isoMDS(k=2)
  isoplot <- iso.data$points
  isostress <- iso.data$stress
  # iso_sig = cut(isostress, breaks = c(0, 0.0025, 0.05, 0.1, 0.2,Inf),
  #     label=c("perfect","excellent","good","fair","poor" )) 
  iso_sig = cut(isostress, 
                breaks = c(0, 0.0024, 0.049, 0.099, 0.19,Inf),
                label=c("perfect","excellent","good","fair","poor" )) 
  
  
  colName= c("x","y")
  colnames(cmdplot)= colName
  colnames(isoplot)= colName
  
  
  
  
  
  mat <- result.matrix 
  dist <- mat %>% dist() #거리함수
  
  # 척도가 다른 경우 표준화 진행 할 것 
  mat_scale <- result.matrix %>% scale() #표준화  
  dist_scale <- mat_scale %>% dist() # 표준화 거리함수
  # dist_diasy <-mat %>% diasy(metric= "grower") # 비계량 거리함수
  
  #shepard plot data 
  # dist_sh <- Shepard(dist, iso.data$points) #shepard plot 데이터 
  # cdist_sh=cbind(dist_sh$x, dist_sh$y, dist_sh$yf)
  # colnames(cdist_sh)=c("원거리", "FitDist", "FitDATA")
  dist_sh <- Shepard(dist, iso.data$points) #shepard plot 데이터 
  cdist_sh=cbind(dist_sh$x, dist_sh$y, dist_sh$yf)
  colnames(cdist_sh)=c("Dissimilarity_dist", "FitDist", "FitData")
  shepardImage_data = cbind(index,cdist_sh )
  
  
  separd_plot_data= cdist_sh[,c(1,3)] 
  image_plot_data= cdist_sh[,c(2,3)] 
  
  #plot 
  cmd_plot = cmdplot %>% data.frame() %>% mutate(app = var) %>% 
    ggplot(aes(x=x, y=y))+
    geom_point(aes(color=app), size= 5, show.legend = FALSE)+
    geom_text_repel(aes(label=app),size= 6,
                    box.padding = 1)+
    ylim(yl, yh)+xlim(xl, xh)+
    theme_bw()+
    geom_hline(yintercept=0, linetype="dashed",
               linewidth=0.8, col="gray40")+
    geom_vline(xintercept=0, linetype="dashed", 
               linewidth=0.8, col="gray40")+
    labs(x= paste0(caption, "[GOF:",round(cmdgof*100,2),"%]"))+
    theme(axis.title.x= element_text(size=size.x))
  
  
  
  # plot 
  iso_plot = isoplot %>% data.frame() %>% mutate(app= var) %>% 
    ggplot(aes(x=x, y=y))+
    geom_point(aes(color=app), size= 5, show.legend = FALSE)+
    geom_text_repel(aes(label=app),size=text_size,
                    box.padding = 1)+
    ylim(yl, yh)+xlim(xl, xh)+
    theme_bw()+
    geom_hline(yintercept=0, linetype="dashed", 
               linewidth=0.8, col="gray40")+
    geom_vline(xintercept=0, linetype="dashed", 
               linewidth=0.8, col="gray40")+
    labs(x=paste0(caption,"[stress:", 
                  round(isostress,3),"(",iso_sig,")]"))+
    theme(axis.title.x= element_text(size=size.x) )
  
  
  
  
  Shepard = separd_plot_data %>% data.frame() %>% 
    ggplot(aes(x=Dissimilarity_dist, y=FitData))+
    geom_point(size=3)+
    geom_line(linewidth=1, linetype ="dashed")+
    theme_bw()+
    geom_smooth(method=lm, se=T, fill="gray75")
  
  
  Imageplot= image_plot_data%>% data.frame() %>%  
    ggplot(aes(x=FitDist, y=FitData))+
    geom_point(size=3, col="red")+
    geom_line(linewidth=1, linetype ="dashed")+
    theme_bw()+
    geom_smooth(method=lm, se=T, fill="gray70")
  
  #shape analysis 
  iso_plot_line = isoplot %>% data.frame() %>% mutate(app= var) %>% 
    ggplot(aes(x=x, y=y))+
    geom_path(linewidth= linewidth, 
              color=linecolor, linetype= linetype) +
    geom_point(aes(color=app), size= 5, show.legend = FALSE)+
    geom_text_repel(aes(label=app),size=text_size,
                    box.padding = 1)+
    ylim(yl, yh)+xlim(xl, xh)+
    theme_bw()+
    geom_hline(yintercept=0, linetype="dashed", 
               linewidth=0.8, col="gray40")+
    geom_vline(xintercept=0, linetype="dashed", 
               linewidth=0.8, col="gray40")+
    labs(x=paste0(caption,"[stress:", 
                  round(isostress,3),"(",iso_sig,")]"))+
    theme(axis.title.x= element_text(size=15) )  # added path 
  #shape analysis 
  cmd_plot_line = cmdplot %>% data.frame() %>% mutate(app= var) %>% 
    ggplot(aes(x=x, y=y))+
    geom_path(linewidth=linewidth, 
              color=linecolor, linetype= linetype)+  # added path 
    geom_point(aes(color=app), size= 5, show.legend = FALSE)+
    geom_text_repel(aes(label=app),size=text_size,
                    box.padding = 1)+
    ylim(yl, yh)+xlim(xl, xh)+
    theme_bw()+
    geom_hline(yintercept=0, linetype="dashed",
               linewidth=0.8, col="gray40")+
    geom_vline(xintercept=0, linetype="dashed", 
               linewidth=0.8, col="gray40")+
    labs(x= paste0(caption, "[GOF:",round(cmdgof*100,2),"%]"))+
    theme(axis.title.x= element_text(size=size.x))
  
  
  #plot size auto
  #shape analysis 
  iso_plot_line_auto = isoplot %>% data.frame() %>% mutate(app= var) %>% 
    ggplot(aes(x=x, y=y))+
    geom_path(linewidth= linewidth, 
              color=linecolor, linetype= linetype) +
    geom_point(aes(color=app), size= 5, show.legend = FALSE)+
    geom_text_repel(aes(label=app),size=text_size,
                    box.padding = 1)+
    # ylim(yl, yh)+xlim(xl, xh)+
    theme_bw()+
    geom_hline(yintercept=0, linetype="dashed", 
               linewidth=0.8, col="gray40")+
    geom_vline(xintercept=0, linetype="dashed", 
               linewidth=0.8, col="gray40")+
    labs(x=paste0(caption,"[stress:", 
                  round(isostress,3),"(",iso_sig,")]"))+
    theme(axis.title.x= element_text(size=size.x) )  # added path 
  #shape analysis 
  cmd_plot_line_auto = cmdplot %>% data.frame() %>% mutate(app= var) %>% 
    ggplot(aes(x=x, y=y))+
    geom_path(linewidth=linewidth, 
              color=linecolor, linetype= linetype)+  # added path 
    geom_point(aes(color=app), size= 5, show.legend = FALSE)+
    geom_text_repel(aes(label=app),size=text_size,
                    box.padding = 1)+
    # ylim(yl, yh)+xlim(xl, xh)+
    theme_bw()+
    geom_hline(yintercept=0, linetype="dashed",
               linewidth=0.8, col="gray40")+
    geom_vline(xintercept=0, linetype="dashed", 
               linewidth=0.8, col="gray40")+
    labs(x= paste0(caption, "[GOF:",round(cmdgof*100,2),"%]"))+
    theme(axis.title.x= element_text(size=size.x))
  
  
  
  
  
  
  switch(show,
         all = list(
           data_matrix = result.matrix,
           scale_mat = mat,
           dist_matrix = dist,
           
           cmdScale=cmdplot,
           isoMDS=isoplot,
           cmd_GOF= paste0(round(cmdgof*100,2),"(%)"),
           strss = paste0(round(isostress,4), " (",iso_sig,")" ),
           
           separd = separd_plot_data,
           image= image_plot_data
         ),
         
         cmdscale = cmd.data,
         isoMDS = iso.data,
         
         cmdplot_data = cmdplot, 
         isoplot_data = isoplot,
         
         plot_data = list(cmd=cmdplot, iso=isoplot),
         cmd_plot = cmd_plot,
         iso_plot = iso_plot,
         
         shepard_plot = Shepard,
         image_plot = Imageplot,
         iso_plot_line = iso_plot_line, #shape anlaysis
         iso_line = iso_plot_line, #shape anlaysis
         iso_line_auto = iso_plot_line_auto, #shape anlaysis
         cmd_plot_line = cmd_plot_line, #shape anlaysis
         cmd_line = cmd_plot_line, #shape anlaysis
         cmd_line_auto = cmd_plot_line_auto, #shape anlaysis
         
         cmd_gof= paste0(round(cmdgof*100,2),"(%)"),
         stress = paste0(round(isostress,4), " (",iso_sig,")" ),
         
         shepard_data = separd_plot_data,
         image_data= image_plot_data,
         shepardimage_data= shepardImage_data, #shepard and image data 
         mat = result.matrix,
         data_mat = result.matrix,
         scale = mat_scale,
         scale_mat = dist_scale,
         dist = dist,
         dist_mat = dist,
         des = des.data,
         gof = cbind(Metric_MDS_gof= cmdgof, Non_metric_MDS_stress= isostress),
         index = cbind.data.frame(gof= cmdgof, stress=isostress))
  
}

# ma_data(mds_onl[1:20,])
#  비유사성 자료 mds_onl_i를 사용하여 계산할 것 
# mds_onl_i %>% ma_data()
# mds_onl_i %>% ma_data(show="all")
# 
# mds_onl_i %>% ma_data(show="data_mat") 
# mds_onl_i %>% ma_data(show="data_mat") %>% dist()
# mds_onl_i %>% ma_data(show="dist_mat")
# mds_onl_i %>% ma_data(show="dist_mat") %>% as.matrix() %>% 
#   markdown_table("Distance matrix", digits=3)



### New MDS함수 plot comparison 제작 ----
mds_makePlot <- function(mds_data,
                         grid_title="",
                         xl = -3,
                         xh =  3,
                         yl = -2.3,
                         yh =  2,
                         variable= c("ZOOM","Webex","googleMEET",
                                     "Youtube","naveBand","prism"),
                         show="manual", #"auto","line"
                         linecolor="gray50", #shape analysis 
                         linetype= 1, 
                         linewidth=1,
                         what=""
                         
                         
){
  library(psych)
  library(lavaan)
  library(tidyverse)
  library(MASS)
  library(gridExtra)
  library(ggrepel)
  
  if(is.character(variable ==FALSE)){
    stopifnot("input again")
  }
  #
  
  var= variable
  
  # generate matrix
  mds_data<-mds_data %>% as.data.frame()
  mds0 =rep(0, nrow(mds_data)) %>% as_tibble()
  
  dms_raw <-cbind(mds0,mds_data[,1:5],
                  mds0,mds_data[,6:9],
                  mds0,mds_data[,10:12],
                  mds0,mds_data[,13:14],
                  mds0,mds_data[,15],mds0)
  
  des.data <- psych::describe(dms_raw) %>% as.data.frame()
  
  result.matrix <- des.data[,3] %>% lav_matrix_upper2full()
  
  
  if(length(variable) != 0){
    colnames(result.matrix)=variable
    rownames(result.matrix)=variable}
  #result.matrix
  
  
  
  
  
  #scale- dist - mds 
  cmd.data <- result.matrix %>%
    scale() %>% 
    dist() %>% 
    cmdscale(k=2, eig = T)
  
  cmdplot <- cmd.data$points
  cmdgof <- cmd.data$GOF[1]
  
  iso.data <- result.matrix %>%
    scale() %>% 
    dist() %>%
    isoMDS(k=2)
  
  isoplot <- iso.data$points
  isostress <- iso.data$stress
  iso_sig = cut(isostress, breaks = c(0, 0.0025, 0.05, 0.1, 0.2,Inf),
                label=c("perfect","excellent","good","fair","poor" )) 
  
  colName= c("x","y")
  colnames(cmdplot)= colName
  colnames(isoplot)= colName
  
  
  
  
  
  #shepard plot data 
  mat <- result.matrix %>% scale() #표준화  
  dist <-mat %>% dist() #거리함수
  #shepard plot data 
  dist_sh <- Shepard(dist, iso.data$points) #shepard plot 데이터 
  cdist_sh=cbind(dist_sh$x, dist_sh$y, dist_sh$yf)
  colnames(cdist_sh)=c("원거리", "FitDist", "FitDATA")
  
  
  separd_plot_data = cdist_sh[,c(1,3)] 
  image_plot_data = cdist_sh[,c(1,2)] 
  
  
  # library(gridExtra)
  
  if(show=="manual"){
    cmd_plot = cmdplot %>% data.frame() %>% mutate(app= var) %>% 
      ggplot(aes(x=x, y=y))+
      geom_point(aes(color=app), size= 5, show.legend = FALSE)+
      geom_text_repel(aes(label=app),size=5,
                      box.padding = 1)+
      ylim(yl, yh)+xlim(xl, xh)+
      theme_bw()+
      geom_hline(yintercept=0, linetype="dashed",
                 linewidth=0.8, col="gray40")+
      geom_vline(xintercept=0, linetype="dashed", 
                 linewidth=0.8, col="gray40")+
      labs(x= paste0("(a) metric MDS [GOF:",round(cmdgof*100,2),"%]"))+
      theme(axis.title.x= element_text(size=15))
    
    iso_plot = isoplot %>% data.frame() %>% mutate(app= var) %>% 
      ggplot(aes(x=x, y=y))+
      geom_point(aes(color=app), size= 5, show.legend = FALSE)+
      geom_text_repel(aes(label=app),size=5,
                      box.padding = 1)+
      ylim(yl, yh)+xlim(xl, xh)+
      theme_bw()+
      geom_hline(yintercept=0, linetype="dashed", 
                 linewidth=0.8, col="gray40")+
      geom_vline(xintercept=0, linetype="dashed", 
                 linewidth=0.8, col="gray40")+
      labs(x=paste0( "(b) Non-metric MDS [stress:", 
                     round(isostress,3),"]"))+
      theme(axis.title.x= element_text(size=15) )
    
  }else if(show=="auto"){
    
    
    cmd_plot = cmdplot %>% data.frame() %>% mutate(app= var) %>% 
      ggplot(aes(x=x, y=y))+
      geom_point(aes(color=app), size= 5, show.legend = FALSE)+
      geom_text_repel(aes(label=app),size=5,
                      box.padding = 1)+
      # ylim(-max(abs(cmdplot$y))-1, max(abs(cmdplot$y))+1)+
      # xlim(-max(abs(cmdplot$x))-1, max(abs(cmdplot$x))+1)+
      theme_bw()+
      geom_hline(yintercept=0, linetype="dashed",
                 linewidth=0.8, col="gray40")+
      geom_vline(xintercept=0, linetype="dashed", 
                 linewidth=0.8, col="gray40")+
      labs(x= paste0("(a) metric MDS [GOF:",round(cmdgof*100,2),"%]"))+
      theme(axis.title.x= element_text(size=15))
    
    
    
    iso_plot = isoplot %>% data.frame() %>% mutate(app = var) %>% 
      ggplot(aes(x=x, y=y))+
      geom_point(aes(color=app), size= 5, show.legend = FALSE)+
      geom_text_repel(aes(label=app),size=5,
                      box.padding = 1)+
      # ylim(-max(abs(isoplot$y))-1, max(abs(isoplot$y))+1)+
      # xlim(-max(abs(isoplot$x))-1, max(abs(isoplot$x))+1)+
      theme_bw()+
      geom_hline(yintercept=0, linetype="dashed", 
                 linewidth=0.8, col="gray40")+
      geom_vline(xintercept=0, linetype="dashed", 
                 linewidth=0.8, col="gray40")+
      labs(x=paste0( "(b) Non-metric MDS [stress:", 
                     round(isostress,3),"]"))+
      theme(axis.title.x= element_text(size=15) )
  }else if(show=="line"){
    
    cmd_plot = cmdplot %>% data.frame() %>% mutate(app= var) %>% 
      ggplot(aes(x=x, y=y))+
      geom_path(linewidth=linewidth, 
                color=linecolor, linetype= linetype)+  # added path 
      geom_point(aes(color=app), size= 5, show.legend = FALSE)+
      geom_text_repel(aes(label=app),size=5,
                      box.padding = 1)+
      ylim(yl, yh)+xlim(xl, xh)+
      theme_bw()+
      geom_hline(yintercept=0, linetype="dashed",
                 linewidth=0.8, col="gray40")+
      geom_vline(xintercept=0, linetype="dashed", 
                 linewidth=0.8, col="gray40")+
      labs(x= paste0("(a) metric MDS [GOF:",round(cmdgof*100,2),"%]"))+
      theme(axis.title.x= element_text(size=15))
    
    
    iso_plot = isoplot %>% data.frame() %>% mutate(app= var) %>% 
      ggplot(aes(x=x, y=y))+
      geom_path(linewidth= linewidth, 
                color=linecolor, linetype= linetype) +
      geom_point(aes(color=app), size= 5, show.legend = FALSE)+
      geom_text_repel(aes(label=app),size=5,
                      box.padding = 1)+
      ylim(yl, yh)+xlim(xl, xh)+
      theme_bw()+
      geom_hline(yintercept=0, linetype="dashed", 
                 linewidth=0.8, col="gray40")+
      geom_vline(xintercept=0, linetype="dashed", 
                 linewidth=0.8, col="gray40")+
      labs(x=paste0( "(b) Non-metric MDS [stress:", 
                     round(isostress,3),"]"))+
      theme(axis.title.x= element_text(size=15) )  # added path 
  }
  
  
  
  # if(show=="graph"){
  grid.arrange( cmd_plot, 
                iso_plot,
                ncol=2, 
                nrow=1,
                bottom= paste0(grid_title,
                               " ( Non-metric stress=",
                               round(isostress,3),"->",iso_sig,")") )
  
}



# gtl("2020년 전체 인식")  

# ma_new(8- mds_onl[1:20,], grid_title = "2020 full group awareness ")

# gtl2("연도별 다차원분석 결과[ metric MDS와 Non-metric MDS]로 분석하였다.  [그림 ]과 같이 2020년의 참가자의 인식을 나타내었다.")
# mds_onl_2020
# mds_onl_i

# mds_onl_i %>% slice(1:64) %>% 
#   mds_makePlot(grid_title = gtl("실험참가자 전체의 인식"))
# 



stress <- function(data){
  data = cut(data, breaks = c(0, 0.0024, 0.049, 0.099, 0.19,Inf),
             label=c("perfect","excellent","good","fair","poor" )) 
  data
}

# stress(0.0025)
# stress(0.002)

stress_mutate <- function(data){
  data = data %>% mutate(stress= cut(data[,ncol(data)],
                                     breaks = c(0, 0.0024, 0.049, 0.099, 0.19,Inf),
                                     label=c("perfect","excellent","good","fair","poor" )) )
  data
}

# load(file ="mds_list.RData")
# mds_list



# # unload tidyverse and ggplot2 as an example first
# detach("package:tidyverse", unload = TRUE)
# detach("package:ggplot2", unload = TRUE)
# magic option
# options(tidyverse.quiet = TRUE)
# library(tidyverse)



mds_shepard <- function(mds_data,
                        grid_title="",
                        variable= c("ZOOM","Webex","googleMEET",
                                    "Youtube","naveBand","prism")
                        
){
  library(psych)
  library(lavaan)
  library(tidyverse)
  library(MASS)
  library(gridExtra)
  library(ggrepel)
  
  if(is.character(variable ==FALSE)){
    stopifnot("input again")
  }
  #
  
  var= variable
  
  # mean matrix generator  ==mdsDataMaker()
  mds_data<-mds_data %>% as.data.frame()
  mds0 =rep(0, nrow(mds_data)) %>% as_tibble()
  
  dms_raw <-cbind(mds0,mds_data[,1:5],
                  mds0,mds_data[,6:9],
                  mds0,mds_data[,10:12],
                  mds0,mds_data[,13:14],
                  mds0,mds_data[,15],mds0)
  
  des.data <- psych::describe(dms_raw) %>% as.data.frame()
  
  result.matrix <- des.data[,3] %>% lav_matrix_upper2full()
  
  
  
  if(length(variable) != 0){
    colnames(result.matrix)=variable
    rownames(result.matrix)=variable}
  #result.matrix
  
  #scale- dist - mds 
  cmd.data <- result.matrix %>%
    scale() %>% 
    dist() %>% 
    cmdscale(k=2, eig = T)
  
  cmdplot <- cmd.data$points
  cmdgof <- cmd.data$GOF[1]
  
  iso.data <- result.matrix %>%
    scale() %>% 
    dist() %>%
    isoMDS(k=2)
  isoplot <- iso.data$points
  isostress <- iso.data$stress
  iso_sig = cut(isostress, breaks = c(0, 0.0025, 0.05, 0.1, 0.2,Inf),
                label=c("perfect","excellent","good","fair","poor" )) 
  
  colName= c("x","y")
  colnames(cmdplot)= colName
  colnames(isoplot)= colName
  
  
  
  
  #shepard plot data 
  mat <- result.matrix %>% scale() #표준화  
  dist <- mat %>% dist() #거리함수
  #shepard plot data  Shepard( dist matrix, isoMDS plot data )
  dist_sh <- Shepard(dist, iso.data$points) #shepard plot 데이터 
  
  cdist_sh = cbind(dist_sh$x, dist_sh$y, dist_sh$yf)
  colnames(cdist_sh)=c("Dissimilarity_dist", "FitDist", "FitData")
  
  
  separd_plot_data = cdist_sh[,c(1,3)]  #"Dissimilarity_dist, FitData
  image_plot_data  = cdist_sh[,c(2,3)]  #FitDist, FitData
  
  
  
  # result data 
  res=list(
    sephard = separd_plot_data, 
    image_residuals = image_plot_data,
    # cdist_sh,
    result.matrix
    # mat,
    # dist
    # dms_raw
  )
  
  
  
  
  # aaa$sephard
  
  grid.arrange(  
    separd_plot_data %>%data.frame() %>%  
      ggplot(aes(x=Dissimilarity_dist, y=FitData))+
      geom_point(size=3)+
      geom_step(linewidth=0.8, linetype ="dashed")+theme_bw()+
      geom_smooth(method=lm, se=T, fill="gray80")
    # geom_abline(slope=1, intercept = 0, )
    # xlim(xl, xh)
    ,
    
    image_plot_data%>%data.frame() %>%  
      ggplot(aes(x=FitDist, y=FitData))+
      geom_point(size=3, col="red")+
      geom_line(linewidth=1, linetype ="dashed")+theme_bw()+
      geom_smooth(method=lm, se=T, fill="gray70")
    # geom_abline(slope=1, intercept = 0)
    ,
    ncol=2, bottom="sephard diagram and image plot(residuals)"
  )
}

#path 
# https://ggplot2.tidyverse.org/reference/geom_path.html

# mds_onl_2020 %>% mds_shepard(grid_title = gtl("2020년 전체그룹의 인식"))
# 

# A function that automatically creates a distance matrix and performs MDS analysis.
mds_tatal_plot  <- function(mds_data,
                            grid_title="",
                            xl = -3,
                            xh =  3,
                            yl = -2,
                            yh =  2,
                            variable= c("ZOOM","Webex","googleMEET",
                                        "Youtube","NaveBand","Prism"),
                            show="manual",
                            linewidth=0.9,
                            linecolor="gray40",
                            linetype=1,
                            size.x = 14,     #xlab size
                            size_point=4,     #MDS point size 
                            size_text=5,    #MDS text
                            color_regline="blue",   #shepard and image
                            color_point="gray10",   #shepard
                            color_point2="red"     #image
                            
){
  library(psych)
  library(lavaan)
  library(tidyverse)
  library(MASS)
  library(gridExtra)
  library(ggrepel)
  
  if(is.character(variable ==FALSE)){
    stopifnot("input again")
  }
  #
  
  var= variable
  
  # generate matrix
  mds_data<-mds_data %>% as.data.frame()
  mds0 =rep(0, nrow(mds_data)) %>% as_tibble()
  
  dms_raw <-cbind(mds0,mds_data[,1:5],
                  mds0,mds_data[,6:9],
                  mds0,mds_data[,10:12],
                  mds0,mds_data[,13:14],
                  mds0,mds_data[,15],mds0)
  
  des.data <- psych::describe(dms_raw) %>% as.data.frame()
  
  result.matrix <- des.data[,3] %>% lav_matrix_upper2full()
  if(length(variable) != 0){
    colnames(result.matrix)=variable
    rownames(result.matrix)=variable}
  #result.matrix
  
  #scale- dist - mds 
  cmd.data <- result.matrix %>%
    scale() %>% 
    dist() %>% 
    cmdscale(k=2, eig = T)
  
  cmdplot <- cmd.data$points
  cmdgof <- cmd.data$GOF[1]
  
  iso.data <- result.matrix %>%
    scale() %>% 
    dist() %>%
    isoMDS(k=2)
  
  isoplot <- iso.data$points
  isostress <- iso.data$stress
  iso_sig = cut(isostress, breaks = c(0, 0.0025, 0.05, 0.1, 0.2,Inf),
                label=c("perfect","excellent","good","fair","poor" )) 
  
  colName= c("x","y")
  colnames(cmdplot)= colName
  colnames(isoplot)= colName
  
  
  
  
  #shepard plot data 
  mat <- result.matrix %>% scale() #표준화  
  dist <- mat %>% dist() #거리함수
  #shepard plot data  Shepard( dist matrix, isoMDS plot data )
  dist_sh <- Shepard(dist, iso.data$points) #shepard plot 데이터 
  
  cdist_sh = cbind(dist_sh$x, dist_sh$y, dist_sh$yf)
  colnames(cdist_sh)=c("Dissimilarity_dist", "FitDist", "FitData")
  
  
  separd_plot_data = cdist_sh[,c(1,3)]  #"Dissimilarity_dist, FitData
  image_plot_data  = cdist_sh[,c(2,3)]  #FitDist, FitData
  
  
  
  
  # library(gridExtra)
  
  if(show=="manual"){
    cmd_plot = cmdplot %>% data.frame() %>% mutate(app= var) %>% 
      ggplot(aes(x=x, y=y))+
      geom_hline(yintercept=0, linetype="dashed",
                 linewidth=0.7, col="gray50")+
      geom_vline(xintercept=0, linetype="dashed", 
                 linewidth=0.7, col="gray50")+
      geom_point(aes(color=app, 
                     # shape=app
      ), 
      size= size_point, show.legend = FALSE)+
      geom_text_repel(aes(label=app),size= size_text,
                      box.padding = 1)+
      ylim(yl, yh)+xlim(xl, xh)+
      theme_bw()+
      labs(x= paste0("(a) metric MDS [GOF:",round(cmdgof*100,2),"%]"))+
      theme(axis.title.x= element_text(size= size.x))
    
    
    
    
    
    iso_plot = isoplot %>% data.frame() %>% mutate(app= var) %>% 
      ggplot(aes(x=x, y=y))+
      geom_hline(yintercept=0, linetype="dashed", 
                 linewidth=0.7, col="gray50")+
      geom_vline(xintercept=0, linetype="dashed", 
                 linewidth=0.7, col="gray50")+
      geom_point(aes(color= app, 
                     # shape=app
      ), 
      size= size_point, show.legend = FALSE)+
      geom_text_repel(aes(label=app),size= size_text,
                      box.padding = 1)+
      ylim(yl, yh)+xlim(xl, xh)+
      theme_bw()+
      labs(x=paste0( "(b) Non-metric MDS [stress:", 
                     round(isostress,3),"]"))+
      theme(axis.title.x= element_text(size= size.x))
    
    
    
    separd_diagram =  separd_plot_data %>%data.frame() %>%  
      ggplot(aes(x=Dissimilarity_dist, y=FitData))+
      geom_step(linewidth=0.8, #linetype ="dashed"
                color= "gray20")+
      theme_bw()+
      geom_smooth(method=lm, se=T, fill="gray80", 
                  color=color_regline, linetype=linetype)+
      geom_point(size=3, color= color_point )+
      labs(x=paste0("(c) Shepard Diagram"))+
      theme(axis.title.x= element_text(size= size.x))
    # geom_abline(slope=1, intercept = 0, )
    # xlim(xl, xh)
    
    
    image_plot = image_plot_data%>%data.frame() %>%  
      ggplot(aes(x=FitDist, y=FitData))+
      geom_point(size=3, col=color_point2)+
      # geom_line(linewidth=1, linetype ="dashed")+
      theme_bw()+
      geom_smooth(method=lm, se=T, fill="gray80",
                  color=color_regline, linetype= linetype)+
      
      labs(x=paste0("(d) Image Plot (residuals)"))+
      theme(axis.title.x= element_text(size= size.x))
    
    
    
  }else if(show=="auto"){
    
    
    cmd_plot = cmdplot %>% data.frame() %>% mutate(app= var) %>% 
      ggplot(aes(x=x, y=y))+
      geom_hline(yintercept=0, linetype="dashed",
                 linewidth=0.8, col="gray40")+
      geom_vline(xintercept=0, linetype="dashed", 
                 linewidth=0.8, col="gray40")+
      geom_point(aes(color=app), size= 5, show.legend = FALSE)+
      geom_text_repel(aes(label=app),size=size_text,
                      box.padding = 1)+
      # ylim(-max(abs(cmdplot$y))-1, max(abs(cmdplot$y))+1)+
      # xlim(-max(abs(cmdplot$x))-1, max(abs(cmdplot$x))+1)+
      theme_bw()+
      
      labs(x= paste0("(a) metric MDS [GOF:",round(cmdgof*100,2),"%]"))+
      theme(axis.title.x= element_text(size= size.x))
    
    
    
    iso_plot = isoplot %>% data.frame() %>% mutate(app= var) %>% 
      ggplot(aes(x=x, y=y))+
      geom_hline(yintercept=0, linetype="dashed", 
                 linewidth=0.8, col="gray40")+
      geom_vline(xintercept=0, linetype="dashed", 
                 linewidth=0.8, col="gray40")+
      geom_point(aes(color=app), size= 5, show.legend = FALSE)+
      geom_text_repel(aes(label=app),size=size_text,
                      box.padding = 1)+
      # ylim(-max(abs(isoplot$y))-1, max(abs(isoplot$y))+1)+
      # xlim(-max(abs(isoplot$x))-1, max(abs(isoplot$x))+1)+
      theme_bw()+
      
      labs(x=paste0( "(b) Non-metric MDS [stress:", 
                     round(isostress,3),"]"))+
      theme(axis.title.x= element_text(size= size.x))
    
    
    
    separd_diagram =  separd_plot_data %>%data.frame() %>%  
      ggplot(aes(x=Dissimilarity_dist, y=FitData))+
      geom_step(linewidth=0.8, #linetype ="dashed"
                color= "gray20")+
      theme_bw()+
      geom_smooth(method=lm, se=T, fill="gray80", 
                  color=color_regline, linetype=linetype)+
      geom_point(size=3, color= color_point )+
      labs(x=paste0("(c) Shepard Diagram"))+
      theme(axis.title.x= element_text(size= size.x))
    # geom_abline(slope=1, intercept = 0, )
    # xlim(xl, xh)
    
    
    image_plot = image_plot_data%>%data.frame() %>%  
      ggplot(aes(x=FitDist, y=FitData))+
      geom_point(size=3, col=color_point2)+
      # geom_line(linewidth=1, linetype ="dashed")+
      theme_bw()+
      geom_smooth(method=lm, se=T, fill="gray80",
                  color=color_regline, linetype= linetype)+
      
      labs(x=paste0("(d) Image Plot (residuals)"))+
      theme(axis.title.x= element_text(size= size.x))
    
    
    
  }else if(show=="line"){
    
    cmd_plot = cmdplot %>% data.frame() %>% mutate(app= var) %>% 
      ggplot(aes(x=x, y=y))+
      geom_hline(yintercept=0, linetype="dashed",
                 linewidth=0.8, col="gray40")+
      geom_vline(xintercept=0, linetype="dashed", 
                 linewidth=0.8, col="gray40")+
      geom_path(linewidth=linewidth, 
                color=linecolor, linetype= linetype)+  # added path 
      geom_point(aes(color=app), size= 5, show.legend = FALSE)+
      geom_text_repel(aes(label=app),size=size_text,
                      box.padding = 1)+
      ylim(yl, yh)+xlim(xl, xh)+
      theme_bw()+
      
      labs(x= paste0("(a) metric MDS [GOF:",round(cmdgof*100,2),"%]"))+
      theme(axis.title.x= element_text(size= size.x))
    
    
    iso_plot = isoplot %>% data.frame() %>% mutate(app= var) %>% 
      ggplot(aes(x=x, y=y))+
      geom_hline(yintercept=0, linetype="dashed", 
                 linewidth=0.8, col="gray40")+
      geom_vline(xintercept=0, linetype="dashed", 
                 linewidth=0.8, col="gray40")+
      geom_path(linewidth= linewidth, 
                color=linecolor, linetype= linetype) +
      geom_point(aes(color=app), size= 5, show.legend = FALSE)+
      geom_text_repel(aes(label=app),size=size_text,
                      box.padding = 1)+
      ylim(yl, yh)+xlim(xl, xh)+
      theme_bw()+
      labs(x=paste0( "(b) Non-metric MDS [stress:", 
                     round(isostress,3),"]"))+
      theme(axis.title.x= element_text(size= size.x ) ) # added path 
    
    
    
    separd_diagram =  separd_plot_data %>%data.frame() %>%  
      ggplot(aes(x=Dissimilarity_dist, y=FitData))+
      geom_step(linewidth=0.8, #linetype ="dashed"
                color= "gray20")+
      theme_bw()+
      geom_smooth(method=lm, se=T, fill="gray80", 
                  color=color_regline, linetype=linetype)+
      geom_point(size=3, color= color_point )+
      labs(x=paste0("(c) Shepard Diagram"))+
      theme(axis.title.x= element_text(size= size.x))
    # geom_abline(slope=1, intercept = 0, )
    # xlim(xl, xh)
    
    
    image_plot = image_plot_data%>%data.frame() %>%  
      ggplot(aes(x=FitDist, y=FitData))+
      geom_point(size=3, col=color_point2)+
      # geom_line(linewidth=1, linetype ="dashed")+
      theme_bw()+
      geom_smooth(method=lm, se=T, fill="gray80",
                  color=color_regline, linetype= linetype)+
      
      labs(x=paste0("(d) Image Plot (residuals)"))+
      theme(axis.title.x= element_text(size= size.x))
    
  } #내부의 마무리 
  
  
  # if(show=="graph"){
  grid.arrange( cmd_plot, 
                iso_plot,
                separd_diagram,
                image_plot,
                ncol=2, 
                nrow=2,
                bottom= paste0(grid_title,
                               " ( Kruskal(1694) stress index = ",
                               round(isostress,3)," -> ",iso_sig," )") )
  
}
# end function 




#한개의 이미지와 OPSS, RMSD 분석 
shape_plot <- function(OPA_data,
                       title="",
                       line=c(1:6,1),...) {
  
  
  
  if(is.character(title)==FALSE){
    stopifnot("input title ")
  }
  library(shape)
  
  
  OPA_data_d <- OPA_data
  OPA_data1  <- OPA_data_d$Bhat
  OPA_OSS    <- OPA_data_d$OSS
  OPA_rmsd   <- OPA_data_d$rmsd
  
  
  
  #Nonmetric shape coordinated
  plotshapes( OPA_data1, joinline=c(line,...),color="red",symbol = 1)#mtericOPA 형상 
  abline(v=0,h=0)
  points(OPA_data1,pch=21,bg=c(line,...),cex=1.5)
  grid()
  text(OPA_data1+c(-.2, .1),rownames(OPA_data1),cex=0.9, col = c(rep("blue",3), rep("red",3)))
  title(paste("shape analysis::", title),paste("OPSS =", round(OPA_OSS),2),paste("RMSD = ",round(OPA_rmsd,2)))
  tab<-cbind(OPA_OSS,OPA_rmsd)
  res<-list(OPA_data1,OPSS_RMSD=tab)
  res
}
#
###shape_plot(pnm)

#ProcOPA#####
#Ordinary Procustes analysis : the matching of one configuration to another using translation, rotation and (possibly) scale. Reflections can also be included if desired. The function matches configuration B onto A by least squares.
#두형상 적합도 분석 
#RMSD(roor mean square deviation): procrustes distance를 최소화하는 값으로 0에 가까울 수록 두형상은 같은 경향을 보인다. 
#Cox & Cox(2001)




OPA_Plot <- function(metricD,
                     nonmetricD,title="",
                     row_name,line=c(1:6,1),...) {
  if(is.character(title)==FALSE){
    stopifnot("input title ")
  }
  
  library(shape)
  #metric(뒤) onto nonmetirc(앞)
  pnm<- procOPA(nonmetricD, metricD)
  #nonmetric on metirc
  pmn<- procOPA(metricD, nonmetricD)
  
  
  OPA_data_nm <- pnm
  OPA_data1  <- OPA_data_nm$Bhat
  OPA_OSS_nm    <- OPA_data_nm$OSS
  OPA_rmsd_nm   <- OPA_data_nm$rmsd
  
  OPA_data_mn <- pmn
  OPA_data2  <- OPA_data_mn$Bhat
  OPA_OSS_mn   <- OPA_data_mn$OSS
  OPA_rmsd_mn  <- OPA_data_mn$rmsd
  
  rownames(OPA_data1 )= row_name
  rownames(OPA_data2 )= row_name
  
  
  #metric shape coordinated
  par(mfrow = c(1,2))
  plotshapes( OPA_data1, joinline=c(line,...),color="red",symbol = 1)#mtericOPA 형상 
  abline(v=0,h=0)
  points(OPA_data1,pch=21,bg=c(line,...),cex=1.5)
  grid()
  text(OPA_data1+c(-.2, .1),rownames(OPA_data1),cex=0.9, col = c(rep("blue",3), rep("red",3)))
  title(paste("shape analysis::", title),paste("OPSS =", round(OPA_OSS_nm),2),
        paste("RMSD = ",round(OPA_rmsd_nm,2)))
  
  
  #Non-metric shape coordinated
  plotshapes( OPA_data2, joinline=c(line,...),color="red",symbol = 1)#mtericOPA 형상 
  abline(v=0,h=0)
  points(OPA_data2,pch=21,bg=c(line,...),cex=1.5)
  grid()
  text(OPA_data2+c(-.2, .1),rownames(OPA_data2),cex=0.9, col = c(rep("blue",3), rep("red",3)))
  title(paste("shape analysis::", title),paste("OPSS =", round(OPA_OSS_mn),2),
        paste("RMSD = ",round(OPA_rmsd_mn,2)))
  
  
  
  
  
  par(mfrow=c(1,1))
  
  tabnm <-cbind(OPA_OSS_nm,OPA_rmsd_nm)
  tabmn <-cbind(OPA_OSS_mn,OPA_rmsd_mn)
  tablefull<-rbind(tabnm, tabmn)
  rownames(tablefull)=c("n -> m", "m -> n")
  colnames(tablefull)=c("OPSS","RMSD")
  
  res<-list(PNM=OPA_data1,PMN=OPA_data2,OPSS_RMSD=tablefull)
  res
}




#형상분석 OPA적합 알고리즘에 의한 적합 


OPA_Plot_data <- function(metricD=NULL,
                          nonmetricD=NULL,
                          title="",
                          first="", 
                          second="") {
  
  library(shapes)
  #metric(뒤) onto nonmetirc(앞)
  pnm <- procOPA(nonmetricD, metricD)
  #nonmetric on metirc
  pmn <- procOPA(metricD, nonmetricD)
  
  
  Title_explain  <- paste(" \n\n Procustes  analysis \n\n (1)",title,"\n\n",
                          "Oridinary Procrustes  sum of squares: OPSS is Procrutes statistic \n",
                          "Root mean Square Deviation: RMSD \n (2)",
                          first,"->",second, "\n (3)",
                          second,"->", first ,"\n\n",
                          "RMSD는 OPA 적합에서 두 개체간의 형상변동을 측정하는 측도이다. 이 값이 0에 가까울 수록 두 형상간의 OPA적합을 한 후에 형상변동이 작아 두 형상은 같은 경향을 보인다고 할 수 있다. 두 적합의 RMSD의 값이 비슷하더라도 값이 크면 두 개의 형상변동이 크고, 두개의 형상이 매우 다름을 보여주는 것이다. 만만약 어느 쪽으로 OPA적합하던지 RMSD의 값이 비슷하면 낮다면 두개의 그림이 다르더라도 형상변동이 작다는 것을 알 수 있다.
 OPSS는 프로크로스티즈 통계량으로 알려져있으며, 이 값을 이용하여 두 형상을 비교한다. 이값이 작을 수록 형상 변동(variability)가 적게 나타난다. \n
                          "
  )
  
  OPA_data_nm <- pnm
  OPA_data1  <- OPA_data_nm$Bhat
  colnames(OPA_data1)=c("x","y")
  OPA_OSS_nm    <- OPA_data_nm$OSS
  OPA_rmsd_nm   <- OPA_data_nm$rmsd
  
  OPA_data_mn <- pmn
  OPA_data2  <- OPA_data_mn$Bhat
  colnames(OPA_data2)=c("x","y")
  OPA_OSS_mn   <- OPA_data_mn$OSS
  OPA_rmsd_mn  <- OPA_data_mn$rmsd
  
  
  
  cat(Title_explain)
  
  res=  list(A_B = OPA_data1,
             A_B_OPSS = OPA_OSS_nm,
             A_B_RMSD = OPA_rmsd_nm,
             B_A = OPA_data2,
             B_A_OPSS = OPA_OSS_mn,
             B_A_RMSD = OPA_rmsd_mn)
  res
  
}
# 
# OPA_Plot_data(mds_onl_yonsei %>% ma_data("isoplot_data"),
#               mds_onl_korea %>% ma_data("isoplot_data") , 
#               title=gtl("그룹비교 연세대, 고려대"),
#               first="yonsei Uni", second="korea Uni")


#형상 분석 그림 비교용 ---------
OPA_Plot_new <- function(metricD=NULL,
                         nonmetricD=NULL,
                         title="",
                         first="",
                         second="",
                         show="auto",
                         linewidth=0.8,
                         linetype=1,
                         linecolor="gray40",
                         text_size= 5,
                         xl=-3, xh=3, yl=-2.3, yh=2,
                         size.x = 14
                         
) {
  if(is.character(title)==FALSE){
    stopifnot("input title ")
  }
  
  
  library(shape)
  #metric(뒤) onto nonmetirc(앞)
  pnm <- procOPA(nonmetricD, metricD)
  #nonmetric on metirc
  pmn <- procOPA(metricD, nonmetricD)
  
  
  
  
  OPA_data_nm <- pnm
  OPA_data1  <- OPA_data_nm$Bhat
  colnames(OPA_data1)=c("x","y")
  OPA_OSS_nm    <- OPA_data_nm$OSS
  OPA_rmsd_nm   <- OPA_data_nm$rmsd
  
  OPA_data_mn <- pmn
  OPA_data2  <- OPA_data_mn$Bhat
  colnames(OPA_data2)=c("x","y")
  OPA_OSS_mn   <- OPA_data_mn$OSS
  OPA_rmsd_mn  <- OPA_data_mn$rmsd
  
  
  
  
  
  var=c("ZOOM","Webex","googleMEET","Youtube","NaverBand","Prism")
  rownames(OPA_data1 )= var
  rownames(OPA_data2 )= var
  
  library(gridExtra)
  
  if(show=="manual"){
    grid.arrange(
      
      OPA_data1 %>% data.frame() %>% mutate(app= var) %>% 
        ggplot(aes(x=x, y=y))+
        geom_path(linewidth= linewidth, 
                  color=linecolor, 
                  linetype= linetype) +
        geom_point(aes(color=app), 
                   size= 5, 
                   show.legend = FALSE)+
        geom_text_repel(aes(label=app),
                        size=text_size,
                        box.padding = 1)+
        ylim(yl, yh)+ xlim(xl, xh)+
        theme_bw()+
        geom_hline(yintercept=0, linetype="dashed", 
                   linewidth=0.8, col="gray40")+
        geom_vline(xintercept=0, linetype="dashed", 
                   linewidth=0.8, col="gray40")+
        labs(x=paste0("RMSD: ", round(OPA_rmsd_nm,3),
                      " \n OPSS: ",round(OPA_OSS_nm,3),"\n",first))+
        theme(axis.title.x= element_text(size=size.x) )  # added path 
      ,
      
      OPA_data2 %>% data.frame() %>% mutate(app= var) %>% 
        ggplot(aes(x=x, y=y))+
        geom_path(linewidth= linewidth, 
                  color=linecolor, 
                  linetype= linetype) +
        geom_point(aes(color=app), 
                   size= 5, 
                   show.legend = FALSE)+
        geom_text_repel(aes(label=app),
                        size=text_size,
                        box.padding = 1)+
        ylim(yl, yh)+ xlim(xl, xh)+
        theme_bw()+
        geom_hline(yintercept=0, linetype="dashed", 
                   linewidth=0.8, col="gray40")+
        geom_vline(xintercept=0, linetype="dashed", 
                   linewidth=0.8, col="gray40")+
        labs(x=paste0("RMSD: ", round(OPA_rmsd_mn,3),
                      " \n OPSS: ",round(OPA_OSS_mn,3),"\n",second))+
        theme(axis.title.x= element_text(size=size.x) )  # added path 
      , ncol=2,
      bottom=title  ) 
  }else if(show=="auto"){
    grid.arrange(
      
      OPA_data1 %>% data.frame() %>% mutate(app= var) %>% 
        ggplot(aes(x=x, y=y))+
        geom_path(linewidth= linewidth, 
                  color=linecolor, 
                  linetype= linetype) +
        geom_point(aes(color=app), 
                   size= 5, 
                   show.legend = FALSE)+
        geom_text_repel(aes(label=app),
                        size=text_size,
                        box.padding = 1)+
        # ylim(yl, yh)+ xlim(xl, xh)+
        theme_bw()+
        geom_hline(yintercept=0, linetype="dashed", 
                   linewidth=0.8, col="gray40")+
        geom_vline(xintercept=0, linetype="dashed", 
                   linewidth=0.8, col="gray40")+
        labs(x=paste0("RMSD: ", round(OPA_rmsd_nm,3),
                      " \n OPSS: ",round(OPA_OSS_nm,3),"\n",first))+
        theme(axis.title.x= element_text(size=size.x) )  # added path 
      ,
      
      OPA_data2 %>% data.frame() %>% mutate(app= var) %>% 
        ggplot(aes(x=x, y=y))+
        geom_path(linewidth= linewidth, 
                  color=linecolor, 
                  linetype= linetype) +
        geom_point(aes(color=app), 
                   size= 5, 
                   show.legend = FALSE)+
        geom_text_repel(aes(label=app),
                        size=text_size,
                        box.padding = 1)+
        # ylim(yl, yh)+ xlim(xl, xh)+
        theme_bw()+
        geom_hline(yintercept=0, linetype="dashed", 
                   linewidth=0.8, col="gray40")+
        geom_vline(xintercept=0, linetype="dashed", 
                   linewidth=0.8, col="gray40")+
        labs(x=paste0("RMSD: ", round(OPA_rmsd_mn,3),
                      " \n OPSS: ",round(OPA_OSS_mn,3),"\n",second))+
        theme(axis.title.x= element_text(size=size.x) )  # added path 
      , ncol=2,
      bottom=title)
    
  }
  
}

#shape analysis OPA 
# procursetes analysis
# OPA_Plot_new(mds_onl_i[-64,] %>% ma_data("cmdplot_data"), 
#              mds_onl_i[-64,] %>% ma_data("isoplot_data"),
#              first = "(a) Metric  --> Non-Metric  ", 
#              second="(b) Non-Metric MSD",
#              title="Entire Group")

#P MDS plot 연도별 MDS 논문에 넣기 ---------------------
# 
# 
# 
# mds_onl_i[-64,] %>% mds_tatal_plot(grid_title = gtl("전체그룹의 인식"),
#                                    size_text = 5)

#text mining####

#text를 띄어쓰기로 분리하기-------------
word_split0<- function(x){
  library(stringr)
  wordsplit <-unlist(str_extract_all(x, boundary("word")))
  return(wordsplit)
}



#자연어 분석 1- 명사추출+ggplot, wordcloud
word_split<- function(x){
  library(stringr)
  library(KoNLP)
  library(tidyverse)
  library(ggplot2)
  library(knitr)
  library(wordcloud2)
  x <-str_remove_all(x,"\\,")
  xx <- extractNoun(x)
  x1 <- xx %>% unlist() %>% table() %>%  as.data.frame(stringsAsFactors=F)
  #x1 %>% rename(word=".",n=Freq)
  colnames(x1)=c("word","n")  #뱐수명
  x2<-filter(x1,nchar(word)>=2)  #두글자 이상
  x2<-x2 %>% arrange(desc(n))  #빈도수별로 정렬
  #table.x2 <-x2 %>% kable("pandoc",caption = "명사별 빈도수")

  x3 <- x2 %>% mutate(word = fct_reorder(word,n,"mean")) #순서 정렬


  a1 <-ggplot(data= x3[1:20,], aes(x=word, y=n),fill=word)+geom_bar(stat="identity")+coord_flip()+
    labs(y="빈도수", x= "주요 키워드 ")

  a2<-wordcloud2(x2,size = 2,
                 color = "random-light",backgroundColor = "black", shape = 'circle')


  result <-list(Noun_extract=xx, Noun_count= x2,Bar_Plot=a1,WordCloud=a2)
  result
}


#자연어 분석 2- 형태소로 추출 ggplot, wordcloud
word_split2<- function(x){
  library(KoNLP)
  #useNIADic()
  #useSejongDic()   #사전 설치
  library(tidyverse)           #install.packages("dplyr")
  library(stringr)           #install.packages("stringr")
  library(wordcloud2)        #install.packages("wordcloud22")
  library(reshape2)          #install.packages("reshape2")


  xxx <-x
  txt_df <-  xxx%>% SimplePos09 %>%melt %>%as_tibble
  txt_s_df<-txt_df %>%  dplyr::select(3,1)


  txt_count <-txt_s_df %>%
    mutate(noun=str_match(value, '([가-힣]+)/N')[,2]) %>%
    na.omit %>%
    filter(str_length(noun)>=2) %>%
    count(noun, sort=TRUE)

  txt_count <- txt_count %>% mutate(noun = fct_reorder(noun,n,"mean")) #순서 정렬
  table<-ggplot(data= txt_count[1:20,], aes(x=noun, y=n),fill=noun)+geom_bar(stat="identity")+coord_flip()+
    labs(y="빈도수", x="주요 키워드 ")
  wdc<-wordcloud2(txt_count,size = 2,
                  color = "random-light",backgroundColor = "black", shape = 'circle')

  list(part_of_Speech= txt_df,Count_data=txt_count,BarPlot=table,WodrCloud=wdc)
}


#형태소로 분석3 데이터만
word_split3<- function(x){
  library(KoNLP)
  #useNIADic()
  #useSejongDic()   #사전 설치
  library(tidyverse)           #install.packages("dplyr")
  library(stringr)           #install.packages("stringr")
  library(wordcloud2)        #install.packages("wordcloud22")
  library(reshape2)          #install.packages("reshape2")



  xxx <-x
  txt_df <-  xxx%>% SimplePos09 %>%melt %>%as_tibble
  txt_s_df<-txt_df %>%  dplyr::select(3,1)


  txt_count <-txt_s_df %>%
    mutate(noun=str_match(value, '([가-힣]+)/N')[,2]) %>%
    na.omit %>%
    filter(str_length(noun)>=2) %>%
    count(noun, sort=TRUE)

  txt_count <- txt_count %>% mutate(noun = fct_reorder(noun,n,"mean")) #순서 정렬
  #table<-ggplot(data= txt_count[1:20,], aes(x=noun, y=n),fill=noun)+geom_bar(stat="identity")+coord_flip()+
  #  labs(y="빈도수", x="주요 키워드 ")
  #wdc<-wordcloud2(txt_count,size = 2,
  #                color = "random-light",backgroundColor = "black", shape = 'circle')

  result<- list(part_of_Speech= txt_df,Count_data=txt_count)#,BarPlot=table,WodrCloud=wdc)
  result
}



#사전에 단어를 추가 - 분석이 잘 안될 경우 명사를 추가하는 기능
dicAdd<- function(add_Keyword){
  library(KoNLP)
  #useNIADic()
  #useSejongDic()   #사전 설치
  buildDictionary(user_dic = data.frame(add_Keyword,rep("ncn",length(add_Keyword))),replace_usr_dic = T)
}





##텍스트 마이닝 #####
#tokens화 #####

Token_gen <- function(data){
  library(tidyverse)
  library(tidytext)
  data1 <-data %>% tibble(line=1:length(data))%>%
    unnest_tokens(word,text) %>%
    count(word,sort = T) %>% filter(str_length(word)>=2)
  data1
}

##word_kor--token화  한글단어추출 함수 ####
word_kor<- function(data){
  library(KoNLP)
  library(stringr)
  colnames("text")
  #SimplePos09() 형태소 구분
  data %>% mutate(word=str_match(text, '([가-힣]+)')[,2]) %>%
    na.omit %>%
    filter(str_length(word)>=2) %>%  count(word, sort=TRUE)
}

#불용어 제거함수: 데이터와 불용어를 넣어서 처리
#데이터, 컬럼, 제거할 단어
#자체네 tibble처리후에 변수를 reorder
word_remover <- function(data,remove_words){
  library(tidyverse)
  library(tidytext)
  colnames(data)=c("word","n")
  word_remove<-tibble(word=remove_words)
  data <-data %>% anti_join(word_remove)
  data<-data %>% mutate(word=reorder(word,n))
  data
}


#text mining빈도수 그래프 ####
freq_bar<- function(data){
  library(ggplot2)
  library(dplyr)
  colnames(data)=c("word","n")
  
  data1 <- data %>% mutate(word=reorder(word,n)) #순서 정렬
  ggplot(data=data1[1:30,], aes(x=word, y=n))+
    geom_bar(stat="identity") +
    geom_col()+coord_flip()+
    labs(y="빈도수", x="주요 키워드 ")
}



# 조사들이 결합된 단어는 변경하여 정리 
change_word<- function(data,
                       findword="", 
                       change="",
                       col=1){
  
  data[data[,col]== findword, col] <- change
  data <- data %>% as_tibble()
  data
}


#연도선택함수----
yearsel <- function(data, y="2020", 
                    show="keyword", 
                    year="partial",
                    sel = "특성추출_가중치순상위50개"){
  library(tidyverse)
  #모든 기간 나타내기 
  if(year == "all"){
    data2 <- data %>%  
      dplyr::select(일자, year, month, 키워드, 본문, 제목, 
                    특성추출_가중치순상위50개,언론사,`뉴스 식별자`) 
    
    data3 <-data2 %>% dplyr::select(일자, all_of(sel))
    
  }else if(year=='partial'){
    #일부기간 선택
    data2 <- data %>%  dplyr::select(일자, year, month, 키워드, 본문, 
                                     특성추출_가중치순상위50개,언론사) %>% 
      filter(year == y )
    data3 <-data2 %>% dplyr::select(일자,  all_of(sel))
  }
  
  #모든 변수 표시 
  if(show== "all"){
    res = data2
  }else if(show=="keyword"){
    #토큰화할 내용만 표시 
    res = data3
  }
  res
}



##불용어 stopword -----------
# 부석시 관련없는 조사 불용어를 모두 정리 
# my_stopword0 <- tibble(word = c(as.character(1:18),
#                                 as.character(20:30),
#                                 "에서","활용")
# )
# my_stopword <- my_stopword0
# my_stopword <- my_stopword %>% add_row(word=c("대한","있다"), .before = 1)
# my_stopword
# #stopword add function ----
# Topic분석을 위하여 제거해야 하는 불용어 
# 불용어 추가함수 
add_stopword <- function(data, 
                        word= NULL, 
                        add=TRUE, 
                        remove=NULL){
  library(tidyverse)
  
  wordlist= word
  removelist = remove 
  if(length(remove)>1){
    cat("한번에 한개씩 제거하세요,동시에 2개 단어이상은 제거하지 않습니다.")}
  
  if(add==TRUE){
    data <- data%>% 
      add_row(word= word, .before = 1)
  }else if(add==FALSE){
    data <- data %>% filter(word != remove)
  }
  
  data1 <- unique(data)
  res=data1
  res
}

# # 불용어 추가 
# my_stopword <- my_stopword %>% 
#   add_stopword(c("위한","이날","따라","열린","일부터",
#                  "주제로","따라","으로","인해",
#                  "지난", "일까지", "대상으로","올해",
#                  "가운데","기자","이하",
#                  "년생","따르면","등을",
#                  "이번","밝혔다","있는","오는", "통해","최근",
#                  "위해","함께","최근","활용한",
#                  "명을","맞아"
#   ))






#추출 : 같은 단어에 대하여 점검을 하도록 만든 것 ----------
extract_df = function(data, 
                      keyword= NULL , 
                      keyword2= "",
                      keyword3="",
                      keyword4="",
                      keyword5="",
                      show="token",
                      var1="year",
                      var2="키워드"){
  
  if(show =="data"){
    
    data <- data %>% yearsel(year="all",show= "all") %>% 
      dplyr::select(all_of(var1), all_of(var2)) %>%
      Token_gen(show="token")  #토큰화만 실행 
    
    res=data
    
  }else if(show=="token"){ 
    
    #데이터가 선택에 대한 입력이 없으면 전체 출력 
    if(is.null(keyword)){
      data <- data %>% yearsel(year="all",show= "all") %>% 
        dplyr::select(all_of(var1), all_of(var2)) %>%
        Token_gen() 
      
      res=data 
    }else if(keyword=="all"){
      data <- data %>% yearsel(year="all",show= "all") %>% 
        dplyr::select(all_of(var1), all_of(var2)) %>%
        Token_gen() 
      
      res=data 
    }
    else{
      data <- data %>% yearsel(year="all",show= "all") %>% 
        dplyr::select(all_of(var1), all_of(var2)) %>%
        Token_gen() %>% filter(word== keyword| 
                                 word==keyword2|
                                 word==keyword3|
                                 word == keyword4)
      
      res=data }  #이상 내부 조절 
    
  }else if(show=="all"){
    data1 <- data %>% yearsel(year="all",show= "all") %>% 
      dplyr::select(all_of(var1), all_of(var2)) %>%
      Token_gen(show = "token")
    
    data4 <- data %>% yearsel(year="all",show= "all") %>% 
      dplyr::select(all_of(var1), all_of(var2)) %>%
      Token_gen(show = "count")
    
    
    # show ="all"에서 filter입력이 없다면. 실행 
    if(is.null(keyword)){
      data2 ="keyword에 검색할 case를 입력하세요"
      frequencydata="keyword에 검색할 case를 입력하세요"
      
    }else{
      
      data2 <- data1 %>% filter(word== keyword| 
                                  word==keyword2|
                                  word==keyword3|
                                  word == keyword4)
      
      frequencydata = data2 %>% group_by(year, word) %>%
        summarise(N=n(),.groups = "drop")
    }
    
    res= list(df=data1, 
              df_count=data4, 
              sel_df=data2, 
              result=frequencydata)
    # res=data1
    
    
  }else if(show=="combine"){
    data1 <- data %>% yearsel(year="all",show= "all") %>% 
      dplyr::select(all_of(var1), all_of(var2)) %>%
      Token_gen(show = "token")
    
    data2 <- data1 %>% filter(word== keyword| 
                                word==keyword2|
                                word==keyword3|
                                word == keyword4)
    
    frequencydata = data2 %>% group_by(year, word) %>%
      summarise(N=n(), .groups = "drop")
    
    res= frequencydata
  }
  
  res
}

## 코로나19로 데이터결합이 필요 -----
# nstotal %>% extract_df("19","코로나")




# extract_token :: token이 된것에 적용  같은 것을 찾아보기  -------------
extract_token = function(data, 
                         keyword= "" , 
                         keyword2= "",
                         keyword3= "",
                         keyword4= "",
                         show="data",
                         year1="2020"){
  
  library(tidyverse)
  
  
  
  if(show =="data"){
    
    data <- data %>% filter(word== keyword| 
                              word==keyword2|
                              word==keyword3|
                              word == keyword4)
    
    res=data 
    
  }else if(show=="all"){
    
    data0 <- data %>% count(word, sort = T)
    #그룹선별
    data1 <- data %>% filter(word== keyword| 
                               word==keyword2|
                               word==keyword3|
                               word == keyword4)
    #그룹별데이터 
    countdata = data1 %>% group_by(year,word) %>% 
      summarise(N=n(), .groups = "drop")
    
    n_count = count(data1)
    cname = unique(data1[,2])
    N <- cbind.data.frame(word=cname,n_count) %>% as_tibble()
    
    
    data_year_2020 <-data%>% filter(year== "2020") %>% 
      count(word, sort = T) %>%
      slice(1:50)
    data_year_2021 <-data%>% filter(year== "2021") %>% 
      count(word, sort = T)%>%
      slice(1:50)
    data_year_2022 <-data%>% filter(year== "2022") %>% 
      count(word, sort = T)%>%
      slice(1:50)
    data_year_2023 <-data%>% filter(year== "2023") %>% 
      count(word, sort = T)%>%
      slice(1:50)
    year_data = cbind.data.frame(y2020=data_year_2020,
                                 y2021=data_year_2021,
                                 y2022=data_year_2022,
                                 y2023=data_year_2023) %>% as_tibble()
    
    res= list(totalcount=data0,
              year=year_data,
              df=data1,  
              groupby = countdata,
              result=N)
    
  }else if(show=="combine"){
    #데이터 보고서 만들 때 사용할 함수 
    data <- data%>% filter(word== keyword| 
                             word==keyword2|
                             word==keyword3|
                             word == keyword4)
    n_count = count(data)
    cname= unique(data[,2])
    N <- cbind.data.frame(word=cname,n_count) %>% as_tibble()
    res= N
    
  }else if(show== "count"){
    data <- data %>% count(word, sort = T)
    res=data
    
  }else if(show =="year"){
    data1 <-data%>% filter(year== year1) 
    
    data2 <- data1 %>% count(word, sort = T)
    res=data2
    
  }else if(show == "groupby"){
    data0 <- data %>% count(word, sort = T)
    #그룹선별
    data1 <- data %>% filter(word== keyword| 
                               word==keyword2|
                               word==keyword3|
                               word == keyword4)
    #그룹별데이터 
    countdata = data1 %>% group_by(year,word) %>% 
      summarise(N=n(), .groups = "drop")
    res = countdata
  }
  
  res
}


# nstotal_token

# 단어변경 : 같은 것을 하나로 만들기 
# nstotal_token <- nstotal_token %>% change_word("19","코로나19", col=2)
# nstotal_token <- nstotal_token %>% change_word("코로나","코로나19", col=2)



#Text_freq_bar ()  빈도수, bargraph, wordcloud show --------
Text_freq_bar<- function(data, 
                         row.range = 1:30,
                         # fill.color="black",
                         vjust=0,
                         hjust=-2,
                         label.size=3.5,
                         text.x.size=15,
                         text.y.size=15,
                         axis_size = 10,
                         show="all", #"data", "wc", "bar"
                         title="Textmining data
                         (keyword='online', '원격강의') ",
                         subtitle="",
                         caption="",
                         ylim.just=20,
                         wc.range=1:300,
                         wc.shape="circle"){
  library(ggplot2)
  library(dplyr)
  library(wordcloud2)
  # nane
  colnames(data)=c("word","n")
  #arrane data 
  data1 <- data %>% mutate(word=reorder(word, n)) %>% arrange(desc(n))
  #순서 정렬
  
  g<-ggplot(data=data1[row.range,], aes(x=word, y=n))+
    geom_bar(stat="identity")+
    geom_col( aes( fill= row.range))+  #입력된 범위
    coord_flip()+
    geom_text(aes(label=n), vjust= vjust, hjust= hjust, size=label.size)+
    theme_bw()+
    theme(axis.text.x = element_text(size = text.x.size),
          axis.text.y = element_text(size = text.y.size),
          legend.position = "none",
          axis.title.x = element_text(size = axis_size),
          axis.title.y = element_text(size = axis_size)
    )+ylim(0, max(data$n)+ ylim.just)+
    labs(y="빈도수", x="주요 키워드 ", title=title, 
         subtitle=subtitle, caption=caption)
  
  
  # wc <-    data %>% WC(wc.range=wc.range,
  #                      wc.shape=wc.shape
  #                      )
  wc<- data %>%  slice(wc.range) %>% 
    wordcloud2(backgroundColor = "black",
               color="random-light",
               shape=wc.shape
    )
  # cardioid,   # diamond,   # triangle-forward
  # triangle,   # pentagon,   # star
  
  if(show =="data"){
    data1
  }else if(show =="bar"){
    g
  }else if(show=="wc"){
    wc
  } else if(show =="all"){
    res=list(data1, g, wc)
    res
  }
  
}



# #dataframe 번역 column단위로 번역 
# gtl_df <- function(df, col=1){
#   
#   df <-  df %>% as.data.frame()   #tibble to dataframe
#   df[,col]<- as.character(df[,col])  #factor to character
#   
#   for( i in 1:nrow(df)){
#     df[i,col]<- gtl(df[i,col])
#   }
#   df
# }
# 

# 
# #전체 데이터 분석 --------
# nstotal_token_freq <- nstotal_token %>% count(word, sort = T) %>% 
#   Text_freq_bar(show = "data",
#                 row.range = 1:30, #bargraph
#                 text.y.size = 16, 
#                 text.x.size = 14, 
#                 axis_size = 14,
#                 label.size = 5,
#                 title = "Top Keywords and Topics  ",
#                 ylim.just = 5000,
#                 vjust=0.2,
#                 hjust=-0.3,
#                 wc.range = 1:1000, 
#                 wc.shape = "circle")
# #전체 빈도 분석 데이터 
# nstotal_token_freq0 <- nstotal_token_freq %>% 
#   mutate(prop =  paste0(round(n/sum(n)*100,2),"%"))





#빈도수 그래프 ####
#wordcloud2 fucnton ----
WordCloud <- function(data,
                      wc.range=1:100,
                      wc.size=1,
                      wc.shape="circle"){
  library(wordcloud2)
  data%>% slice(wc.range) %>% 
    wordcloud2(backgroundColor = "black",
               color="random-light",
               size= wc.size,
               shape=wc.shape)
}

# nstotal %>% yearsel("2020") %>% 
#   Token_gen() %>%
#   WordCloud()












#히스토그램 #####
Histogram <-function(x, ycut=50, main="", xlab="",ylab=""){
  if(is.numeric(x)){
    hist(x, breaks=max(x), labels=TRUE,
         ylim=c(0, length(x)-ycut),
         density=30, angle=135, border="black",
         main=main, xlab=xlab,ylab=ylab )
    box()

  }else{
    if(!is.numeric(x)){
      x%>%table %>% barplot(ylim=c(0,length(x)))
      text(x %>%table+2,label=x %>%table, cex = 1.5)
    }else(return("numeric 변수를 입력하세요 "))}
}




#히스토그램 #####
histogram <-function(x,text.height=0.1,
                     lim="auto",
                     breaks = "Sturges",
                     text.col="black",
                     text.cex=1,
                     # xlim=c(0, 100),
                     ylim=c(0,100),
                     xlab = "x",
                     ylab = "Freauency",
                     title="Histogram of x"
                     ){
  if(lim=="auto"){
  a <- hist(x,
            breaks= breaks,
            xlab = xlab,
            ylab = ylab,
            main=title)
  }else if(lim=="manaual"){
    a <- hist(x,
              breaks= breaks,
              # xlim=xlim,
              ylim=ylim,
              xlab = xlab,
              ylab = ylab,
              main=title)
  }

  text(a$mids,
       a$counts + text.height,
       label=a$counts,
       cex=text.cex,
       col=text.col)
  box()
}







#왜도와 첨도에 의한 정규성 검정 
#descrive data 정규성 판단####
des.3<- function(input_data, 
                 digit=2){
  library(psych)
  library(knitr)
  input_data<-as.data.frame(input_data)
  output_data <- psych::describe(input_data)
  
  N<-output_data$n
  
  output_data$skew.z <- output_data$skew/sqrt((6*N*((N-1))/((N-2)*(N+1)*(N+3))))
  output_data$kurt.z <- output_data$kurtosis/sqrt((24*N*(N-1)*(N-1))/((N-3)*(N-2)*(N+3)*(N-5)))
  # output_data
  output_data<-as.data.frame(output_data)
  a1 <-output_data%>% kable(digits=digit, 
                            format="pandoc", caption="Describe Statistics data")
  
  output <- round(output_data[,c("n","mean","sd","skew", "kurtosis", "skew.z","kurt.z")],digit)
  #data nomality check making .
  output[,"skew_TF"]<- "Not"
  output[output$skew.z < 3,"skew_TF"]<- "fair"
  output[output$skew.z < 1.96,"skew_TF"]<- "Good"
  output[output$skew.z < -1.96,"skew_TF"]<- "fair"
  output[output$skew.z < -3,"skew_TF"]<- "Not"
  
  output[,"kurt_TF"]<- "Not"
  output[output$kurt.z < 3,"kurt_TF"]<- "fair"
  output[output$kurt.z < 1.96,"kurt_TF"]<- "Good"
  output[output$kurt.z < -1.96,"kurt_TF"]<- "fair"
  output[output$kurt.z < -3,"kurt_TF"]<- "Not"
  
  a2 <-output%>% kable(digits=digit, format="pandoc", caption="Describe Normality, |skew.Z|<1.96,|Krut.z|<1.96 ")
  cat("ref: Kline(2011):skew<3, kurt<10, Crran,West & Finch(1997):skew<2, kurt<7 ","\n",
      "|skew.Z|<1.96,|Krut.z|<1.96인 경우 정규성을 충족한다","\n",
      "H0: 정규성을 충족한다, H1:정규성을 충족하지 않는다")  
  a2
  
  #ss<- list(a2,pairs.panels(input_data))
  #ss
}
# des.3(mds_onl,2) 


# #구글 번역기 -------------


library(tidyverse)
# library(rvest)


# # div.result-container
# 
# "https://papago.naver.com/?sk=ko&tk=en&st="
# https://translate.google.com/m?sl=ko&tl=en&hl=ko&q=한글을+번역


gtranslate2 = function (text="안녕하세요 번역을 시작합니다!(by 박중희).", 
                        slang="ko", 
                        elang="en",
                        show="all") {
  library("rvest")
  library("httpuv")
  url = sprintf("https://translate.google.co.kr/m?sl=%s&hl=%s&q=%s",
                slang,
                elang,
                encodeURIComponent(text))
  
  node = html_nodes(read_html(url), ".result-container")
  
  result= html_text(node)
  
  
  if(show=="all"){
    res = list(input_text= text,
               translate= result, 
               View_browse=browseURL(url)
    )
    res
  }else if(show=="translate"){
    res = result 
    res 
  }else if(show== "browser"){
    res = paste("Input text:\n\n" ,input_text = text,
                "\n",
                "\n",
                "Translate result:\n\n",
                translate= result)
    cat(res)
  }
  
}

gtranslate = function (text="안녕하세요 번역을 시작합니다!(by 박중희).", 
                       slang="ko", 
                       elang="en",
                       show="translate") {
  library("rvest")
  library("httpuv")
  url = sprintf("https://translate.google.co.kr/m?sl=%s&hl=%s&q=%s",
                slang,
                elang,
                encodeURIComponent(text))
  
  node = html_nodes(read_html(url), ".result-container")
  
  result= html_text(node)
  
  
  if(show=="all"){
    res = list(input_text= text,
               translate= result, 
               View_browse=browseURL(url)
    )
    res
  }else if(show=="translate"){
    res = result 
    res 
  }else if(show== "browser"){
    res = paste("Input text:\n\n" ,input_text = text,
                "\n",
                "\n",
                "Translate result:\n\n",
                translate= result)
    cat(res)
  }
  
  # cat(res) 
}

# gtrans() %>% cat()
# # gtrans("Hello, World!", 'en', 'ko')
# # gtrans("통계분석을 통한 전문적인 접근을 시작하려고 합니다", 'ko', 'en')
# gtrans("분산분석(ANOVA)결과, 통계적으로 유의하였다, F(3,150) = 3.46, p<.001.")
# gtrans2("분산분석(ANOVA)결과, 통계적으로 유의하였다, F(3,150) = 3.46, p<.001.")
# gtrans("분산분석(ANOVA)결과, 통계적으로 유의하였다, F(3,150) = 3.46, p<.001.",
#        show="st")
# gtrans("분산분석(ANOVA)결과, 통계적으로 유의하였다, F(3,150) = 3.46, p<.001.") %>% cat()
# # gtrans("텍스트마이닝을 이용한 토픽모델링")
# 
# 
# gtrans("텍스트마이닝을 이용한 토픽모델링", show = "all" )
# 
# gtrans("텍스트마이닝을 이용한 토픽모델링.", show = "st" )
# gtrans2("텍스트마이닝을 이용한 토픽모델링.")
# 



gtl2 = function (text="text를 입력해주시면, 번역을 시작합니다!(by 박중희).", 
                 slang="ko", 
                 elang="en",
                 show="normal") {
  library("rvest")
  library("httpuv")
  url = sprintf("https://translate.google.co.kr/m?sl=%s&hl=%s&q=%s",
                slang,
                elang,
                encodeURIComponent(text))
  #deepl browser 
  deepl_url = sprintf("https://www.deepl.com/ko/translator#%s/%s/%s",
                      slang,
                      elang,
                      encodeURIComponent(text))
  #papago browser
  papago_url = sprintf("https://papago.naver.com/?sk=%s&tk=%s&st=%s",
                       slang,
                       elang,
                       encodeURIComponent(text))
  
  node = html_nodes(read_html(url), ".result-container")
  
  result= html_text(node)
  
  
  if(show=="all"){
    res = list(input_text= text,
               translate= result, 
               View_browse=browseURL(url) )
    res
    
  }else if(show=="translate"){
    res = result 
    res 
  }else if(show== "normal"){
    res = paste("Input text:\n\n" ,
                input_text = text,
                "\n",
                "\n",
                "Translate result:\n\n",
                translate = result)
    cat(res)
  }else if(show =="papago"){  
    #papago browser를 보여주면서 구글로 번역수행 
    res = list( cat(paste("Input text:\n\n" ,
                          input_text = text,
                          "\n",
                          "\n",
                          "Translate result:\n\n",
                          translate = result)),
                papago_translate_to_compare_browser= browseURL(papago_url))
    res
  }else if(show =="deepl"){
    #deepl browser를 보여주면서 구글로 번역수행 
    res = list(cat(paste("Input text:\n\n" ,
                         input_text = text,
                         "\n",
                         "\n",
                         "Translate result:\n\n",
                         translate = result)),
               deepl_translate_to_compare_browser= browseURL(deepl_url))
    res
  }  #show result 
}

# # 
# gtl2("Epistemic Network Analysis (ENA)는 사용자들의 인식속에서 나타나는 데이터 요소 사이의 관계가 만들어지는  연결에 대하여 식별을 하거나 정량화하여 동적인 네트워크를 모델로 표현하는 방법이다. 보통은 fMRI데이터에서 여러 영역에서 뇌의 상호작용을 보이거나 사회적으로 연결되는 관계를 분석하거나 여러 가지 기술의 관계를 통합하여 보여주는 기법이다. 연구자들은 시각적으로 연결된 데이터를 생성하는데, 이때 사용하는 것은 연결의 가중치가 반영된 요약통계를 사용한다. 이 방법을 통하여 동시에 출현하는 행동패턴이나 속성패턴을 분석하여 각종 문제해결의 키포인트를 찾아낼 수 있다는 점에서 유리한 분석이다( Myeong-Hwa Jin et al., 2022).  [그림 39]는 주로 사용되는 ZOOM과 Youtube, Naver band 등과 연결되는 각 학습앱의 속성(attributes)들고 관계를 맺는 정도를 타나낸 것이다. 연결이 된 노드간에는 관계가 있으며, 선의 두께가 두꺼울 수록 더 강한 관계를 맺고 있다는 것을 보여준다.
# 줌의 경우에는 토론에 적합하거나 발표를 하기에 적합하거나 리모트 접속을 부분에서 특징으로 나타났고, 유튜브는 모바일 사용에서 적합하고, 복습하기에 강의를 보는데 매우 적합한 것으로 나타났다. 공통의 특징으로는 사용하기 쉬운 인터페이스와 실시간으로 접속하기에 편리하고, 학습시 접속에 편리한 부분에서 공통점이 나타났다. 또한 두 앱은 기능에 따라서 추가적인 결제를 요구하는 것들이 공통이었다. 네이버밴드는 줌과 유튜브와 공통적인 속성(attributes)부분을 공유하고 있었다.  기술적으로 보완이 필요하다고 느끼는 부분은 줌과 관계가 있고, 복습하기 편리한 것이라는 부분은 유튜브와 관련이 있었다. 
# 진명화, 정유진, 박은주, 김혜준 and 임규연. (2022). 인식연결망분석(ENA)을 적용한 문제해결과정 비교: 초급자와 중급자, 전문가의 문제해결과정 탐색. 교육공학연구, 38(4), 1059-1094",
#      show = "papago") 



gtl = function (text="안녕하세요 번역을 시작합니다!(by 박중희).", 
                slang="ko", 
                elang="en",
                show="translate") {
  library(tidyverse)
  library(rvest)
  library(httpuv)
  
  url = sprintf("https://translate.google.co.kr/m?sl=%s&hl=%s&q=%s",
                slang,
                elang,
                encodeURIComponent(text))
  
  node = html_nodes(read_html(url), ".result-container")
  
  result= html_text(node)
  
  
  if(show=="all"){
    res = list(input_text= text,
               translate= result, 
               View_browse=browseURL(url)
    )
    res
  }else if(show=="translate"){
    res = result 
    res 
    
  }else if(show== "browser"){
    res = paste("Input text:\n\n" ,input_text = text,
                "\n",
                "\n",
                "Translate result:\n\n",
                translate= result)
    cat(res)
    # res
  }
  
}








#" "없이 나오기 
gt = function (text="안녕하세요 번역을 시작합니다!(by 박중희).", 
                slang="ko", 
                elang="en",
                show="translate") {
  library(tidyverse)
  library(rvest)
  library(httpuv)
  
  url = sprintf("https://translate.google.co.kr/m?sl=%s&hl=%s&q=%s",
                slang,
                elang,
                encodeURIComponent(text))
  
  node = html_nodes(read_html(url), ".result-container")
  
  result= html_text(node)
  
  
  if(show=="all"){
    res = list(input_text= text,
               translate= result, 
               View_browse=browseURL(url)
    )
    res
  }else if(show=="translate"){
    res = result 
    cat(res) 
    
  }else if(show== "browser"){
    res = paste("Input text:\n\n" ,input_text = text,
                "\n",
                "\n",
                "Translate result:\n\n",
                translate= result)
    cat(res)
    # res
  }
  
  # 
  # python으로 변경한 코드 ----
  # import requests
  # from bs4 import BeautifulSoup
  # import urllib.parse
  # 
  # def gt(text="안녕하세요 번역을 시작합니다!(by 박중희).", slang="ko", elang="en", show="translate"):
  #   url = f"https://translate.google.co.kr/m?sl={slang}&hl={elang}&q={urllib.parse.quote(text)}"
  # 
  # response = requests.get(url)
  # soup = BeautifulSoup(response.content, 'html.parser')
  # result = soup.find(class_='result-container').text
  # 
  # if show == "all":
  #   res = {'input_text': text, 'translate': result, 'View_browse': url}
  # elif show == "translate":
  #   res = result
  # print(res)
  # elif show == "browser":
  #   res = f"Input text:\n\n{text}\n\nTranslate result:\n\n{result}"
  # print(res)
  # 
  # return res
  
  
  
  
  
  
  
}
# korean to english 
g = function (text="안녕하세요 번역을 시작합니다!(by 박중희).", 
               slang="ko", 
               elang="en",
               show="translate") {
  library(tidyverse)
  library(rvest)
  library(httpuv)
  
  url = sprintf("https://translate.google.co.kr/m?sl=%s&hl=%s&q=%s",
                slang,
                elang,
                encodeURIComponent(text))
  
  node = html_nodes(read_html(url), ".result-container")
  
  result= html_text(node)
  
  
  if(show=="all"){
    res = list(input_text= text,
               translate= result, 
               View_browse=browseURL(url)
    )
    res
  }else if(show=="translate"){
    res = result 
    cat(res) 
    
  }else if(show== "browser"){
    res = paste("Input text:\n\n" ,input_text = text,
                "\n",
                "\n",
                "Translate result:\n\n",
                translate= result)
    cat(res)
    # res
  }
  
}
# english  to koean 
e = function (text="안녕하세요 번역을 시작합니다!(by 박중희).", 
              slang="en", 
              elang="ko",
              show="translate") {
  library(tidyverse)
  library(rvest)
  library(httpuv)
  
  url = sprintf("https://translate.google.co.kr/m?sl=%s&hl=%s&q=%s",
                slang,
                elang,
                encodeURIComponent(text))
  
  node = html_nodes(read_html(url), ".result-container")
  
  result= html_text(node)
  
  
  if(show=="all"){
    res = list(input_text= text,
               translate= result, 
               View_browse=browseURL(url)
    )
    res
  }else if(show=="translate"){
    res = result 
    cat(res) 
    
  }else if(show== "browser"){
    res = paste("Input text:\n\n" ,input_text = text,
                "\n",
                "\n",
                "Translate result:\n\n",
                translate= result)
    cat(res)
    # res
  }
  
}


gbm = function (text="안녕하세요 번역을 시작합니다!(by 박중희).", 
              slang="ko", 
              elang="en",
              show="all") {
  library(tidyverse)
  library(rvest)
  library(httpuv)
  
  url = sprintf("https://translate.google.co.kr/m?sl=%s&hl=%s&q=%s",
                slang,
                elang,
                encodeURIComponent(text))
  
  node = html_nodes(read_html(url), ".result-container")
  
  result= html_text(node)
  
  
  if(show == "all"){
    res = list(input_text= text,
               translate= result, 
               View_browse=browseURL(url)
    )
    res
  }else if(show=="translate"){
    res = result 
    cat(res) 
    
  }else if(show== "browser"){
    res = paste("Input text:\n\n" ,input_text = text,
                "\n",
                "\n",
                "Translate result:\n\n",
                translate= result)
    cat(res)
    # res
  }
  
}
# https://translate.google.com/?hl=ko&sl=en&tl=ko&text=korea&op=translate

gb = function (text="안녕하세요 번역을 시작합니다!(by 박중희).", 
               slang="ko", 
               elang="en",
               show="all") {
  library(tidyverse)
  library(rvest)
  library(httpuv)
  
  # url = sprintf("https://translate.google.co.kr/m?sl=%s&hl=%s&q=%s",
  #               slang,
  #               elang,
  #               encodeURIComponent(text))
  url = sprintf("https://translate.google.com/?hl=ko&sl=%s&tl=%s&text=%s&op=translate",
                slang,
                elang,
                encodeURIComponent(text))
  #위치는 다시 찾아야 함. 목소리를 듣기 위해서 브라우저를  실행 
  node = html_nodes(read_html(url), ".result-container")
  
  result= html_text(node)
  
  
  if(show == "all"){
    res = list(input_text= text,
               translate= result, 
               View_browse=browseURL(url)
    )
    res
  }else if(show=="translate"){
    res = result 
    cat(res) 
    
  }else if(show== "browser"){
    res = paste("Input text:\n\n" ,input_text = text,
                "\n",
                "\n",
                "Translate result:\n\n",
                translate= result)
    cat(res)
    # res
  }
  
}
# "한국" %>% gb()

#gtl_df() dataframe 번역 ----------
#영어논문을 만들 때, 표에 들어가는 단어를 번역하여 넣기 
#너무 많은 수는 오래걸림--> 웹페이지를 갔다와야 하므로... 
gtl_df <- function(df, col=1){
  
  df <-  df %>% as.data.frame()   #tibble to dataframe
  df[,col]<- as.character(df[,col])  #factor to character
  
  for( i in 1:nrow(df)){
    df[i,col]<- gtl(df[i,col])
  }
  df
}
# df0 <-  data.frame(word=c("온라인","진행","교육"),
#                    n=c(100, 200, 300))
# df0 %>% gtl_df()

# 
# gtl_df2 <- function(df, col=1, ecol=NULL){
#   
#   
#   df <-  df %>% as.data.frame()   #tibble to dataframe
#   df[,col]<- as.character(df[,col])  #factor to character
#   
#   for( i in 1:nrow(df)){
#     df[i,col]<- gtl(df[i,col])
#   }
#   
#   
#   df
# }

#kakao--------
ktl = function (text="", slang="ko", elang="en") {
  library("httr")
  library("httpuv")
  response <- POST("https://translate.kakao.com/translator/translate.json",
                   body=sprintf("queryLanguage=%s&resultLanguage=%s&q=%s", 
                                                                                                        slang, 
                                                                                                        elang, 
                                                                                                        encodeURIComponent(text)),
                   add_headers(.headers=c("Referer"="https://translate.kakao.com/",
                                          "content-type"="application/x-www-form-urlencoded; charset=UTF-8"))
  )
  data = content(response, "parsed")
  output = data$result$output
  
  # paste(output, sep="\n")
  # output
  data_result= unlist(output)
  res= paste(data_result) 
  cat(res) 
}

# ?content



#kakao--------
k = function (text="", slang="ko", elang="en") {
  library("httr")
  library("httpuv")
  response <- POST("https://translate.kakao.com/translator/translate.json",
                   body=sprintf("queryLanguage=%s&resultLanguage=%s&q=%s", 
                                slang, 
                                elang, 
                                encodeURIComponent(text)),
                   add_headers(.headers=c("Referer"="https://translate.kakao.com/",
                                          "content-type"="application/x-www-form-urlencoded; charset=UTF-8"))
  )
  data = content(response, "parsed")
  output = data$result$output
  
  # paste(output, sep="\n")
  # output
  data_result= unlist(output)
  res= paste(data_result) 
  # cat(res) 
  res
}

# ?content





ktl2 = function (text="", 
                 slang="ko", 
                 elang="en",
                 show ="normal") {
  library("httr")
  library("httpuv")
  response <- POST("https://translate.kakao.com/translator/translate.json",    
                   body=sprintf("queryLanguage=%s&resultLanguage=%s&q=%s", 
                                                                                            slang, 
                                                                                            elang, 
                                                                                            encodeURIComponent(text)),
                   add_headers(.headers=c("Referer"="https://translate.kakao.com/",
                                          "content-type"="application/x-www-form-urlencoded; charset=UTF-8"))
  )
  data = content(response, "parsed")
  output = data$result$output
  
  #데이터를 list에서 character로 만들기 
  data_result_input= unlist(data$result$input)
  data_result_output= unlist(data$result$output)
  #결과 정리 
  resout = paste(data_result_output) 
  resin = paste(data_result_input) 
  
  if(show =="normal"){
    cat("Source language: \n\n",resin,"\n\n")
    cat("Translate language: \n\n", resout)
    
  }else if(show == "data"){
    res= list(source= resin, 
              translate = resout)
    res
  }
}
# 
# ktl2("Epistemic Network Analysis (ENA)는 사용자들의 인식속에서 나타나는 데이터 요소 사이의 관계가 만들어지는  연결에 대하여 식별을 하거나 정량화하여 동적인 네트워크를 모델로 표현하는 방법이다. 보통은 fMRI데이터에서 여러 영역에서 뇌의 상호작용을 보이거나 사회적으로 연결되는 관계를 분석하거나 여러 가지 기술의 관계를 통합하여 보여주는 기법이다. 연구자들은 시각적으로 연결된 데이터를 생성하는데, 이때 사용하는 것은 연결의 가중치가 반영된 요약통계를 사용한다. 이 방법을 통하여 동시에 출현하는 행동패턴이나 속성패턴을 분석하여 각종 문제해결의 키포인트를 찾아낼 수 있다는 점에서 유리한 분석이다( Myeong-Hwa Jin et al., 2022).  [그림 39]는 주로 사용되는 ZOOM과 Youtube, Naver band 등과 연결되는 각 학습앱의 속성(attributes)들고 관계를 맺는 정도를 타나낸 것이다. 
# 연결이 된 노드간에는 관계가 있으며, 선의 두께가 두꺼울 수록 더 강한 관계를 맺고 있다는 것을 보여준다.
# 줌의 경우에는 토론에 적합하거나 발표를 하기에 적합하거나 리모트 접속을 부분에서 특징으로 나타났고, 유튜브는 모바일 사용에서 적합하고, 복습하기에 강의를 보는데 매우 적합한 것으로 나타났다. 공통의 특징으로는 사용하기 쉬운 인터페이스와 실시간으로 접속하기에 편리하고, 학습시 접속에 편리한 부분에서 공통점이 나타났다. 또한 두 앱은 기능에 따라서 추가적인 결제를 요구하는 것들이 공통이었다. 네이버밴드는 줌과 유튜브와 공통적인 속성(attributes)부분을 공유하고 있었다.  기술적으로 보완이 필요하다고 느끼는 부분은 줌과 관계가 있고, 복습하기 편리한 것이라는 부분은 유튜브와 관련이 있었다. 
#      진명화, 정유진, 박은주, 김혜준 and 임규연. (2022). 인식연결망분석(ENA)을 적용한 문제해결과정 비교: 초급자와 중급자, 전문가의 문제해결과정 탐색. 교육공학연구, 38(4), 1059-1094",show="data")





kakaoi = function (text="", 
                   slang="ko", 
                   elang="en",
                   show =" normal") {
  library("httr")
  library("httpuv")
  response <- POST("https://translate.kakao.com/translator/translate.json",    
                   body=sprintf("queryLanguage=%s&resultLanguage=%s&q=%s", 
                                                                                            slang, 
                                                                                            elang, 
                                                                                            encodeURIComponent(text)),
                   add_headers(.headers=c("Referer"="https://translate.kakao.com/",
                                          "content-type"="application/x-www-form-urlencoded; charset=UTF-8"))
  )
  data = content(response, "parsed")
  output = data$result$output
  
  #데이터를 list에서 character로 만들기 
  data_result_input= unlist(data$result$input)
  data_result_output= unlist(data$result$output)
  #결과 정리 
  resout = paste(data_result_output) 
  resin = paste(data_result_input) 
  
  if(show ==" normal"){
    cat("Source language: \n\n",resin,"\n\n")
    cat("Translate language: \n\n", resout)
  }else if(show == "data"){
    res= list(source= resin, 
              translate = resout)
    res
  }
}
# # 
# kakaoi("안녕하세요. 한글을 영어로 번역해보겠습니다.
#        그리고 언어적 문제도 체크합니다. ",
#        show="data")

# ptl("안녕하세요. 한글을 영어로 번역해보겠습니다.")
# dtl("안녕하세요. 한글을 영어로 번역해보겠습니다.")
# gtl2("안녕하세요. 한글을 영어로 번역해보겠습니다.")

# https://papago.naver.com/?sk=ko&tk=en&st=한국어%20번역
# https://papago.naver.com/?sk=%s&tk=%s&st=%s&hl=%s&q=%s



# ptl papago translation ------------------------------------------------------

ptl = function(text="", slang='ko', 
               elang='en', 
               translator='papago',
               translator_2='itranslate',
               show="one"){
  
  library(httr)
  library(httpuv)
  
  host = "http://121.129.167.192:10147"
  url = sprintf("%s/%s/%s/%s?text=%s",
                host, 
                translator, 
                slang, 
                elang,
                encodeURIComponent(text))
  response <- GET(url)
  # print(url)
  
  url_2 = sprintf("%s/%s/%s/%s?text=%s",
                  host, 
                  translator_2, 
                  slang, 
                  elang,
                  encodeURIComponent(text))
  response_2 <- GET(url_2)
  
  
  if(show == "all"){
    res = list(source= cat("source \n\n", text, "\n\n"),
               target= cat("translate ===>",translator,
                           " \n\n", content(response, "text",
                                            encoding = "UTF-8"),"\n\n"),
               taget_2= cat("translate(2nd recommandation) ==>",
                            translator_2," \n\n", 
                            content(response_2, "text",
                                    encoding = "UTF-8"),"\n\n"))
    
  }else if(show == "normal"){
    res = list(source= cat("source \n\n", text, "\n\n"),
               target= cat("translate ===>",translator,
                           " \n\n", content(response, "text",
                                            encoding = "UTF-8"),"\n\n"))
  }else if(show == "one"){
    res=content(response, "text", encoding = "UTF-8")
  }
  
  cat(res)
}


# 

# papago browser ----------------------------------------------------------


papago = function (text="안녕하세요 번역을 시작합니다!(by 박중희).", 
                   slang="ko", 
                   elang="en",
                   show="browser") {
  library("rvest")
  library("httpuv")
  papago_url = sprintf("https://papago.naver.com/?sk=%s&tk=%s&st=%s",
                       slang,
                       elang,
                       encodeURIComponent(text))
  # 
  # node = html_nodes(read_html(papago_url), xpath='//*[@id="txtTarget"]/span')
  # 
  # result= html_text(node)
  # 
  
  if(show=="all"){
    res = list(input_text= text,
               translate= result, 
               View_browse=browseURL(papago_url)
    )
   
  }else if(show=="translate"){
    res = result 
    
  }else if(show== "browser"){
    res = View_browse=browseURL(papago_url)
   
  }
  res
  }


# https://www.deepl.com/ko/translator#ko/en

# https://www.deepl.com/ko/translator#ko/en


# dtl is deepl ------------------------------------------------------------


dtl = function (text="안녕하세요 번역을 시작합니다!(by 박중희).", 
                slang="ko", 
                elang="en",
                show="all") {
  library("rvest")
  library("httpuv")
  deepl_url = sprintf("https://www.deepl.com/ko/translator#%s/%s/%s",
                      slang,
                      elang,
                      encodeURIComponent(text))
  
  node = html_nodes(read_html(deepl_url), xpath='//*[@id="headlessui-tabs-panel-7"]')
  
  result= html_text(node)
  
  
  if(show=="all"){
    res = list(input_text= text,
               translate= result, 
               View_browse=browseURL(deepl_url)
    )
    res
  }else if(show=="translate"){
    res = result 
    res 
  }else if(show== "browser"){
    res = paste("Input text:\n\n" ,input_text = text,
                "\n",
                "\n",
                "Translate result:\n\n",
                translate= result)
    cat(res)
  }
  
}


# deepl -------------------------------------------------------------------


deepl = function (text="안녕하세요 번역을 시작합니다!(by 박중희).", 
                  slang="ko", 
                  elang="en",
                  show="all") {
  library("rvest")
  library("httpuv")
  deepl_url = sprintf("https://www.deepl.com/ko/translator#%s/%s/%s",
                      slang,
                      elang,
                      encodeURIComponent(text))
  
  node = html_nodes(read_html(deepl_url), xpath='//*[@id="headlessui-tabs-panel-7"]')
  
  result= html_text(node)
  
  
  if(show=="all"){
    res = list(input_text= text,
               translate= result, 
               View_browse=browseURL(deepl_url)
    )
    res
  }else if(show=="translate"){
    res = result 
    res 
  }else if(show== "browser"){
    res = paste("Input text:\n\n" ,input_text = text,
                "\n",
                "\n",
                "Translate result:\n\n",
                translate= result)
    cat(res)
  }
  
}



# jtl 박재형 -----------------------------------------------------------------


jtl = function(text="", slang='ko', 
               elang='en', 
               translator='papago',
               translator_2='itranslate',
               show="one"){
  
  library(httr)
  library(httpuv)
  
  host = "http://121.129.167.192:10147"
  url = sprintf("%s/%s/%s/%s?text=%s",
                host, 
                translator, 
                slang, 
                elang,
                encodeURIComponent(text))
  response <- GET(url)
  # print(url)
  
  url_2 = sprintf("%s/%s/%s/%s?text=%s",
                  host, 
                  translator_2, 
                  slang, 
                  elang,
                  encodeURIComponent(text))
  response_2 <- GET(url_2)
  
  
  if(show == "all"){
    res = list(source= cat("source \n\n", text, "\n\n"),
               target= cat("translate ===>",translator,
                           " \n\n", content(response, "text",
                                            encoding = "UTF-8"),"\n\n"),
               taget_2= cat("translate(2nd recommandation) ==>",
                            translator_2," \n\n", 
                            content(response_2, "text",
                                    encoding = "UTF-8"),"\n\n"))
    
  }else if(show == "normal"){
    res = list(source= cat("source \n\n", text, "\n\n"),
               target= cat("translate ===>",translator,
                           " \n\n", content(response, "text",
                                            encoding = "UTF-8"),"\n\n"))
  }else if(show == "one"){
    res = content(response, "text", encoding = "UTF-8") %>% cat()
  }
  res
  
}


#여러가지 옵션을 제공하는 경우 
jtl_all = function(text="", slang='ko', 
                   elang='en', 
                   translator='papago',
                   translator_2='itranslate',
                   show="all"){
  '
  사용할 수 있는 translator 목록
  
  [ 
    "qqTranSmart" <--- 빙과 비슷 
    "argos", <--- 내용을 간단하게 해서 생략함. 
    "bing", <---글자수 제한이짧음 
    "papago", "google", 
     "itranslate",   <---- Good:너에게 이야기 하듯이.. you
     ,"reverso",
    "cloudTranslation",   <-------- fair
    iciba"  <-- summary style 그들이...
    "sysTran", "modernMt",    <-- not simple , long ...lazy
    "sogou",   <---번역을 잘 못함, 놓치는 부분...
    "lingvanex"  <-- kr_KR 옵션이 다름 itrans와 비슷 
    "baidu"<--- kor이 옵션
    
    
    "deepl", "caiyun",<--------------- not supprot 
    "alibaba",   "baidu", "apertium", "tilde", 
    "elia",    "caiyun", "niutrans",
    , "iflytek", "translateCom", "qqFanyi",
    "languageWire", "judic", "myMemory", "mglip",
    "mirai","translateMe",     "utibet","volcEngine""yeekit",
        "yandex",  "youdao" 
  ]
  
  '
  
  library(httr)
  library(httpuv)
  
  
  host = "http://121.129.167.192:10147"
  url = sprintf("%s/%s/%s/%s?text=%s",
                host, 
                translator, 
                slang, 
                elang,
                encodeURIComponent(text))
  response <- GET(url)
  # print(url)
  
  url_2 = sprintf("%s/%s/%s/%s?text=%s",
                  host, 
                  translator_2, 
                  slang, 
                  elang,
                  encodeURIComponent(text))
  response_2 <- GET(url_2)
  
  
  if(show == "all"){
    cat("source \n\n", text, "\n\n")
    cat("(1)translate ===>",translator,"------------------",
        " \n\n", content(response, "text",
                         encoding = "UTF-8"),"\n\n")
    cat("(2)translate(2nd recommandation) ==>",
        translator_2,"------------------"," \n\n", 
        content(response_2, "text",
                encoding = "UTF-8"),"\n\n")
    
  }else if(show == "normal"){
    res = list(source= cat("source \n\n", text, "\n\n"),
               target= cat("translate ===>",translator,
                           " \n\n", content(response, "text",
                                            encoding = "UTF-8"),"\n\n"))
  }else if(show == "one"){
    res=content(response, "text", encoding = "UTF-8")
  }
  
  
}


jtl2 = function(text="", 
                slang='ko', 
                elang='en', 
                translator='google',
                translator_2='papago'){
  '
  사용할 수 있는 translator 목록
  
  [ 
    "qqTranSmart" <--- 빙과 비슷 
    "argos", <--- 내용을 간단하게 해서 생략함. 
    "bing", <---글자수 제한이짧음 
    "papago", "google", 
     "itranslate",   <---- Good:너에게 이야기 하듯이.. you
     ,"reverso",
    "cloudTranslation",   <-------- fair
    iciba"  <-- summary style 그들이...
    "sysTran", "modernMt",    <-- not simple , long ...lazy
    "sogou",   <---번역을 잘 못함, 놓치는 부분...
    "lingvanex"  <-- kr_KR 옵션이 다름 itrans와 비슷 
    "baidu"<--- kor이 옵션
    
    
    "deepl", "caiyun",<--------------- not supprot 
    "alibaba",   "baidu", "apertium", "tilde", 
    "elia",    "caiyun", "niutrans",
    , "iflytek", "translateCom", "qqFanyi",
    "languageWire", "judic", "myMemory", "mglip",
    "mirai","translateMe",     "utibet","volcEngine""yeekit",
        "yandex",  "youdao" 
  ]
  
  '
  library("httr")
  library("httpuv")
  
  
  host = "http://121.129.167.192:10147"
  url = sprintf("%s/%s/%s/%s?text=%s",
                host, 
                translator, 
                slang, 
                elang,
                encodeURIComponent(text))
  response <- GET(url)
  # print(url)
  
  url_2 = sprintf("%s/%s/%s/%s?text=%s",
                  host, 
                  translator_2, 
                  slang, 
                  elang,
                  encodeURIComponent(text))
  response_2 <- GET(url_2)
  
  
  
  cat("(1) source  ===> input:  \n\n", text, "\n\n\n")
  
  cat("(2) translate(first) ===> [",translator,"]",
      " \n\n", content(response, "text",
                       encoding = "UTF-8"),"\n\n\n")
  
  cat("(3) translate(2nd recommandation) ==> [",
      translator_2,"]"," \n\n", 
      content(response_2, "text",
              encoding = "UTF-8"),"\n\n")
  
  cat("(4) translate(cross check) ==> [","kakaoi","]"," \n\n")
  ktl(text,slang,elang) #kakao function
}


# jtl("미국식 사고가 어렵다.")
# #
# # jtl(translator = "cloudTranslation",


jtl3 = function(text="", 
                slang='ko', 
                elang='en'){
  
  
  # input=text
  cat("입력된 내용:\n\n ", text,"\n\n\n")
  cat("(1) google------------- \n\n", 
      gtl(text,slang, elang),"\n\n")
  #     "(2) kakakoi------------- \n\n")
  # cat(ktl(text,slang, elang),"\n\n")
  # cat("kakaoi \n\n", kakaoi(text,slang, elang),"\n\n\n")
  cat("(2)kakaoi_sentence------------- \n\n")
  ktl3(text,slang, elang)
  
}


# 
# 
# jtl(
#     "Epistemic Network Analysis (ENA)는 사용자들의 인식 데이터 요소 사이의 관계를 통해 만들어지는 연결에 대하여 식별을 하거나 정량화하여 동적인 네트워크 모델을 통해 관계를 표현해 내는 연구방법이다. 보통 여러 영역에서 발생하는 뇌의 상호작용을 fMRI 데이터를 이용하여분석하거나, 사회적으로 연결되는 사회망 관계를 분석하거나, 여러 가지 기술의 연결 관계를 그래프를 통하여 보여주는 기법이다. 연구자들은 시각적으로 연결된 데이터를 생성할 수 있으며, 이때 연결의 가중치가 반영된 요약통계를 사용하여 구성한다.이 방법을 통하여 동시에 출현하는 행동패턴이나 속성패턴을 분석할 수 있으며, 다양한 문제해결 방법에 대한 키포인트를 찾아낼 수 있다는 점에서 유리한 분석이다( Myeong-Hwa Jin et al., 2022).
# [그림 39]는 주로 사용되는 ZOOM과 Youtube, Naver band 등과 연결되는 각 학습앱의 속성(attributes)들고 관계를 맺는 정도를 타나낸 것이다. 연결이 된 노드간에는 관계가 있으며, 선의 두께가 두꺼울 수록 더 강한 관계를 맺고 있다는 것을 보여준다.줌의 경우에는 토론에 적합하거나 발표를 하기에 적합하거나 리모트 접속을 부분에서 특징으로 나타났고, 유튜브는 모바일 사용에서 적합하고, 복습하기에 강의를 보는데 매우 적합한 것으로 나타났다.
# 공통의 특징으로는 사용하기 쉬운 인터페이스와 실시간으로 접속하기에 편리하고, 학습시 접속에 편리한 부분에서 공통점이 나타났다. 또한 두 앱은 기능에 따라서 추가적인 결제를 요구하는 것들이 공통이었다. 네이버밴드는 줌과 유튜브와 공통적인 속성(attributes)부분을 공유하고 있었다.  기술적으로 보완이 필요하다고 느끼는 부분은 줌과 관계가 있고, 복습하기 편리한 것이라는 부분은 유튜브와 관련이 있었다.  ")
