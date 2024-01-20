#' descriptive statistics
#'
#' @param x data.frame or etc
#' @param type  result option. default 'normalitytest'. if you possible select optuon. 'col', 'row','skew','kurt','mean','sd', 'n', 'median', 'all', 'normalitytest','normalitytest_data', 'df', 'mvn','shapiro', 'ref'
#' @param digit rounding default 2
#' @examples
#' #example code
#' \dontrun{
#'  mtcars%>% Summarise("col", 2)
#'  mtcars %>% Summarise("mean",4)
#'  mtcars %>% Summarise("sd",3)
#'  mtcars %>% Summarise("skew",4)
#'  mtcars %>% Summarise("kurt",4)
#'  mtcars %>% Summarise("all")
#'  mtcars %>% Summarise("all_tibble")
#'  mtcars %>% Summarise("all", 3)
#'  mtcars %>% Summarise("all", 4)
#'  mtcars %>% Summarise("normalitytest")
#'  mtcars %>% Summarise("normalitytest", 3)
#'  mtcars %>% Summarise("normalitytest_data")
#'
#' }
#'
#'
#' @export
#'
#'
Summarise <- function(x,
                      type="normalitytest",
                      digit=2){
  # library(stringr)
  # library(tidyverse)
  # library(MVN)
  tryCatch({
    #col: Display vertically (variable is vertical)
    #row: Display horizontally (variable is horizontal)
    x <- x %>% as.data.frame()
    s_col <- dplyr::summarise(x,
                       dplyr::across( .cols=c(1:ncol(x)),
                               .fns = function(x){
                                 stringr::str_c(round(mean(x, na.rm = T),
                                             digit),"(", round(sd(x),digit),")" )}
                       )) %>% t() %>% as.data.frame()
    colnames(s_col)="Mean(sd)"

    s_row<- s_col %>% t() %>% data.frame()


    #sample size
    s_n <- dplyr::summarise(x,  dplyr::across(
      .cols=c(1:ncol(x)),
      .fns = function(x){length(x)}
    )) %>% t()
    colnames(s_n)="n"
    #mean
    s_mean <-  dplyr::summarise(x,  dplyr::across(
      .cols=c(1:ncol(x)),
      .fns = function(x){mean(x, na.rm = T)}
    )) %>% t() #%>% as.data.frame()
    colnames(s_mean)="mean"

    #Standard Deviation
    s_sd <- dplyr::summarise(x,  dplyr::across(
      .cols=c(1:ncol(x)),
      .fns = function(x){sd(x, na.rm = T)}
    )) %>% t()# %>% as.data.frame()
    colnames(s_sd)="sd"

    #skewness
    s_skew <- dplyr::summarise(x,  dplyr::across(
      .cols=c(1:ncol(x)),
      # .fns = function(x){SKEW(x)}
      .fns = function(x){ mean((x - mean(x))^3)/(sd(x)^3)}
    )) %>% t() #%>% as.data.frame()
    colnames(s_skew)="skew"
    #kurtosis
    s_kurt <- dplyr::summarise(x,  dplyr::across(
      .cols=c(1:ncol(x)),
      # .fns = function(x){KURT(x)}
      .fns = function(x){mean((x - mean(x))^4) /(sd(x)^4) - 3}
    )) %>% t() #%>% as.data.frame()
    colnames(s_kurt)="kurtosis"

    #median
    s_median <- dplyr::summarise(x,  dplyr::across(
      .cols=c(1:ncol(x)),
      .fns = function(x){median(x)}
    )) %>% t() #%>% as.data.frame()
    colnames(s_median)="median"

    #colname addon
    colName = colnames(x)

    # description = bind_cols(variable= colName,
    #                         round(s_mean, digit),
    #                         round(s_sd, digit),
    #                         s_n,
    #                         round(s_skew, digit),
    #                               round(s_kurt, digit) ) %>% tibble()
    description = dplyr::bind_cols(s_mean,
                            s_sd,
                            s_n,
                            s_skew,
                            s_kurt )
    #

    #Normality test data
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

    output_res = dplyr::bind_cols(variabel= colName,
                          round(output_df, digit),
                          output[,c(8:9)]
    )


    ref = c("Reference:
     (1) Kline (2011):skew<3, kurt<10,
     (2) Crran, West & Finch (1997): skew<2, kurt<7
     (3) Normality Test
        |skew.Z|<1.96,|Krut.z|<1.96 -> normality is satisfied
      H0: Satisfies normality,
      H1: Does not meet normality \n \n "
    )

    #### Normality : test Henze-Zirkler, Anderson-Darling -----
    mvn_data = MVN::mvn(x, mvnTest = "mardia")  ## 11plot auto
    shapirotest = MVN::mvn(x, univariateTest="SW")
    mvn_hz = MVN::mvn(x, mvnTest = "hz")




    switch(type,
           col = s_col,
           row = s_row,
           skew = round(s_skew, digit),
           kurt = round(s_kurt, digit),
           mean = round(s_mean,digit),
           sd = round(s_sd, digit),
           n = s_n,
           median = s_median,
           all = dplyr::bind_cols(variable= colName, output_data0),
           all_tibble = dplyr::bind_cols(variable= colName,
                                  output_data0) %>% tibble::tibble(),
           normalitytest = list(cat(ref), output_res),
           normalitytest_data = output_res,
           df = output_res,
           mvn = mvn_hz,
           shapiro = shapirotest,
           ref= cat(ref)


    )
  },error=function(e)return("Error: Non-Numeric variable included. Check the data again. Run missCheck() to see if there is NA data. Then, run the functions SKEW() and KURT()."))

}
