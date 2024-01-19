
#'  Comparison of Model Fit Indices of two or more structural equation models
#' @export
#' @param ... lavaan result and input fit1, fit2, ...


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
