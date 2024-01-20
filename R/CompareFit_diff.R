#'  Comparison of Model Fit Indices of two or more structural equation models
#' @param fit lavaan result and input fit1, fit2, ...
#'
#' @param ... lavaan result and input fit1, fit2, ...
#'
#' @export
CompareFit_diff<- function(fit, ...) {
  # library(magrittr)
  # # library(stargazer)
  # library(tibble)
  # library(knitr)
  # library(dplyr)
  # librar(lavaan)


  m <- list(fit, ...)

  result <- sapply(m, lavaan::fitMeasures) %>%
    magrittr::set_colnames(paste0("Model_", 1:length(m))) %>%
    as.data.frame() %>%
    tibble::rownames_to_column("Fit_Measures") %>%
    dplyr::slice(match(c("chisq",
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
    dplyr::mutate(Fit_Measures=c("Chi-square",
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
    result%>% dplyr::mutate("diff(M2-M1)"=`Model_2`-`Model_1`) %>%
      dplyr::mutate(check=ifelse(`diff(M2-M1)`>0,"Increase(+)",
                          ifelse(`diff(M2-M1)`<0,"Decrese(-)",""))) %>%
      knitr::kable("markdown",3)

  },error=function(e)return("Enter two SEM analysis data to compare!!") )



}
