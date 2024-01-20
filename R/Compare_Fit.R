#' model fit
#'
#' @param ... lavaan result
#' @param option option =1 markdonw, 2 data.frame
#' @param format "markdown', 'html'

#'
#' @return paper format
#' @export
#'

Compare_Fit <- function(...,
                        option=2,
                        format="markdown") {
  library(magrittr)
  # library(stargazer)
  library(tibble)
  library(knitr)
  library(dplyr)


  m <- list(...)
  tryCatch({

    if(option==1){ #knitr table
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
      if(option==2){n # data.frame
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
      else{ return(" choose your option = 1, 2")}
    }
    return(result)

  },error=function(e)return("input option=1 or option=2  "))
}
