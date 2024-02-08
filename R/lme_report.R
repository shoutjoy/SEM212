#' linear mixed model report
#' @param lmedata lmedata is lmer() function result
#' @param apa = default FALSE, if you  REML is FALSE
#' @param fit_more = default FALSE, TRUE detailed report
#' @export
#' @examples
#' \dontrun{
#' data(sleepstudy)
#' sleepstudy |> str()
#' lmer(Reaction ~ Days + (Days | Subject), sleepstudy) |> lme_report()
#' lmer(Reaction ~ Days + (Days | Subject), sleepstudy) |> lme_report(apa=TRUE)
#'
#'
#' book.url <- "https://stat.ethz.ch/~meier/teaching/book-anova"
#' quality <- readRDS(url(file.path(book.url, "data/quality.rds")))
#' str(quality)
#' fit.quality <- lmer(score ~ (1 | employee) + (1 | batch) +
#'                      (1 | employee:batch), data = quality)
#' summary(fit.quality)
#' fit.quality|> lme_report()
#' fit.quality|> lme_report(apa= TRUE) #error the app is FALSE
#' fit.quality|> lme_report(apa=FALSE) #error the app is FALSE
#' fit.quality|> ICC()
#'
#' lmer(Reaction ~ Days + (Days | Subject), sleepstudy) |>
#' SEM212::lme_report(apa=TRUE, fit_more = T)
#' lmer(Reaction ~ Days + (Days | Subject), sleepstudy) |>
#' SEM212::lme_report(apa=TRUE, fit_more = F)  #default FALSE, it is show that AIC and BIC
#'
#' }
#'
lme_report <- function(lmedata, apa=FALSE, fit_more=FALSE){

  library(multilevelTools)
  #formula output
  formula = lmedata@call

  #generate summary data
  lmedata_summary <- summary(lmedata)

  #fixed effect
  fixed_effect <- lmedata_summary$coefficients

  # lmedata |> # summary()|> coef()


  #random effect
  random_effect <- data.frame(lme4::VarCorr(lmedata))


  #ICC
  icc =  random_effect |>
    dplyr::mutate(Sum = sum(vcov),
                  ICC = (vcov/Sum),
                  ICC_ratio = paste0(round((vcov/Sum)*100, 2),"%"),
                  ICC_rank = rank(dplyr::desc(ICC))
    ) |>
    dplyr::select(1:2,7,8,9)

  CI = confint(lmedata, oldNames = FALSE)


  #apa output
  if(apa){
    apa = lmedata |>
            JWileymisc::modelTest() |>
            JWileymisc::APAStyler()

  }else{
    apa = NULL
  }

  # bind_cols(AIC(lmedata), BIC(lmedata))


  #model fit
  if(fit_more){
  fit = lmedata |> JWileymisc::modelPerformance()
  }else{
    fit = dplyr::bind_cols(AIC = AIC(lmedata), BIC= BIC(lmedata))
  }

  # result
  res = list(formula = formula,
             Fixed_effect = fixed_effect,
             Random_effect = random_effect,
             ICC = icc,
             ConfidenceInterval_95 = CI,
             FIT = fit,
             APA = apa
  )

  res
}
