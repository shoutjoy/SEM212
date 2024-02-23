#' linear mixed model report
#' @param lmedata lmedata is lmer() function result
#' @param apa = default FALSE, if you  REML is FALSE
#' @param fit_more = default FALSE, TRUE detailed report
#' @param type = 'all' is res, 'Fixed_effect','Random_effect','ICC', 'ConfidenceInterval_95','Satterthwaite_method','FIT','APA'
#' @param ranef_sig ranef_sig =TRUE  random effect test
#' @param form 'lmer','glmer' = 'logit'='dich', 'poisson' ='pois'
#' @param show.effect if you show.effect = TRUE -> show ranef, fixef, coef
#' @param show.ci TRUE calculation 95percent CI
#' @description
#'  Mixed model summary
#'
#' @export
#' @examples
#' \dontrun{
#' data(sleepstudy)
#'
#' sleepstudy |> str()
#'
#' lmer(Reaction ~ Days + (Days | Subject), sleepstudy) |> lme_report()
#'
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
lme_report <- function(lmedata,
                       type = "basic",
                       form = "lmer",
                       apa=FALSE,
                       fit_more=FALSE,
                       ranef_sig = FALSE,
                       show.effect=FALSE,
                       show.ci=FALSE){

  library(multilevelTools)
  #formula output
  formula = lmedata@call

  #generate summary data
  lmedata_summary <- summary(lmedata)

  #fixed effect
  fixed_effect <- lmedata_summary$coefficients

   #random effect
  random_effect <- data.frame(lme4::VarCorr(lmedata))

if(show.effect){
  ranef = ranef(lmedata)
  fixef = fixef(lmedata)
  # prediction for each categories
  # fixef(lmedata) + ranef(lmedata)$operator
  coef = coef(lmedata)
}else{
  ranef ="If you want to see the effect, show.effect = TRUE results."
  fixef="If you want to see the effect, show.effect = TRUE results."
  coef="If you want to see the effect, show.effect = TRUE results."
}


  if(form=="glmer"){
    #ICC
    # pisqaure/3
    icc =  random_effect |>
      dplyr::mutate(Sum = sum(vcov) + ((pi^2)/3),
                    ICC = (vcov/Sum),
                    ICC_ratio = paste0(round((vcov/Sum)*100, 2),"%"),
                    ICC_rank = rank(dplyr::desc(ICC))
      ) |>
      dplyr::select(1:2,7,8,9)

  }else if(form =="lmer"){
  #ICC
  icc =  random_effect |>
    dplyr::mutate(Sum = sum(vcov),
                  ICC = (vcov/Sum),
                  ICC_ratio = paste0(round((vcov/Sum)*100, 2),"%"),
                  ICC_rank = rank(dplyr::desc(ICC))
    ) |>
    dplyr::select(1:2,7,8,9)

  }else if(form == "poisson"){
    icc =  random_effect |>
      dplyr::mutate(Sum = sum(vcov) + 1,
                    ICC = (vcov/Sum),
                    ICC_ratio = paste0(round((vcov/Sum)*100, 2),"%"),
                    ICC_rank = rank(dplyr::desc(ICC))
      ) |>
      dplyr::select(1:2,7,8,9)
  }else if(form == "pois"){
    icc =  random_effect |>
      dplyr::mutate(Sum = sum(vcov) + 1,
                    ICC = (vcov/Sum),
                    ICC_ratio = paste0(round((vcov/Sum)*100, 2),"%"),
                    ICC_rank = rank(dplyr::desc(ICC))
      ) |>
      dplyr::select(1:2,7,8,9)
  }else if(form == "logit"){
    icc =  random_effect |>
      dplyr::mutate(Sum = sum(vcov) + ((pi^2)/3),
                    ICC = (vcov/Sum),
                    ICC_ratio = paste0(round((vcov/Sum)*100, 2),"%"),
                    ICC_rank = rank(dplyr::desc(ICC))
      ) |>
      dplyr::select(1:2,7,8,9)
  }else if(form == "dich"){
    icc =  random_effect |>
      dplyr::mutate(Sum = sum(vcov) + ((pi^2)/3),
                    ICC = (vcov/Sum),
                    ICC_ratio = paste0(round((vcov/Sum)*100, 2),"%"),
                    ICC_rank = rank(dplyr::desc(ICC))
      ) |>
      dplyr::select(1:2,7,8,9)
  }



  # test the variance parameter
  # APA style


  #Significance of random effects
  # H0: Var(random effect) (i.e., σ2)= 0
  # Ha: Var(random effect) (i.e., σ2) > 0
  if(ranef_sig){
  ranef_sig = RLRsim::exactRLRT(lmedata)
  }else{
    ranef_sig = "ranef_sig = TRUE -> perform random effect test "
  }

#confidence interval
if(show.ci){
  CI = confint(lmedata, oldNames = FALSE)
}else{
  CI = "If you want to see 95%CI, set show.ci = TRUE"
}

  #apa output
  if(apa){
    apa = lmedata |>
            JWileymisc::modelTest() |>
            JWileymisc::APAStyler()

  }else{
    apa = "If you want to see APA style result, set apa = TRUE"
  }

  # bind_cols(AIC(lmedata), BIC(lmedata))
  # p-value based on lmerTest
  anova_test = anova(lmedata)




  #model fit
  if(fit_more){
  fit = lmedata |> JWileymisc::modelPerformance()
  }else{
    fit = dplyr::bind_cols(AIC = AIC(lmedata), BIC= BIC(lmedata))
  }

  # result
  basic = list(formula = formula,
             Fixed_effect = fixed_effect,
             Random_effect = random_effect,
             ICC = icc,
             FIT = fit,
             APA = apa
             )



  res = list(formula = formula,
             Fixed_effect = fixed_effect,
             Random_effect = random_effect,
             ICC = icc,
             ranef_sig  = ranef_sig,
             FIT = fit,
             fixef = fixef,
             ranef = ranef,
             coef = coef,
             ConfidenceInterval_95 = CI,
             Satterthwaite_method = anova_test,
             APA = apa  )

  #full data view
  full = list(
    summary = lmedata_summary,
    formula = formula,
             Fixed_effect = fixed_effect,
             Random_effect = random_effect,
             ICC = icc,
             # ICC_glmer = icc_glmer,
             ranef_sig  = ranef_sig,
             FIT = fit,
             fixef = fixef,
             ranef = ranef,
             coef = coef,
             ConfidenceInterval_95 = CI,
             Satterthwaite_method = anova_test,
             APA = apa  )
#select result
  switch(type,
        basic = basic,
        all = res,
        full = full, #full data
        summary = lmedata_summary, #lmer summary
        Fixed_effect = fixed_effect,
        Random_effect = random_effect,
        ICC = icc,
        ICC_glmer = icc_glmer,
        ranef_sig = ranef_sig,
        CI = CI,
        FIT = fit,
        ranef = ranef,
        fixef = fixef,
        anova = anova_test,
        APA = apa,
        formula =   formula
        )
}
