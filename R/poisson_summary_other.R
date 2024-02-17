#' TPoisson_summary_function is poisson regression summary
#'
#' @param my_model_estimation poisson result
#' @param digit digit default 3
#'
#' @return tabular data for use in a paper
#' @export
#'
#' @examples
#' \dontrun{
#' p <- read.csv("https://stats.idre.ucla.edu/stat/data/poisson_sim.csv")
#' p <- within(p, {
#'   prog <- factor(prog, levels=1:3,
#'                    labels=c("General", "Academic", "Vocational"))
#'                  id <- factor(id) })
#' m1 <- glm(num_awards ~ prog + math, family="poisson", data=p)
#' summary(m1)
#' Poisson_summary_function(m1)
#' }
#'
Poisson_summary_function = function(my_model_estimation,
                                    digit=3){

  if (missing(my_model_estimation)) stop("Please provide the data object as an argument.")
  if (requireNamespace("tidyverse")) library(tidyverse)
  if (requireNamespace("broom")) library(broom)
  if (requireNamespace("modelr")) library(modelr)

  options(knitr.kable.NA = '')

  mycoefs = tidy(my_model_estimation) %>%
    mutate(
      est2 = format(round(estimate, digit),digit),
      se2 = format(round(std.error, digit),digit),
      mystars = cut(p.value,c(0,0.001,0.01,0.05,0.10,1),
                  c("***","**","*","+",""),right=F),
      report = str_c(" ",est2,mystars,"(",se2,")",sep=""),
      my_irr = format(round(exp(estimate), digit),digit)
    ) %>% select(term,report,my_irr)
  myGOF = glance(my_model_estimation) %>%
    mutate(
      LL_CHI2 = null.deviance-deviance,
      DF_CHI2 = df.null-df.residual,
      p.value = 1 - pchisq(LL_CHI2,DF_CHI2),
      McFaddenR2 = format(round((null.deviance-deviance)/null.deviance,
                              digit),digit),
      LL_CHI2 = format(round(LL_CHI2, digit),digit),
      mystars = cut(p.value,c(0,0.001,0.01,0.05,0.10,1),
                  c("***","**","*","+",""),right=F),
      my_CHI2 = str_c(LL_CHI2,mystars),
      LL_CHI2_df = str_c("(",DF_CHI2,")"),
      AIC = format(round(AIC(my_model_estimation),digit),digit),
      BIC = format(round(BIC(my_model_estimation),digit),digit)
    ) %>% select(McFaddenR2, my_CHI2, LL_CHI2_df, AIC, BIC) %>%
    gather(key=term,value=report) %>% mutate(my_irr="")
  # sortid를 만든 이유는 여러 모형들의 결과를 하나의 표에 합칠 경우
  mytable = bind_rows(mycoefs,myGOF) %>%
    mutate(sortid = row_number()) %>%
    select(sortid,term,report,my_irr) %>%
    # mutate_at(vars(2:4),funs(as.character(.)))
    mutate_at(vars(2:4), list(as.character))

  return(data.frame(mytable))
}



#' summary_pois is poisson regression summary
#'
#' @param my_model_estimation poisson result
#' @param digit digit default 3
#'
#' @return tabular data for use in a paper
#' @export
#'
#' @examples
#' \dontrun{
#' p <- read.csv("https://stats.idre.ucla.edu/stat/data/poisson_sim.csv")
#' p <- within(p, {
#'   prog <- factor(prog, levels=1:3,
#'                    labels=c("General", "Academic", "Vocational"))
#'                  id <- factor(id) })
#' m1 <- glm(num_awards ~ prog + math, family="poisson", data=p)
#' summary(m1)
#' Poisson_summary_function(m1)
#' }
#'
summary_pois = function(my_model_estimation,
                        digit=3){

  if (missing(my_model_estimation)) stop("Please provide the data object as an argument.")
  if (requireNamespace("tidyverse")) library(tidyverse)
  if (requireNamespace("broom")) library(broom)
  if (requireNamespace("modelr")) library(modelr)

  options(knitr.kable.NA = '')

  mycoefs = tidy(my_model_estimation) %>%
    mutate(
      est2 = format(round(estimate, digit),digit),
      se2 = format(round(std.error, digit),digit),
      mystars = cut(p.value,c(0,0.001,0.01,0.05,0.10,1),
                    c("***","**","*","+",""),right=F),
      report = str_c(" ",est2,mystars,"(",se2,")",sep=""),
      my_irr = format(round(exp(estimate), digit),digit)
    ) %>% select(term,report,my_irr)
  myGOF = glance(my_model_estimation) %>%
    mutate(
      LL_CHI2 = null.deviance-deviance,
      DF_CHI2 = df.null-df.residual,
      p.value = 1 - pchisq(LL_CHI2,DF_CHI2),
      McFaddenR2 = format(round((null.deviance-deviance)/null.deviance,
                                digit),digit),
      LL_CHI2 = format(round(LL_CHI2, digit),digit),
      mystars = cut(p.value,c(0,0.001,0.01,0.05,0.10,1),
                    c("***","**","*","+",""),right=F),
      my_CHI2 = str_c(LL_CHI2,mystars),
      LL_CHI2_df = str_c("(",DF_CHI2,")"),
      AIC = format(round(AIC(my_model_estimation),digit),digit),
      BIC = format(round(BIC(my_model_estimation),digit),digit)
    ) %>% select(McFaddenR2, my_CHI2, LL_CHI2_df, AIC, BIC) %>%
    gather(key=term,value=report) %>% mutate(my_irr="")
  # sortid를 만든 이유는 여러 모형들의 결과를 하나의 표에 합칠 경우
  mytable = bind_rows(mycoefs,myGOF) %>%
    mutate(sortid = row_number()) %>%
    select(sortid,term,report,my_irr) %>%
    # mutate_at(vars(2:4),funs(as.character(.)))
    mutate_at(vars(2:4), list(as.character))

  return(data.frame(mytable))
}


