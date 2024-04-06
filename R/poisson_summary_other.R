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


  mycoefs = broom::tidy(my_model_estimation) %>%
    mutate(
      est2 = format(round(estimate, digit),digit),
      se2 = format(round(std.error, digit),digit),
      mystars = cut(p.value,c(0,0.001,0.01,0.05,0.10,1),
                    c("***","**","*","+",""), right = F),
      report = str_c(" ", est2, mystars,"(",se2,")", sep=""),
      exp_est = format(round(exp(estimate), digit), digit)
    ) %>% dplyr::select( term, report, exp_est)

  myGOF = glance(my_model_estimation) %>%
    mutate(
      `--------`= "---------",
      DF_null = df.null,
      DF_residual = df.residual,
      LL_CHI2 = null.deviance - deviance,
      DF_CHI2 = df.null-df.residual,
      p.value = 1 - pchisq(LL_CHI2, DF_CHI2),
      McFaddenR2 = format(round((null.deviance-deviance)/null.deviance,
                                digit),digit),
      LL_CHI2 = format(round(LL_CHI2, digit), digit),
      mystars = cut(p.value,c(0,0.001,0.01,0.05,0.10,1),
                    c("***","**","*","+",""),right=F),
      my_CHI2 = str_c(LL_CHI2, mystars),
      LL_CHI2_df = str_c("(",DF_CHI2,")"),
      AIC = format(round(AIC(my_model_estimation),digit),digit),
      BIC = format(round(BIC(my_model_estimation),digit),digit)
    ) %>% dplyr::select( `--------`,
                         DF_null,
                         DF_residual,
                         McFaddenR2,
                         my_CHI2,
                         LL_CHI2_df,
                         AIC,
                         BIC) %>%
    gather(key = term, value = report) %>%
    mutate(exp_est="")
  # sortid를 만든 이유는 여러 모형들의 결과를 하나의 표에 합칠 경우
  mytable = bind_rows(mycoefs,myGOF) %>%
              mutate(sortid = row_number()) %>%
              dplyr::select(sortid,
                            term,
                            report,
                            exp_est) %>%
    # mutate_at(vars(2:4),funs(as.character(.)))
              mutate_at(vars(2:4), list(as.character))

  mytable|>data.frame()

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

  var0 = my_model_estimation$data[[as.character(my_model_estimation$formula[2])]]|>var()
  mean0 = my_model_estimation$data[[as.character(my_model_estimation$formula[2])]]|>mean()

  my_model_summary =  summary( my_model_estimation)

  #function check

  mycoefs = broom::tidy(my_model_estimation) %>%
    mutate(
      est2 = format(round(estimate, digit),digit),
      se2 = format(round(std.error, digit),digit),
      mystars = cut(p.value,c(0,0.001,0.01,0.05,0.10,1),
                    c("***","**","*","+",""), right = F),
      report = str_c(" ", est2,"(",se2,")", mystars, sep=""),
      exp_est = format(round(exp(estimate), digit), digit)
    ) %>% dplyr::select( term, report, exp_est)

  myGOF = glance(my_model_estimation) %>%
    mutate(
      `----------`= "--------------",
      LL_CHI2 = null.deviance - deviance,
      DF_CHI2 = df.null- df.residual,
      p.value = 1 - pchisq(LL_CHI2, DF_CHI2),
      McFaddenR2 = format(round((null.deviance-deviance)/null.deviance,
                                digit),digit),
      LL_CHI2 = format(round(LL_CHI2, digit), digit),
      mystars = cut(p.value,c(0,0.001,0.01,0.05,0.10,1),
                    c("***","**","*","+",""),right=F),
      my_CHI2 = str_c(LL_CHI2, mystars),
      LL_CHI2_df = str_c("(",DF_CHI2,")"),
      AIC = format(round(AIC(my_model_estimation),digit),digit),
      BIC = format(round(BIC(my_model_estimation),digit),digit),
      `----------------`= "---------------",
      null_deviance_df = paste0(round(null.deviance,2), "(", df.null, ")"),
      resi_deviance_df = paste0(round(deviance,2), "(", df.residual, ")"),
      Dispersion =  round(my_model_summary$dispersion, digit),

      Dispersion_chcek = ifelse(my_model_estimation$family$family=="quasi",
                       round(var0 /mean0^2, digit),
                       ifelse(my_model_estimation$family$family=="quasipoisson"|
                              my_model_estimation$family$family == "poisson",
                       round(var0 /mean0, digit) ))
          ) %>% dplyr::select(  `----------`,
                          McFaddenR2,
                          my_CHI2,
                          LL_CHI2_df,
                          AIC,
                          BIC,
                          `----------------`,
                          null_deviance_df,
                          resi_deviance_df,
                          Dispersion,
                          Dispersion_chcek) %>%
    gather(key = term, value = report) %>%
    mutate(exp_est="")
  # sortid를 만든 이유는 여러 모형들의 결과를 하나의 표에 합칠 경우
  mytable=bind_rows(mycoefs,myGOF) %>%
    mutate(sortid = row_number()) %>%
    dplyr::select(sortid,
                  term,
                  report,
                  exp_est) %>%
    # mutate_at(vars(2:4),funs(as.character(.)))
    mutate_at(vars(2:4), list(as.character))
  mytable|>data.frame()
}





#' bind_pois_df multiple model
#'
#' @param ... model name
#' @param term_name NA variabel first  model
#' @param n n and range is possiblle
#'
#' @return  compare table
#' @export
#'
#' @examples
#' \dontrun{
#' set.seed(20240217)
#' dat <- data.frame(
#'   exposure = rpois(100, lambda = 10),  # 노출량 (인구, 시간 등)
#'   cases = rpois(100, lambda = 2),      # 발생빈도 (사건 수)
#'   pop = rpois(100, lambda = 1000),     # 지역별 인구수
#'   hospital = rpois(100, lambda = 5)    # 지역별 병원수
#' )
#' dat$logpop <- log(dat$pop)
#'
#' model1a <- glm(cases ~ exposure + hospital, data = dat, family = poisson)
#' model2a <- glm(cases ~ exposure + hospital + pop , data = dat, family = poisson)
#' model3a <- glm(cases ~ exposure + hospital + offset(logpop), data = dat,
#'                 family = poisson)
#' ###comparison model
#' summary_pois(model1a)
#' summary_pois(model2a)
#' summary_pois(model3a)
#' ## all
#' bind_pois(model1a, model2a, model3a)
#'
#' ##  Options that are set to prevent NA from shifting position
#' when there are no variables in the first model
#' bind_pois(model1a, model2a, model3a, term_name=c("pop"),n = 4)
#' ## additional test
#' bind_pois(model1a, model2a, model3a, term_name=c("pop","pop1"),n = 4:5)
#'
#' }
#'
bind_pois <- function(...) {
  # Combine multiple models by joining their summary results
  # Usage: bind_pois(model1, model2, model3, ...)
  options(knitr.kable.NA = '')
  # Convert arguments to a list of models
  model_list <- list(...)

  if(length(model_list) == 1){
    result <- summary_pois(model_list[[1]])
    return(result)
  }else{

    # Initialize the result with the first model's summary
    result <- summary_pois(model_list[[1]])

    # Loop through the remaining models and join their summaries
    for (i in 2:length(model_list)) {
      result <- full_join(result,
                          summary_pois(model_list[[i]]), by = "term")
      #
    }

    result <- dplyr::arrange(result,
                             dplyr::select(result, ncol(result)-2))
    #            dplyr::select(term, starts_with("report"))

    # # Set column names based on model names
    result<- result %>% dplyr::select(term, starts_with("report"))
    colnames(result) <- c("term",
                          paste0("model_", 1:(length(model_list) )))

    return(result)
  }
}




