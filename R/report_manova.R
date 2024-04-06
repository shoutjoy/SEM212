
#' Functions to report MANOVA results
#'
#' @param model_manova manova result
#' @param type default res, and all, summary_aov, Pillai, Wilks, Hotelling_Lawley, Roy, Multivariate_Tests
#' @param digits default 2
#'
#' @return report MANOVA table
#' @export
#'
#'
#' @examples
#' \dontrun{
#'
#' Skulls = heplots::data(Skulls)
#'
#' # execute
#' manova(cbind(mb,bh,bl,nh)~ epoch, data=Skulls) %>%report_manova()
#'
#' manova(cbind(mb,bh,bl,nh)~ epoch, data=Skulls) %>%report_manova(type="all")
#'
#' manova(cbind(mb,bh,bl,nh)~ epoch, data=Skulls) %>%report_manova(type="summmary_aov")
#'
#' manova(cbind(mb,bh,bl,nh)~ epoch, data=Skulls) %>%report_manova(type="Pillai")
#'
#' manova(cbind(mb,bh,bl,nh)~ epoch, data=Skulls) %>%report_manova(type="Wilks")
#'
#' manova(cbind(mb,bh,bl,nh)~ epoch, data=Skulls) %>%report_manova(type="Hotelling_Lawley")
#'
#' manova(cbind(mb,bh,bl,nh)~ epoch, data=Skulls) %>%report_manova(type="Roy")
#'
#' manova(cbind(mb,bh,bl,nh)~ epoch, data=Skulls) %>%report_manova(type="Multivariate_Tests")
#'
#'
#'
## Example on producing plastic film from Krzanowski (1998, p. 381)

#' Y <- cbind.data.frame(tear=c(6.5, 6.2, 5.8, 6.5, 6.5, 6.9, 7.2, 6.9, 6.1, 6.3,
#'                              6.7, 6.6, 7.2, 7.1, 6.8, 7.1, 7.0, 7.2, 7.5, 7.6),
#'                       gloss=c(9.5, 9.9, 9.6, 9.6, 9.2, 9.1, 10.0, 9.9, 9.5, 9.4,
#'           9.1, 9.3, 8.3, 8.4, 8.5, 9.2, 8.8, 9.7, 10.1, 9.2),
#'           opacity=c(4.4, 6.4, 3.0, 4.1, 0.8, 5.7, 2.0, 3.9, 1.9, 5.7,
#'                     2.8, 4.1, 3.8, 1.6, 3.4, 8.4, 5.2, 6.9, 2.7, 1.9),
#'           rate = gl(2,10, labels = c("Low", "High")),
#'           additive = gl(2, 5, length = 20, labels = c("Low", "High")) )
#'
#' manova(cbind(tear, gloss, opacity) ~ rate * additive, data=Y)%>%report_manova()
#'
#' manova(cbind(tear, gloss, opacity) ~ rate * additive, data=Y)%>%report_manova(type="all")
#' manova(cbind(tear, gloss, opacity) ~ rate * additive, data=Y)%>%
#'                            report_manova(type="summmary_aov")
#' manova(cbind(tear, gloss, opacity) ~ rate * additive, data=Y) %>%
#'   report_manova("Pillai")
#' manova(cbind(tear, gloss, opacity) ~ rate * additive, data=Y)%>%
#'   report_manova("Wilks")
#' manova(cbind(tear, gloss, opacity) ~ rate * additive, data=Y)%>%
#'   report_manova("Hotelling_Lawley")
#' manova(cbind(tear, gloss, opacity) ~ rate * additive, data=Y)%>%
#'   report_manova("Roy")
#' manova(cbind(tear, gloss, opacity) ~ rate * additive, data=Y)%>%
#'   report_manova("Multivariate_Tests")
#'
#' }
#'
#'
report_manova <- function(model_manova,
                          type="res",
                          digits = 2) {
  manovares = model_manova
  model_summary = summary.aov(model_manova)
  # Extracting relevant information from the model summary
  type2 = manovares %>%summary()
  #####

  var = c(manovares$residuals %>%colnames())
  # combinde data.frame
  sqtest = do.call(rbind,
                   lapply(model_summary, function(x) {
                     data.frame(
                       name = trimws(rownames(x)),
                       #  levels = c(manovares$residuals %>%colnames()),
                       Df = x$Df,
                       Sum_Sq = x$"Sum Sq",
                       Mean_Sq = x$"Mean Sq",
                       F_value = x$"F value",
                       Pr_F = x$"Pr(>F)" )
                   }))%>% p_mark_sig("Pr_F")%>%
    dplyr::filter(name != "Residuals")

  residuals = do.call(rbind,
                      lapply(model_summary, function(x) {
                        data.frame(
                          name = trimws(rownames(x)),
                          # levels = c(manovares$residuals %>%colnames()),
                          Df = x$Df,
                          Sum_Sq = x$"Sum Sq",
                          Mean_Sq = x$"Mean Sq",
                          F_value = x$"F value",
                          Pr_F = x$"Pr(>F)" )
                      }))%>% p_mark_sig("Pr_F")%>%
    dplyr::filter(name == "Residuals")

  #Sum of squares and products for the hypothesis
  iv = as.character(manovares$call[[2]][3])
  ## data combine
  sosh = cbind(sqtest[,c(1,2, 3)],
               residuals[,c(3)],
               sqtest[,c(5, 7)])%>%
    `colnames<-`(c("dv","Df", iv ,"Residuals","F","sig" ))%>%
    Round(digits) %>%
    tidyr::unite(F, F, sig, sep="")

  sosh$F = format(sosh$F, justify="left")

  Nrow =  nrow(sosh)
  Levels = manovares$residuals %>%colnames()
  Nlevels =  length(manovares$residuals %>%colnames())
  levels = rep(c(manovares$residuals %>%colnames()), each= Nrow/Nlevels)

  ## data arrange
  ### combine variables
  sosh1  =  cbind(levels, sosh ) %>%
    tidyr::unite(dv, dv, levels, sep="_")
  ### combine variables
  sqtest =  cbind(levels, sqtest ) %>%
    tidyr::unite(dv, name, levels, sep="_")%>%
    Round(digits)

  ##Mutltivariate test  : Pillai, Wilks,  Hotelling-Lawley, Roy
  Pillai0 = summary(manovares, test = "Pillai")
  Wilks0 = summary(manovares, test = "Wilks")
  Hotelling_Lawley0 = summary(manovares, test = "Hotelling-Lawley")
  Roy0 = summary(manovares, test = "Roy")

  ## Select data for combine
  Pillai = summary(manovares, test = "Pillai")$stats%>%
    as.data.frame() %>%
    tibble::rownames_to_column("term") %>%
    dplyr::filter(term != "Residuals")%>%
    rename(test = Pillai)
  Wilks = summary(manovares, test = "Wilks")$stats%>%
    as.data.frame() %>%
    tibble::rownames_to_column("term") %>%
    dplyr::filter(term != "Residuals") %>%
    rename(test = Wilks)
  Hotelling_Lawley = summary(manovares, test = "Hotelling-Lawley")$stats%>%
    as.data.frame() %>%
    tibble::rownames_to_column("term") %>%
    dplyr::filter(term != "Residuals")%>%
    rename(test = `Hotelling-Lawley`)
  Roy = summary(manovares, test = "Roy")$stats %>%
    as.data.frame() %>%
    tibble::rownames_to_column("term") %>%
    dplyr::filter(term != "Residuals")%>%
    rename(test = Roy)

  #####Multivariate_Tests : combine each test
  Multivariate_Tests = rbind(
    Pillai%>% dplyr::slice( nrow(Pillai)),
    Wilks%>% dplyr::slice( nrow(Wilks)),
    Hotelling_Lawley%>% dplyr::slice( nrow(Hotelling_Lawley)),
    Roy %>% dplyr::slice( nrow(Roy))
  )%>% `rownames<-`(c("Pillai", "Wilks", "Hotelling-Lawley", "Roy"))%>%
    dplyr::select(-term) %>%
    tibble::rownames_to_column("Test")%>%
    p_mark_sig("Pr(>F)")%>%
    Round(digits)

  ##result arrange
  res0 = list(
    MANOVA_test = type2,
    SS_hypothesis = sosh1,
    # variable_tests = sqtest,
    Multivariate_Tests = Multivariate_Tests
  )
  #full result data
  res = list(
    MANOVA_test = type2,
    SS_hypothesis = sosh1,
    variable_tests = sqtest,
    Multivariate_Tests = Multivariate_Tests
  )

  ## message
  cat("\nType II MANOVA Tests(by jjstat package): \n\n")

  ## result putpput
  switch(type,
         res = res0,
         all = res,
         summmary_aov = model_summary,
         sumsq = sosh1,
         aov = sqtest,
         Pillai = Pillai0,
         Wilks = Wilks0,
         Hotelling_Lawley = Hotelling_Lawley0,
         Roy = Roy0,
         Multivariate_Tests= Multivariate_Tests,
         levels = Levels
  )

}
