
#' MeasurementInvariance
#'
#' @param model lavaan mode syntax
#' @param data data.frame
#' @param grp setting compare group name
#'
#' @return Measurement invariance Testing
#' @export
#'

MeasurementInvariance <- function(model, data, grp=""){
  library(lavaan)
  library(broom)
  suppressPackageStartupMessages(library(broom))
  library(dplyr)
  library(knitr)
  options(knitr.kable.NA = '')

  # Form equivalence assumption
  Configural <- sem(model, data=data , group = grp )
  # Factor equivalence
  Loadings <- sem(model, data=data ,
                  group = grp,group.equal=c("loadings"))

  #Equivalence of intercept means
  Intercepts <- sem(model, data=data ,
                    group = grp,group.equal=c("loadings","intercepts","means"))

  # Residual equivalence
  Residuals <- sem(model, data=data ,
                   group = grp,group.equal=c("loadings", "residuals",
                                             "intercepts","means"))
  #Variance, covariance error equality
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
