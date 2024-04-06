
#' Contrast test with mean, standard deviation, and number of samples
#'
#' @param M means M = c(26.66, 19.74, 15.10)
#' @param sd sd  sd = c(4.51, 1.45, 2.56)
#' @param n sample  n = c(11, 7, 14)
#' @param ...  contrast c(1, -1,  0), c(1,  0, -1), c(0,  1, -1) ....
#' @param digits default 3
#' @param type res, res_ci
#' @param meanadd include means
#'
#' @return  conrast result
#' @export
#'
#' @examples
#' \dontrun{
#'
#'
#' contrast_msdn(M = c(26.66, 19.74, 15.10),
#'               sd = c(4.51, 1.45, 2.56),
#'               n = c(11, 7, 14),
#'               c(1, -1, 0) )
#'
#' contrast_msdn(M = c(26.66, 19.74, 15.10),
#'               sd = c(4.51, 1.45, 2.56),
#'               n = c(11, 7, 14),
#'               c(1, -1, 0) ,
#'               meanadd = FALSE)
#'
#' contrast_msdn(M = c(26.66, 19.74, 15.10),
#'               sd = c(4.51, 1.45, 2.56),
#'               n = c(11, 7, 14),
#'               c(1, -1, 0) ,
#'              meanadd = FALSE,
#'               type="res_ci")
#'
#'
#' contrast_msdn(M = c(26.66, 19.74, 15.10),
#'               sd = c(4.51, 1.45, 2.56),
#'              n = c(11, 7, 14),
#'               c(1, -1,  0),
#'               c(1,  0, -1),
              #'               c(0,  1, -1)
#' )
#' contrast_msdn(M = c(26.66, 19.74, 15.10),
#'               sd = c(4.51, 1.45, 2.56),
#'               n = c(11, 7, 14),
#'               contr1 = c(1, -1,  0),
#'               contr2= c(1,  0, -1),
#'               contr3= c(0,  1, -1),
#'               meanadd=F
#' )
#' contrast_msdn(M = c(26.66, 19.74, 15.10),
#'               sd = c(4.51, 1.45, 2.56),
#'               n = c(11, 7, 14),
#'              contr1 = c(1, -1,  0),
#'              contr2= c(1,  0, -1),
#'               contr3= c(0,  1, -1),
#'               meanadd=FALSE, type="res_ci"
#' )
#' #error
#' contrast_msdn(M = c(26.66, 19.74, 15.10),
#'               sd = c(4.51, 1.45, 2.56),
#'              n = c(11, 7, 14),
#'               contr = list(c(1, -1,  0),
#'                             c(1,  0, -1),
#'                            c(0,  1, -1)
#'               ))
#'
#'
#' }
contrast_msdn <- function(M, sd, n,
                          ...,
                          digits = 3,
                          type = "res",
                          meanadd=TRUE) {



  # Create an empty dataframe to hold the cumulative results
  result <- data.frame(contributes = character(),

                       cont.mean = numeric(),
                       critical_t = numeric(),
                       p.value = numeric(),
                       sig = character(),
                       low_CI = numeric(),
                       high_CI = numeric(),
                       stringsAsFactors = FALSE)
  contr = list(...)

  for (i in 1:length(contr)) {
    grp <- c(paste0("g", 1:length(M), ":"))

    if(meanadd){
      contratst_var <- paste0(grp, M, "(", contr[[i]], sep = ")", collapse = "/")
    }else{
      contratst_var <- paste0(grp, "(", contr[[i]], sep = ")", collapse = "/")
    }


    # MSW=s_pooled
    var.pooled <- sum((n - 1) * sd^2) / sum(n - 1)
    # em.mean Calculations
    c_mean <- contr[[i]] * M
    cont.mean <- sum(contr[[i]] * M)

    # standard error
    SE <- sqrt(sum(contr[[i]]^2 * var.pooled / n))

    # t-value
    t.stat1 <- round(cont.mean / SE , digits)

    df <- sum(n) - length(n)
    critical_t <- round(abs(qt(0.05 / 2, df)), digits)

    # p-value: Since it is lower.tail, we double it for two-tailed test.
    p.value <- pt(q = t.stat1, df = df, lower.tail = FALSE) * 2

    # confidence interval
    CI <- c(cont.mean - critical_t * SE, cont.mean + critical_t * SE)



    # Adding results to a dataframe
    res <- data.frame(
      contributes = contratst_var,
      cont.mean = cont.mean,
      critical_t = critical_t,
      p.value = p.value,
      sig = ifelse(p.value < 0.001, "***",
                   ifelse(p.value < 0.01,"**",
                          ifelse(p.value<0.05,"*", ""))),
      low_CI = CI[1],
      high_CI = CI[2],
      stringsAsFactors = FALSE)

    # Accumulate results
    result <- rbind(result, res)
  }

  res_basic <- result %>% dplyr::select(1:5)
  res_ci = result
  switch(type,
         res = res_basic,
         res_ci = result)


}
