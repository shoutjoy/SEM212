
#' Repeat t.test to create a table
#'
#' @param data data. fram
#' @param dv_var ddpendent variable
#' @param iv_vars Independent variables
#' @param grp_mean mean each level
#' @param unite_t tvalue+sig
#' @param unite_p p_vakye+sig
#' @param digits digits 2
#' @param var.equal variance equal -student ttest
#' @param sig star
#'
#' @return  t.test table
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' ttest_df(data = mtcars, dv_var = "mpg", iv_vars = c("am", "vs"))
#'
#' ttest_df(data = mtcars, dv_var = "mpg", iv_vars = c("am", "vs"), sig=T)
#'
#'
#' ttest_df(data = mtcars, dv_var = "mpg", iv_vars = c("am", "vs"), sig=T, unite_t=TRUE)
#'
#' ttest_df(data = mtcars, dv_var = "mpg", iv_vars = c("am", "vs"), sig=T, unite_p=TRUE)
#'
#' ttest_df(data = mtcars, dv_var = "mpg", iv_vars = c("am", "vs"), grp_mean=FALSE)
#'
#'
#' }
#'
#'
ttest_df <- function(data,
                     dv_var = NULL,
                     iv_vars = NULL,
                     grp_mean = TRUE,
                     unite_t = FALSE,
                     unite_p = FALSE,
                     digits = 2,
                     var.equal= TRUE,
                     sig = FALSE
) {
  # data: data.frame
  # dv_var: Dependent variable column name (string)
  # iv_vars: List of independent variable column names (string vector)
  if (!is.data.frame(data)) {
    stop("you need input data.frame")
  }

  if (is.null(dv_var) | is.null(iv_vars)) {
  }

  cat("\nt.test_df Result\n\n")

  data = as_trt(data, iv_vars)  #factor treatment


  # Initialize a data frame to store the results
  result_df <- data.frame(Indv_Variable = character(0),
                          t_value = numeric(0),
                          p_value = numeric(0))

  # Perform a t-test for each independent variable
  for (iv in iv_vars) {
    # t-test
    ttest_result <- t.test(data[[dv_var]] ~ data[[iv]], data = data, var.equal = var.equal )

    meandata =  jjstat::mysummaryBy(formula(paste(dv_var, "~", iv)),
                                    data = data)[2]

    levels_paste <- paste0(unique(data[[iv]]), collapse =", " )
    levels <- unique(data[[iv]])

    tidy_result <- broom::tidy(ttest_result)

    # t-value and p-value
    t_value <- ttest_result$statistic
    df <- ttest_result$parameter
    p_value <- ttest_result$p.value

    # Add the results to the data frame
    result_df <- rbind(result_df,

                       suppressWarnings({ data.frame(
                         iv = iv,
                         dv = dv_var,
                         levels = levels_paste,   # unite
                         level = levels,
                         Mead_diff = tidy_result$estimate,
                         Mean1 = tidy_result$estimate1,
                         Mean2 = tidy_result$estimate2,
                         Mean = meandata,
                         df = df,
                         t_value = t_value,
                         p_value = p_value)
                       })
    )
  }

  # Adding results to a data frame

  if (grp_mean) {
    result_df <- result_df %>% dplyr::select(-levels, -Mead_diff, -Mean1, -Mean2)

    result_df2 <- result_df %>%
      mutate(p_value = format_number(p_value, n3 = 3, n1 = 5)) %>%
      Round(digits, exclude = "p_value")%>%
      tibble::tibble()

    result_df2$p_value <- as.numeric(result_df2$p_value)

    if (sig) {
      result_df2 <- result_df2 %>% mutate(sig = add_sig(p_value))
    } else {
      result_df2 <- result_df2
    }


  }else{
    result_df  = result_df %>% dplyr::select(-Mean, -level)

    result_df = dplyr::distinct(result_df,
                                iv, dv, levels,Mead_diff,Mean1, Mean2, df, t_value, p_value)
    result_df2 = result_df %>%
      mutate(p_value = format_number(as.vector(p_value), n3 = 3, n1=5)) %>%
      Round(digits, exclude = "p_value") %>%
      tibble::tibble()

    result_df2$p_value = as.numeric(result_df2$p_value)

    if(sig){
      result_df2 = result_df2 %>% mutate(sig = add_sig(p_value))
    }else{
      result_df2 = result_df2
    }
  }
  # UNite
  if(unite_t){
    result_df1 = result_df  %>%
      mutate(sig = ifelse(p_value < 0.001, "***",
                          ifelse(p_value < 0.01, "**",
                                 ifelse(p_value < 0.05, "*", "")))

      )

    result_df1$t_value = round(result_df1$t_value , digits)

    result_df2 = result_df1 %>%
      tidyr::unite(t_value, t_value, sig, sep="") %>%
      Round(digits, exclude = "p_value") %>% tibble::tibble()


  }else if(unite_p){

    result_df1 = result_df %>%
      mutate(sig = ifelse(p_value < 0.001, "***",
                          ifelse(p_value < 0.01, "**",
                                 ifelse(p_value < 0.05, "*", ""))))


    # result_df1$F_value = round(result_df1$F_value , digits)

    result_df2 = result_df1 %>%
      Round(digits, exclude = "p_value") %>%
      mutate(p_value = format_number(p_value, n3=2)) %>%
      tidyr::unite(p_value, p_value, sig, sep="") %>%
      tibble::tibble()

  }
  result_df2
}
