
#' Calculate Shapiro-Wilke test results for all columns
#'
#' @param data data.frame
#' @param digits default 3
#'
#' @return nomalitytest result
#' @export
#'
#' @examples
#' \dontrun{
#' data <- mtcars[,c("mpg","disp","hp","drat","wt","qsec")]
#'
#' ## Shapiro-Wilke Test Results for All Columns
#' shapiro_test_df(data)
#'
#' }
shapiro_test_df <- function(data, digits = 3) {
  # Calculate Shapiro-Wilke test results for all columns
  # Shapiro-Wilke test results for each column
  results <- lapply(data, shapiro.test)
  # extract variable name
  variable = names(data)
  #list to data.frame
  df = data.table::rbindlist(results)[,1:2]
  # 2nd method
  # df = (do.call(rbind, results) %>% data.frame())[,1:2]

  #bind data
  res = cbind(variable, df) %>% data.frame() %>%
    dplyr::mutate_if(is.numeric, round, digits)

  res = res %>% transform(hypotheses= ifelse(p.value >0.05, "Normality * ", "reject"))
  #
  return(res)
}





#' Custom function to perform Shapiro-Wilk test on all columns of a data frame
#'
#' @param data data.frame
#' @param digits default 3
#'
#' @return nomalitytest result
#' @export
#'
#' @examples
#' \dontrun{
#' data <- mtcars[,c("mpg","disp","hp","drat","wt","qsec")]
#'
#' ## Shapiro-Wilke Test Results for All Columns
#' shapiro_test_all(data)
#'
#' }
#'
shapiro_test_all<- function(data, digits=3) {
  # Initialize an empty data frame to store results
  result_df <- data.frame(Column = character(0),
                          W = numeric(0),
                          p_value = numeric(0),
                          stringsAsFactors = FALSE)

  # Loop through each column in the data frame
  for (col in colnames(data)) {
    # Perform Shapiro-Wilk test
    test_result <- shapiro.test(data[[col]])

    # Extract test statistics
    W <- test_result$statistic
    p_value <- test_result$p.value

    # Append results to the data frame
    result_df <- rbind(result_df,
                       data.frame(
                         variable = col,
                         W = W,
                         p.value = p_value))%>%
      dplyr::mutate_if(is.numeric, round, digits)
  }


  result_df<- result_df%>%
    transform(hypotheses= ifelse(p.value >0.05,
                                 "Normality * ", "reject"))
  result_df
}


