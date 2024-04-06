#' Frequency Analysis Functions
#'
#' @param data  data.frame
#' @param remove Select variables to exclude from analysis
#' @param digits round
#'
#' @return  Publication frequency table
#' @export
#'
#' @examples
#'
#' \dontrun{
#' ## Identifying variables
#'  mtcars %>% as_trt("cyl","am","vs","gear")%>% tibble()
#'
#'  mtcars %>% as_trt("cyl","am","vs","gear")%>% table_df()
#'
#'  iris %>% as_trt("Species")%>% table_df()
#' }
#'
#'
table_df <- function(data, remove = NULL, digits = 4) {
  # 데이터 프레임에서 Factor 및 chr 변수 추출
  factor_vars <- sapply(data, is.factor)
  chr_vars <- sapply(data, is.character)

  # 제외할 변수 처리
  if (!is.null(remove)) {
    factor_vars[remove] <- FALSE
    chr_vars[remove] <- FALSE
  }

  # Factor 및 chr 변수 목록 추출
  factor_var_names <- names(data)[factor_vars]
  chr_var_names <- names(data)[chr_vars]

  # Factor 변수의 빈도표 및 비율 생성
  factor_freq <- lapply(factor_var_names, function(var) {
    table_data <- table(data[[var]])
    freq_table <- data.frame(Term = rep(var, length(table_data)), Level = names(table_data), Freq = as.vector(table_data))
    freq_table$`Prop(%)` <- round(freq_table$Freq / sum(freq_table$Freq), digits)*100  # 비율 추가
    return(freq_table)
  })

  # chr 변수의 빈도표 및 비율 생성
  chr_freq <- lapply(chr_var_names, function(var) {
    table_data <- table(data[[var]])
    freq_table <- data.frame(Term = rep(var, length(table_data)), Level = names(table_data), Freq = as.vector(table_data))
    freq_table$`Prop(%)` <- round(freq_table$Freq / sum(freq_table$Freq), digits)*100  # 비율 추가
    return(freq_table)
  })
  # Factor 및 chr 변수의 빈도표를 리스트로 결합
  result <- c(factor_freq, chr_freq)

  # 결과 반환
  cat("\n Data frequency analysis results\n\n")
  return(do.call(rbind, result))
}
