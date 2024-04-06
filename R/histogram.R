#' histogram
#'
#' @param data vector
#' @param color color
#'
#' @return plot
#' @export
#'
#' @examples
#' \dontrun{
#' set.seed(123)
#' data <- rnorm(100)
#' histogram(data)
#' }
#'
histogram = function(data, color="gray60"){
  # library(tidyverse)
  Data = data.frame(data)
  Data = Data %>%
    tibble::rowid_to_column() %>%
    tibble::tibble()


  ggplot2::ggplot(Data, ggplot2::aes(x = as.numeric(data))) +
    geom_histogram(aes(y = ..density..),
                   alpha = 0.4,
                   binwidth = 0.2,
                   color = "black",
                   fill = color) +
    geom_density(color = "red", linewidth = 1) +
    labs(title = "Histogram with Overlay Density",
         x = "Values",
         y = "Density") +
    theme_minimal()
}



#' histogram
#'
#' @param data vector
#' @param color fill color
#' @param linecolor line color
#'
#' @return graph histogram
#' @export
#'
#' @examples
#' #' \dontrun{
#' set.seed(123)
#' data <- rnorm(100)
#' hist2(data)
#' }
hist2 = function(data,
                 color = "lightblue",
                 linecolor = "red"){
  data <- as.numeric(data)
  # 히스토그램 그리기
  hist(data, probability = TRUE,
       col = color,
       main = "Histogram with Overlay Density")
  # 밀도 그래프 그리기
  lines(density(data), col = linecolor, lwd = 2)
  # 범례 추가
  legend("topright", legend = c("Histogram", "Density"),
         fill = c(color, linecolor))
}

