
#' Tsimple_bar
#'
#' @param data  table
#' @param add_y  max 30
#' @param color color=1:16
#' @param vjust vjust =10
#' @param cex cex=1
#' @param line  T/F
#' @param main main sistitle
#' @param xlab xlab
#' @param ylab ylab
#'
#' @return bar graph
#' @export
#'
#'@examples
#'\dontrun{
#'table(mtcars$cyl) |> simple_bar(cex=1.5)
#'
#' data(bioChemists, package = "pscl")
#' bioChemists <- bioChemists %>%
#' rename(
#'   Num_Article = art, #articles in last 3 years of PhD
#'    Sex = fem, #coded 1 if female
#'    Married = mar, #coded 1 if married
#'    Num_Kid5 = kid5, #number of childeren under age 6
#'    PhD_Quality = phd, #prestige of PhD program
#'    Num_MentArticle = ment #articles by mentor in last 3 years
#'    )
#'   table(bioChemists$Num_Article) |>
#'       simple_bar(cex=1.5, add_y= 30, vjust=10)
#'
#'}
#'
#'
simple_bar <-function(data,
                      add_y = 10,
                      color = 2:17,
                      vjust = 1,
                      cex = 1,
                      line = FALSE,
                      main = NULL,
                      xlab = "x",
                      ylab = "y"
){
  data |>
    barplot(ylim=c(0, max(data)+ add_y), col= color,
            main= main, xlab=xlab, ylab=ylab)|>
    text(y= data+vjust, label=data, cex=cex)
  if(line){
    par(new=TRUE)
    plot(data, type="o", col="gray30", lty=2)
  }
}
