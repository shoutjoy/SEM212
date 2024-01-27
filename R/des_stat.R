#' Tmean and sd
#'
#' @param x data.frame
#' @param digit default 3
#' @param type output  tpye
#'
#' @return reuren mean(sd) type
#' @export
#'
#' @examples
#' \dontrun{
#' df = data.frame(
#' subject = c(1:6),
#' mon1 = c(45, 42, 36, 39, 51, 44),
#' mon3 = c(50, 42, 41, 35, 55, 49),
#' mon6 = c(55, 45, 43, 40, 59, 56))
#'
#' df[,2:4] %>% des_stat()
#' df[,2:4] %>% des_stat(type="all")
#' df[,2:4] %>% des_stat(type="row")
#' df[,2:4] %>% des_stat(type="col")
#'
#'
#' }
des_stat <- function(x, digit =3, tpye = "col"){
  x <- x %>% as.data.frame()
  s_col <- dplyr::summarise(x,
                            dplyr::across( .cols=c(1:ncol(x)),
                                           .fns = function(x){
                                             stringr::str_c(round(mean(x, na.rm = T),
                                                                  digit),"(", round(sd(x),digit),")" )}
                            )) %>% t() %>% as.data.frame()
  colnames(s_col)="Mean(sd)"

  s_row<- s_col %>% t() %>% data.frame()

  res= list(s_col, s_row)

  switch(tpye,
         all= res,
         row = s_row,
         col= s_col
         )
}
