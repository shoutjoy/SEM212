
#' REversse codings
#'
#' @param vec input data
#' @param n   likert max+1
#' @param check  View the original source and reverse coded results
#'
#' @return reverse coding data
#' @export
#'
#' @examples
#' \dontrun{
#' ## error  :Error message output when data frames are entered
#' rev_coding(mtcars)
#'
#' #reverse coding
#' rev_coding(mtcars$gear)
#'
#' ## View the original source and decoded results
#' rev_coding(mtcars$gear, check=TRUE)
#'
#' ###If the Likert scale is wrong, the highest scale is 5, but the highest scale is 8.
#' rev_coding(mtcars$cyl)
#' }
rev_coding = function(vec,
                      n = 6,
                      check = FALSE){
  if(is.vector(vec)){
    if(max(vec) < n){
      vec_r = n - vec
    }else{
      vec_r = "Please check the value again. It does not match the value to be decoded. Please check the scale again. For example, enter n=6 for 1~5 Likert and n=8 for 1~7 Likert"
    }
  }else{
    vec_r = "You can only input vectors for inverse coding. If you have entered a data frame, please find the variable and enter it correctly."
  }
  if(check){
    res = data.frame(input= vec, direct="->",reverse= vec_r)
  }else{
    res = vec_r
  }
  res
}




#' REversse codings rc
#'
#' @param vec input data
#' @param n   likert max+1
#' @param check  View the original source and reverse coded results
#'
#' @return reverse coding data
#' @export
#'
#' @examples
#' \dontrun{
#' ## error  :Error message output when data frames are entered
#' rev_coding(mtcars)
#'
#' #reverse coding
#' rev_coding(mtcars$gear)
#'
#' ## View the original source and decoded results
#' rev_coding(mtcars$gear, check=TRUE)
#'
#' ###If the Likert scale is wrong, the highest scale is 5, but the highest scale is 8.
#' rev_coding(mtcars$cyl)
#' }
rc = function(vec,
                      n = 6,
                      check = FALSE){
  if(is.vector(vec)){
    if(max(vec) < n){
      vec_r = n - vec
    }else{
      vec_r = "Please check the value again. It does not match the value to be decoded. Please check the scale again. For example, enter n=6 for 1~5 Likert and n=8 for 1~7 Likert"
    }
  }else{
    vec_r = "You can only input vectors for inverse coding. If you have entered a data frame, please find the variable and enter it correctly."
  }
  if(check){
    res = data.frame(input= vec, direct="->",reverse= vec_r)
  }else{
    res = vec_r
  }
  res
}


