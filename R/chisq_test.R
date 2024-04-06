#' chisq_test 2way
#'
#' @param Obs observation
#' @param correct Yates' continuity correction
#' @param digits 3
#' @param cat chisq.test form
#'
#' @return chisq
#' @export
#'
#' @examples
#' \dontrun{
#'
#' table(mtcars$am, mtcars$vs) %>% chisq_test()
#'
#'
#' table(mtcars$am, mtcars$vs) %>% chisq_test(correct=F)
#'
#' ## compare original chisq.test
#' table(mtcars$am, mtcars$vs) %>% chisq.test()
#'
#'
#' ##Pearson's Chi-squared test with Yates' continuity correction
#' table(mtcars$am, mtcars$vs) %>% chisq_test(correct=T)
#' }
#'
#'

chisqTest <- function(Obs, correct=FALSE, digits = 3, cat=TRUE) {
  Expected <- outer( rowSums(Obs), colSums(Obs)) /sum(Obs) #outer: 성분끼리의 곱

  if(correct){

    cat("\n  Pearson's Chi-squared test with Yates' continuity correction \n")
    chisquare <- sum(  (abs(Obs- Expected) - 0.5  )^2/ Expected  )
  }else{
    cat(" \n  Pearson's Chi-squared test\n")
    chisquare <- sum((Obs- Expected)^2/Expected)  #chi square 계산
  }



  df = (ncol(Obs)-1)*(nrow(Obs)-1) #자유도

  p_value = 1- pchisq(chisquare, df) #p값

  statistic <- cbind(Chisq = chisquare, df, p_value)

  rownames(statistic) = c("statistic")
  statistic<- statistic %>% jjstat::p_mark_sig("p_value") #%>%
  statistic$p_value <- format_number(statistic$p_value) %>%as.numeric()
  #  jjstat::Round(digits = digits)
  #  statistic
  if(cat){
    cat("\n", paste0( "X-squared = ",round(chisquare, digits),
                      ", df = ",df, ", p.value = ",
                      statistic$p_value," ", statistic$sig), "\n\n")
  }else{
    res2 = statistic
    res2
  }

}




#' chisq_test
#'
#' @param x 	a numeric vector or matrix. x and y can also both be factors.
#' @param y a numeric vector; ignored if x is a matrix. If x is a factor,
#'            y should be a factor of the same length.
#' @param correct a logical indicating whether to apply continuity correction when computing
#'                  the test statistic for 2 by 2 tables: one half is subtracted from all ∣O−E∣ differences
#'                  ; however, the correction will not be bigger than the differences themselves.
#'                  No correction is done if simulate.p.value = TRUE.
#' @param p a vector of probabilities of the same length as x.
#'              An error is given if any entry of p is negative.
#' @param rescale.p 	a logical scalar; if TRUE then p is rescaled (if necessary) to sum to 1.
#'                    If rescale.p is FALSE, and p does not sum to 1, an error is given.
#' @param simulate.p.value a logical indicating whether to compute p-values by Monte Carlo simulation
#' @param B an integer specifying the number of replicates used in the Monte Carlo test.
#'
#' @return  A list with class "htest" containing the following components:
#' @export
#'
#' @examples
#' \dontrun{
#'
#' ## From Agresti(2007) p.39
#' M <- as.table(rbind(c(762, 327, 468), c(484, 239, 477)))
#' dimnames(M) <- list(gender = c("F", "M"),
#' party = c("Democrat","Independent", "Republican"))
#' (Xsq <- chisq.test(M))  # Prints test summary
#' Xsq$observed   # observed counts (same as M)
#' Xsq$expected   # expected counts under the nul
#' Xsq$residuals  # Pearson residuals
#' Xsq$stdres     # standardized residuals
#'
#'
#' x <- matrix(c(12, 5, 7, 7), ncol = 2)
#' chisq_test(x)$p.value           # 0.4233
#' chisq_test(x, simulate.p.value = TRUE, B = 10000)$p.value
#' }
chisq_test = function (x, y = NULL, correct = TRUE, p = rep(1/length(x), length(x)),
          rescale.p = FALSE, simulate.p.value = FALSE, B = 2000)
{
  DNAME <- deparse(substitute(x))
  if (is.data.frame(x))
    x <- as.matrix(x)
  if (is.matrix(x)) {
    if (min(dim(x)) == 1L)
      x <- as.vector(x)
  }
  if (!is.matrix(x) && !is.null(y)) {
    if (length(x) != length(y))
      stop("'x' and 'y' must have the same length")
    DNAME2 <- deparse(substitute(y))
    xname <- if (length(DNAME) > 1L || nchar(DNAME, "w") >
                 30)
      ""
    else DNAME
    yname <- if (length(DNAME2) > 1L || nchar(DNAME2, "w") >
                 30)
      ""
    else DNAME2
    OK <- complete.cases(x, y)
    x <- factor(x[OK])
    y <- factor(y[OK])
    if ((nlevels(x) < 2L) || (nlevels(y) < 2L))
      stop("'x' and 'y' must have at least 2 levels")
    x <- table(x, y)
    names(dimnames(x)) <- c(xname, yname)
    DNAME <- paste(paste(DNAME, collapse = "\n"), "and",
                   paste(DNAME2, collapse = "\n"))
  }
  if (any(x < 0) || anyNA(x))
    stop("all entries of 'x' must be nonnegative and finite")
  if ((n <- sum(x)) == 0)
    stop("at least one entry of 'x' must be positive")
  if (simulate.p.value) {
    setMETH <- function() METHOD <<- paste(METHOD, "with simulated p-value\n\t (based on",
                                           B, "replicates)")
    almost.1 <- 1 - 64 * .Machine$double.eps
  }
  if (is.matrix(x)) {
    METHOD <- "Pearson's Chi-squared test"
    nr <- as.integer(nrow(x))
    nc <- as.integer(ncol(x))
    if (is.na(nr) || is.na(nc) || is.na(nr * nc))
      stop("invalid nrow(x) or ncol(x)", domain = NA)
    sr <- rowSums(x)
    sc <- colSums(x)
    E <- outer(sr, sc)/n
    v <- function(r, c, n) c * r * (n - r) * (n - c)/n^3
    V <- outer(sr, sc, v, n)
    dimnames(E) <- dimnames(x)
    if (simulate.p.value && all(sr > 0) && all(sc > 0)) {
      setMETH()
      tmp <- .Call(C_chisq_sim, sr, sc, B, E)
      STATISTIC <- sum(sort((x - E)^2/E, decreasing = TRUE))
      PARAMETER <- NA
      PVAL <- (1 + sum(tmp >= almost.1 * STATISTIC))/(B +
                                                        1)
    }
    else {
      if (simulate.p.value)
        warning("cannot compute simulated p-value with zero marginals")
      if (correct && nrow(x) == 2L && ncol(x) == 2L) {
        YATES <- min(0.5, abs(x - E))
        if (YATES > 0)
          METHOD <- paste(METHOD, "with Yates' continuity correction")
      }
      else YATES <- 0
      STATISTIC <- sum((abs(x - E) - YATES)^2/E)
      PARAMETER <- (nr - 1L) * (nc - 1L)
      PVAL <- pchisq(STATISTIC, PARAMETER, lower.tail = FALSE)
    }
  }
  else {
    if (length(dim(x)) > 2L)
      stop("invalid 'x'")
    if (length(x) == 1L)
      stop("'x' must at least have 2 elements")
    if (length(x) != length(p))
      stop("'x' and 'p' must have the same number of elements")
    if (any(p < 0))
      stop("probabilities must be non-negative.")
    if (abs(sum(p) - 1) > sqrt(.Machine$double.eps)) {
      if (rescale.p)
        p <- p/sum(p)
      else stop("probabilities must sum to 1.")
    }
    METHOD <- "Chi-squared test for given probabilities"
    E <- n * p
    V <- n * p * (1 - p)
    STATISTIC <- sum((x - E)^2/E)
    names(E) <- names(x)
    if (simulate.p.value) {
      setMETH()
      nx <- length(x)
      sm <- matrix(sample.int(nx, B * n, TRUE, prob = p),
                   nrow = n)
      ss <- apply(sm, 2L, function(x, E, k) {
        sum((table(factor(x, levels = 1L:k)) - E)^2/E)
      }, E = E, k = nx)
      PARAMETER <- NA
      PVAL <- (1 + sum(ss >= almost.1 * STATISTIC))/(B +
                                                       1)
    }
    else {
      PARAMETER <- length(x) - 1
      PVAL <- pchisq(STATISTIC, PARAMETER, lower.tail = FALSE)
    }
  }
  names(STATISTIC) <- "X-squared"
  names(PARAMETER) <- "df"
  if (any(E < 5) && is.finite(PARAMETER))
    warning("Chi-squared approximation may be incorrect")
  structure(list(statistic = STATISTIC, parameter = PARAMETER,
                 p.value = PVAL, method = METHOD, data.name = DNAME, observed = x,
                 expected = E, residuals = (x - E)/sqrt(E),
                 stdres = (x -E)/sqrt(V)), class = "htest")
}
