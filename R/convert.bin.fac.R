#' Take a data.frame and convert any columns consisting of only 0's and 1's (binary) values, to a factor variable, to ease calculation and interpreations of subsequent analysis.
#'
#'
#' @param x a data frame of patient characteristics from a study.
#' @param ignore.missing a scalar logical indicating on how to deal with missing data.  When true, the function will convert any column with 0's/1's/NA's into a factor, when false (default), NA's will prevent conversion.
#' @return the same data.frame with updated columns
#' @author Jesse D. Raffa
#' @details
#' This function convert any columns consisting of only 0's and 1's (binary) values, to a factor variable, to ease calculation and interpreations of subsequent analysis.
#' @seealso \code{\link[knitr]{kable}} \code{\link[xtable]{xtable}} \code{\link[tableone]{CreateTableOne}}
#' @export
#' @importFrom knitr kable
#' @importFrom xtable xtable
#' @examples
#'
#' set.seed(1)
#' N <- 100;
#' dataf <- data.frame(age=c(runif(N-1,0,100),NA),sex=sample(c(0,1,NA),N,replace=TRUE),cholesterol=rnorm(N,100,30))
#' summary(dataf)
#' out <-  convert.bin.fac(dataf)
#' summary(out)
#' out <- convert.bin.fac(dataf,ignore.missing=TRUE)
#' summary(out)


convert.bin.fac <- function(x,ignore.missing=FALSE) {
  is.bin.col  <- function(col,...,missing=ignore.missing) {
    if(sum(col==0 | col==1,na.rm=TRUE)==ifelse(missing,sum(!is.na(col)),length(col))) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
  is.bin <- unlist(lapply(x,is.bin.col))
  for(i in seq_along(is.bin)) {
    if(is.bin[i]) {
      x[,i] <- as.factor(x[,i])
    }
  }
  return(x)
}
