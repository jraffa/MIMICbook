% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/produce.table1.R
\name{produce.table1}
\alias{produce.table1}
\title{Produce a "Table 1" for many clinical research reports.}
\usage{
produce.table1(x, labels = NULL)
}
\arguments{
\item{x}{a data frame of patient characteristics from a study.  All components in x are assumed to be numeric or factors.}

\item{labels}{a list of reference values to use for factor variables.  If NULL reports the most prevalent group as the referenced group.  It will specify which group in the table.}
}
\value{
a character matrix of 1 column with means and standard deviations calculated for numeric columns and N and % reported for factor columns
}
\description{
This function computes useful summary statistics from a data frame of numbers and factors into what many call a "Table 1" for many clinical research reports used to describe the study population.  It can also be used to construct comparison tables for treatment groups or other strata.
}
\details{
This function computes mean and standard deviations for numeric columns and number of individuals with a characteristic along with percentage for factor columns contained in a data.frame.  Outputs a matrix of characters which can then be passed to some other functions for display in a research report.
See the \code{\link[tableone]{tableone}} package for a more comprehensive function that has many other bells and whistles.
}
\examples{
require(knitr)
set.seed(1)
N <- 100;
dataf <- data.frame(age=runif(N,0,100),sex=sample(c("M","F"),N,replace=TRUE),cholesterol=rnorm(N,100,30))
out <- produce.table1(dataf,labels=c(NA,"F",NA))
kable(out)
dfbysex <- split(dataf,dataf$sex)
out1 <- produce.table1(dfbysex[[1]])
out2 <- produce.table1(dfbysex[[2]],labels=attr(out1,"labels"))
outbysex <- cbind(out1,out2)
colnames(outbysex) <- paste(colnames(outbysex), c(as.character(dfbysex[[1]]$sex[1]),as.character(dfbysex[[2]]$sex[1])))
kable(outbysex)
}
\author{
Jesse D. Raffa
}
\seealso{
\code{\link[knitr]{kable}} \code{\link[xtable]{xtable}} \code{\link[tableone]{tableone}}
}

