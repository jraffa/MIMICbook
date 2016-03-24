#' Produce a "Table 1" for many clinical research reports.
#'
#' This function computes useful summary statistics from a data frame of numbers and factors into what many call a "Table 1" for many clinical research reports used to describe the study population.  It can also be used to construct comparison tables for treatment groups or other strata.
#' @param x a data frame of patient characteristics from a study.  All components in x are assumed to be numeric or factors.
#' @param labels a list of reference values to use for factor variables.  If NULL reports the most prevalent group as the referenced group.  It will specify which group in the table.
#' @return a character matrix of 1 column with means and standard deviations calculated for numeric columns and N and % reported for factor columns
#' @author Jesse D. Raffa
#' @details
#' This function computes mean and standard deviations for numeric columns and number of individuals with a characteristic along with percentage for factor columns contained in a data.frame.  Outputs a matrix of characters which can then be passed to some other functions for display in a research report.
#' @seealso \code{\link[knitr]{kable}} \code{\link[xtable]{xtable}}
#' @export
#' @importFrom knitr kable
#' @importFrom xtable xtable
#' @examples
#' require(knitr)
#' set.seed(1)
#' N <- 100;
#' dataf <- data.frame(age=runif(N,0,100),sex=sample(c("M","F"),N,replace=TRUE),cholesterol=rnorm(N,100,30))
#' out <- produce.table1(dataf,labels=c(NA,"F",NA))
#' kable(out)
#' dfbysex <- split(dataf,dataf$sex)
#' out1 <- produce.table1(dfbysex[[1]])
#' out2 <- produce.table1(dfbysex[[2]],labels=attr(out1,"labels"))
#' outbysex <- cbind(out1,out2)
#' colnames(outbysex) <- paste(colnames(outbysex), c(as.character(dfbysex[[1]]$sex[1]),as.character(dfbysex[[2]]$sex[1])))
#' kable(outbysex)

produce.table1 <- function(x,labels=NULL) {
  out <- matrix(NA,nrow=length(x[1,]))
  rrn <- NULL;
  for(i in 1:length(x[1,])) {
    if(is.factor(x[,i])) {
      if(is.null(labels[i])) {
        tmp<- table(x[,i])
        most.prev.name <- names(which.max(tmp))
      } else  {
        if(is.na(labels[i])) {
          tmp<- table(x[,i])
          most.prev.name <- names(which.max(tmp))
        } else {
          most.prev.name <- labels[i];
        }
      }
      if(sum(is.na(x[,i]))==0) {
        out[i,] <- paste0(sum(x[,i]==most.prev.name,na.rm=T), " (", round(100*mean(x[,i]==most.prev.name,na.rm=T),1), "%)")
      } else {
        out[i,] <- paste0(sum(x[,i]==most.prev.name,na.rm=T), " (", round(100*mean(x[,i]==most.prev.name,na.rm=T),1), "%)", "  [Missing: ", sum(is.na(x[,i])), "]")

      }
      rrn[i] <- paste0(names(x)[i], "==", most.prev.name);
      labels[i] <- most.prev.name;

    } else {
      if(sum(is.na(x[,i]))==0) {
        out[i,] <- paste0(round(mean(x[,i],na.rm=T),1),  " (" , round(sd(x[,i],na.rm=T),1), ")")
      } else {
        out[i,] <- paste0(round(mean(x[,i],na.rm=T),1),  " (" , round(sd(x[,i],na.rm=T),1), ")", "  [Missing: ", sum(is.na(x[,i])), "]")
      }
      rrn[i] <- paste0(names(x)[i]);
    }

  }
  rownames(out) <- rrn;
  colnames(out) <- "Average (SD), or N (%)";
  attr(out,"labels") <- labels;
  return(out)
}
