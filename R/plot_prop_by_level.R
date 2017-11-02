#' Statified Proportion Plot with Confidence Intervals
#'
#' @param dat a data frame of patient characteristics from a study.
#' @param factor.var1 string indicating the first factor variable to stratify by (what will be on the x-axis)
#' @param prop.var string indicating the outcome variable to calculate the proportion of.  Must be binary or logical
#' @param factor.var2 string indicating the second factor variable to stratify by (what will be designated by color), default is NULL which ignores this argument.
#' @param alpha the confidence level to use for the confidence interval construction.  by default it uses 0.05 => 95\% confidence intervals.
#' @return a ggplot of the probability of the outcome stratified by factor.var1 and (optionally) factor.var2
#' @author Jesse D. Raffa
#' @details This function produic
#' @seealso \code{\link[ggplot2]{ggplot}}
#' @export
#' @import dplyr
#' @import ggplot2
#' @examples
#'
#' set.seed(1)
#' require(ggplot2); require(dplyr)
#' N <- 100;
#' dataf <- data.frame(outcome=sample(c(0,1),N,replace=TRUE),age=c(runif(N-1,0,100),NA),sex=as.factor(sample(c(0,1,NA),N,replace=TRUE)),cholesterol=rnorm(N,100,30),rx=as.factor(sample(c(0,1),N,replace=TRUE)))
#' plot_prop_by_level(dataf,"rx","outcome",factor.var2="sex")


plot_prop_by_level <- function(dat,factor.var1,prop.var,alpha=0.05,factor.var2=NULL,ylab="Proportion") {

  if(is.null(factor.var2)) {
    toplot <- dat %>% group_by_(factor.var1) %>%
      mutate_(prop.var2=paste0("as.numeric(as.character(", prop.var, "))"))%>%
      summarise(n=n(),prop=mean(prop.var2,na.rm=T),LL= prop - qnorm(1-alpha/2)*sqrt(prop*(1-prop)/n),UL= prop + qnorm(1-alpha/2)*sqrt(prop*(1-prop)/n))
    #print(toplot)
    return(ggplot2::ggplot(toplot,ggplot2::aes_string(x=factor.var1,"prop")) + ggplot2::geom_point() + ggplot2::geom_errorbar(aes(ymax=UL,ymin=LL),width=0.1) + ylab(ylab))
  } else {

    toplot <- dat %>% group_by_(factor.var1,factor.var2) %>%
      mutate_(prop.var2=paste0("as.numeric(as.character(", prop.var, "))"))%>%
      summarise(n=n(),prop=mean(prop.var2,na.rm=T),LL= prop - qnorm(1-alpha/2)*sqrt(prop*(1-prop)/n),UL= prop + qnorm(1-alpha/2)*sqrt(prop*(1-prop)/n))
    #print(toplot)
    return(ggplot2::ggplot(toplot,aes_string(x=factor.var1,"prop",col=factor.var2,group=factor.var2)) + ggplot2::geom_point(position=position_dodge(width=0.5)) + ggplot2::geom_errorbar(aes(ymax=UL,ymin=LL),width=0.1,alpha=0.6,position=position_dodge(width=0.5)) + ylab(ylab))
  }
}

#' Statified Odds Plot with Confidence Intervals
#'
#' @param dat a data frame of patient characteristics from a study.
#' @param factor.var1 string indicating the first factor variable to stratify by (what will be on the x-axis)
#' @param prop.var string indicating the outcome variable to calculate the odds of.  Must be binary or logical
#' @param factor.var2 string indicating the second factor variable to stratify by (what will be designated by color), default is NULL which ignores this argument.
#' @param alpha the confidence level to use for the confidence interval construction.  by default it uses 0.05 => 95\% confidence intervals.
#' @return a ggplot of the odds of the outcome stratified by factor.var1 and (optionally) factor.var2
#' @author Jesse D. Raffa
#' @details This function 
#' @seealso \code{\link[ggplot2]{ggplot}}
#' @export
#' @import dplyr
#' @import ggplot2
#' @examples
#'
#' set.seed(1)
#' require(ggplot2); require(dplyr)
#' N <- 100;
#' dataf <- data.frame(outcome=sample(c(0,1),N,replace=TRUE),age=c(runif(N-1,0,100),NA),sex=as.factor(sample(c(0,1,NA),N,replace=TRUE)),cholesterol=rnorm(N,100,30),rx=as.factor(sample(c(0,1),N,replace=TRUE)))
#' plot_odds_by_level(dataf,"rx","outcome",factor.var2="sex")

plot_odds_by_level <- function(dat,factor.var1,prop.var,alpha=0.05,factor.var2=NULL,ylab="Odds") {

  if(is.null(factor.var2)) {
    toplot <- dat %>% group_by_(factor.var1) %>%
      mutate_(prop.var2=paste0("as.numeric(as.character(", prop.var, "))"))%>%
      summarise(n=n(),prop=mean(prop.var2,na.rm=T),odds=prop/(1-prop),LL= prop - qnorm(1-alpha/2)*sqrt(prop/(1-prop)^3/n),UL= prop + qnorm(1-alpha/2)*sqrt(prop/(1-prop)^3/n))
    #print(toplot)
    return(ggplot2::ggplot(toplot,ggplot2::aes_string(x=factor.var1,"prop")) + ggplot2::geom_point() + ggplot2::geom_errorbar(aes(ymax=UL,ymin=LL),width=0.1) + ylab(ylab))
  } else {

    toplot <- dat %>% group_by_(factor.var1,factor.var2) %>%
      mutate_(prop.var2=paste0("as.numeric(as.character(", prop.var, "))"))%>%
      summarise(n=n(),prop=mean(prop.var2,na.rm=T),LL= prop - qnorm(1-alpha/2)*sqrt(prop*(1-prop)/n),UL= prop + qnorm(1-alpha/2)*sqrt(prop*(1-prop)/n))
    #print(toplot)
    return(ggplot2::ggplot(toplot,aes_string(x=factor.var1,"prop",col=factor.var2,group=factor.var2)) + ggplot2::geom_point(position=position_dodge(width=0.5)) + ggplot2::geom_errorbar(aes(ymax=UL,ymin=LL),width=0.1,alpha=0.6,position=position_dodge(width=0.5)) + ylab(ylab))
  }
}
