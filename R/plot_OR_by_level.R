#' Statified OR Plot with Confidence Intervals
#'
#' @param dat a data frame of patient characteristics from a study.
#' @param factor.var1 string indicating the first factor variable to stratify by (what will be on the x-axis)
#' @param prop.var string indicating the outcome variable to calculate the proportion of.  Must be binary or logical
#' @param factor.var2 string indicating the second factor variable to stratify by (what will be designated by color), default is NULL which ignores this argument.
#' @param alpha the confidence level to use for the confidence interval construction.  by default it uses 0.05 => 95\% confidence intervals.
#' @return a ggplot of the ORs of the outcome stratified by factor.var1 and (optionally) factor.var2.
#' @author Jesse D. Raffa
#' @details This function produces a plot
#' @seealso \code{\link[ggplot2]{ggplot}}
#' @export
#' @import dplyr
#' @import ggplot2
#' @examples
#'
#' set.seed(1)
#' require(ggplot2); require(dplyr)
#' N <- 100;
#' dataf <- data.frame(outcome=sample(c(0,1),N,replace=TRUE),age=c(runif(N-1,0,100),NA),sex=as.factor(sample(c(0,1),N,replace=TRUE)),cholesterol=rnorm(N,100,30),rx=as.factor(sample(c(0,1),N,replace=TRUE)))
#' plot_OR_by_level(dataf,"sex","outcome",factor.var2="rx")


plot_OR_by_level <- function(dat,factor.var1,prop.var,alpha=0.05,factor.var2=NULL,ylab="Proportion",ref.group=1,include.ref.group.effect=TRUE) {

  if(is.null(factor.var2)) {
    toplot <- dat %>% group_by_(factor.var1) %>%
      mutate_(prop.var2=paste0("as.numeric(as.character(", prop.var, "))"))%>%
      summarise(n=n(),prop=mean(prop.var2,na.rm=T)) %>% mutate(dummy=1)
    toplot <- toplot %>%  full_join(rename_(toplot,levelname1=factor.var1),by=c("dummy")) %>% select(-dummy)
    toplot <- toplot %>% filter(toplot %>% group_by_(factor.var1) %>% group_indices() == ref.group) %>%
      mutate(c=n.x*prop.x,d=n.x*(1-prop.x),a=n.y*prop.y,b=n.y*(1-prop.y),OR=a*d/(b*c),logOR=log(OR),
             selogOR=sqrt(1/a+1/b +1/c+1/d),UL=exp(log(OR) + qnorm(1-alpha/2)*selogOR),LL=exp(log(OR) - qnorm(1-alpha/2)*selogOR)) %>%
      rename_(tmp=factor.var1) %>% mutate(UL=ifelse(tmp!=levelname1,UL,1),LL=ifelse(tmp!=levelname1,LL,1))
    if(!include.ref.group.effect) {
      toplot<- toplot %>% filter(levelname1!=tmp)

    }
    return(ggplot2::ggplot(toplot,ggplot2::aes_string(x="levelname1","OR")) + ggplot2::geom_point() + ggplot2::geom_errorbar(aes(ymax=UL,ymin=LL),width=0.1) + xlab(factor.var1) + ylab("OR") + geom_hline(aes(yintercept=1),linetype=3,alpha=0.5))
  } else {

    toplot <- dat %>% group_by_(factor.var1,factor.var2) %>%
      mutate_(prop.var2=paste0("as.numeric(as.character(", prop.var, "))"))%>%
      summarise(n=n(),prop=mean(prop.var2,na.rm=T)) %>% mutate(dummy=1)
    ccc <- c("dummy","levelname1")
    ccc <- setNames(ccc,c("dummy",factor.var1))
    toplot <- toplot %>%  rename_(var2=factor.var2) %>% inner_join(rename_(toplot,levelname1=factor.var1),by=ccc) %>% select(-dummy)
    keep <- toplot %>% group_by(var2) %>% group_indices() == ref.group
    toplot <- toplot %>% ungroup() %>% filter(keep) %>% mutate(c=n.x*prop.x,d=n.x*(1-prop.x),a=n.y*prop.y,b=n.y*(1-prop.y),OR=a*d/(b*c),logOR=log(OR),
                                                     selogOR=sqrt(1/a+1/b +1/c+1/d),UL=exp(log(OR) + qnorm(1-alpha/2)*selogOR),LL=exp(log(OR) - qnorm(1-alpha/2)*selogOR)) %>%
      rename_(tmp=factor.var2) %>% mutate(UL=ifelse(tmp!=var2,UL,1),LL=ifelse(tmp!=var2,LL,1))
    if(!include.ref.group.effect) {
      toplot<- toplot %>% filter(tmp!=var2)
    }
    return(ggplot2::ggplot(toplot,ggplot2::aes_string(x="tmp","OR",col=factor.var1)) + ggplot2::geom_point(position=position_dodge(width=0.5)) + ggplot2::geom_errorbar(aes(ymax=UL,ymin=LL),width=0.1,position=position_dodge(width=0.5)) + xlab(factor.var2) + ylab("OR") + geom_hline(aes(yintercept=1),linetype=3,alpha=0.5))
  }
}
