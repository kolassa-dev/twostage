doperm<-function(m1,m2,n1,n2,nsamp=1000){
#' Calculate an MC values for two-stage Mann-Whitney-Wilcoxon statistic, with frequencies
#'
#' @param m1 Number in group 1, first evaluation 
#' @param n1 Number in group 2, first evaluation
#' @param m2 Number in group 1, second evaluation 
#' @param n2 Number in group 2, second evaluation
#' @param nsamp Number of MC samples.
#' @return Table of relative frequencies.
#' @examples
#' doperm(2,2,2,2)
#'
#' @export
   w<-array(NA,c(nsamp,2))
   for(j in 1:nsamp){
      x<-stats::runif(m1)
      y<-stats::runif(n1)
      u<-stats::runif(m2)
      v<-stats::runif(n2)
      w[j,]<-c(stats::wilcox.test(x,y)$statistic,stats::wilcox.test(c(x,u),c(y,v))$statistic)
    }
   return(table(w[,1],w[,2])/nsamp)
}
