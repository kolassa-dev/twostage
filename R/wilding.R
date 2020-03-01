#' Calculate mass function for two-stage Mann-Whitney-Wilcoxon statistic
#'
#' @param u1 Statistic at first evaluation 
#' @param u2 Statistic at second evaluation 
#' @param m1 Number in group 1, first evaluation 
#' @param n1 Number in group 2, first evaluation
#' @param m2 Number in group 1, second evaluation 
#' @param n2 Number in group 2, second evaluation
#' @return Mass function
#' @examples
#' wilding(1,1,2,2,2,2)
#'
#' @export
wilding<-function(u1,u2,m1,n1,m2,n2){
   out<-.Fortran("wildings",u1=as.integer(u1),u2=as.integer(u2),m1=as.integer(m1),n1=as.integer(n1),m2=as.integer(m2),n2=as.integer(n2),out=as.double(0.0),PACKAGE="TwoStage")
   return(out$out)
}
