allperm<-function(m1,n1,m2,n2,delta){
#' Calculate all values for two-stage Mann-Whitney-Wilcoxon statistic, with frequencies
#'
#' @param m1 Number in group 1, first evaluation 
#' @param n1 Number in group 2, first evaluation
#' @param m2 Number in group 1, second evaluation 
#' @param n2 Number in group 2, second evaluation
#' @param delta Under normal unit variance model for responses, separation between means in groups.
#' @return Matrix with three columns, reprsenting values of statistic at time 1, values at time 2, and count.
#' @export
#' @examples
#' allperm(2,2,2,2,0)
#'
   out<-array(0,c(n1*m1+1,(n1+n2)*(m1+m2)+1))
   mode(out)<-"integer"
   fout<-.Fortran("allperm",as.integer(m1),as.integer(n1),as.integer(m2),as.integer(n2),as.double(delta),cnt=as.integer(0),out=out,efg=as.integer(0),toobig=as.integer(0),PACKAGE="TwoStage")
   if(fout$efg) cat("Delta too large in allperm\n")
   if(fout$toobig) cat("Too many permutations in allperm\n")
   return(fout$out)
}

