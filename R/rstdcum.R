rstdcum<-function(m1,m2,n1,n2,delta){
#' Calculate standardized joint cumulants for sequential Mann Whitney wilcoxon statistic
#'
#' @param m1 Number in group 1, first evaluation 
#' @param n1 Number in group 2, first evaluation
#' @param m2 Number in group 1, second evaluation 
#' @param n2 Number in group 2, second evaluation
#' @param delta Under normal unit variance model for responses, separation between means in groups.
#' @return List with components kk1 through kk4, with order 1 through 4 order joint cumulants, with 1 through 4 dimensions respectively.
#' @examples
#' rstdcum(2,2,2,2,0)
#'
#' @export
   kk1<-rep(0,2)
   mode(kk1)<-"double"
   kk2<-array(0,c(2,2))
   mode(kk2)<-"double"
   kk3<-array(0,c(2,2,2))
   mode(kk3)<-"double"
   kk4<-array(0,c(2,2,2,2))
   mode(kk4)<-"double"
   out<-.Fortran("rstdcum", m1=as.integer(m1), m2=as.integer(m2),
      n1=as.integer(n1), n2=as.integer(n2),rho=as.double(0),
      sds=as.double(c(0,0)), kk1=kk1, kk2=kk2, kk3=kk3, kk4=kk4,
      delta=as.double(delta),ctl=as.logical(TRUE),badv=as.logical(rep(FALSE,14)),efg=as.integer(0),
      PACKAGE="TwoStage")
   if(out$efg!=0) cat(paste("Error reading sample file",out$efg,"\n"))
   return(list(kk1=out$kk1,kk2=out$kk2,kk3=out$kk3,kk4=out$kk4))
}
