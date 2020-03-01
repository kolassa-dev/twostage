#' Calculate all values for two-stage Mann-Whitney-Wilcoxon statistic, with frequencies
#'
#' @param m1 Number in group 1, first evaluation
#' @param n1 Number in group 2, first evaluation
#' @param m2 Number in group 1, second evaluation
#' @param n2 Number in group 2, second evaluation
#' @param delta Under normal unit variance model for responses, separation between means in groups.
#' @param alpha Two component real vector giving target probabilities at each stage.
#' @param xvn Two component real vector giving normal deviates associated with alpha.  Note that this required knowledge of the correlation, outside of this function.
#'
#' @return List with components MC size of normal critical value, MC size of CF critical value, Edgeworth 1-size of normal critical value, Edgeworth 1-size of CF critical value, Normal type 2 error for corrected critical values, Edgeworth type 2 error for corrected critical values, Monte Carlo type 2 error for uncorrected critical values, Monte Carlo type 2 error for corrected critical values.
#' @importFrom bivcornish cornish2
#' @examples
#' inside(m1=2,n1=2,m2=2,n2=2,delta=1,alpha=c(.98,.95),xvn=c(-2.3263482318637529,-2.0863447264627704))
#'
#' @export
inside<-function(m1,m2,n1,n2,delta,alpha,xvn){
#    cat("Entering inside")
     stdcumlist<-rstdcum(m1,m2,n1,n2,0.0)
     cornout<-cornish2(alpha,list(k2=stdcumlist$kk2,k3=stdcumlist$kk3,k4=stdcumlist$kk4))
     xvn<-cornout$raw
     xvcn<-cornout$cor
#    cat("After cornish2 inside")
     evaluout<-.Fortran("evalu",as.double(xvn),as.double(xvcn),
          size=as.double(0,0),sizec=as.double(0.0),PACKAGE="TwoStage")
#    cat("In inside Mark 1")
     temp<-.Fortran("fillps",as.double(delta),PACKAGE="TwoStage")
#    cat("In inside Mark 2")
     dcl<-rstdcum(m1,m2,n1,n2,delta)
     mode(dcl$kk2)<-"double"; mode(dcl$kk3)<-"double"; mode(dcl$kk4)<-"double"
#    cat("Mark a")
     e1<-.Fortran("edswrapper",xvcn=as.double(xvcn),kk1=as.double(dcl$kk1), kk2=dcl$kk2, kk3=dcl$kk3, kk4=dcl$kk4,nn=as.integer(1), rho=as.double(dcl$rho),tail=as.double(c(0,0)),justnorm=as.logical(FALSE),PACKAGE="TwoStage")
#    cat("Mark a")
     e2<-.Fortran("edswrapper",xvcn=as.double(xvcn),kk1=as.double(dcl$kk1), kk2=dcl$kk2, kk3=dcl$kk3, kk4=dcl$kk4,nn=as.integer(1), rho=as.double(dcl$rho),utail=as.double(c(0,0)),justnorm=as.logical(TRUE),PACKAGE="TwoStage")
#    cat("In inside Just before evalu")
     evaluout<-.Fortran("evalu",xvn=as.double(xvn),xvcn=as.double(xvcn),power=as.double(0),powerc=as.double(0),PACKAGE="TwoStage")
#    cat(       "In inside Just after evalu")
     return(list( size=evaluout$size, sizec=evaluout$sizec,tail=e1$tail,utail=e2$utail,power=evaluout$power,powerc=evaluout$powerc))
}
