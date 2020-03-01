checktest<-function(m1,n1,m2,n2,delta,quantiles=c(0.98,0.95),docheck=TRUE,ctl=FALSE){
#' Calculates values associated with testing using two-stage Mann-Whitney-Wilcoxon statistic.
#'
#' @param m1 Number in group 1, first evaluation 
#' @param n1 Number in group 2, first evaluation
#' @param m2 Number in group 1, second evaluation 
#' @param n2 Number in group 2, second evaluation
#' @param delta Under normal unit variance model for responses, separation between means in groups.
#' @param quantiles Quantiles for first stage and overall 1-size.
#' @param docheck Flag controling whether checking vs a table of the joint distribution is done.  Default is true.
#' @param ctl Flag controling whether simulation is used to get emprical distribution.
#' @return List with componets delta, quantiles at which critical values ought to be calculated, m1, n2, m2, n2 from input,normal-approximation critical values, Cornish-Fisher critical values,true size of normal theory test, true size of Cornish-Fisher test, true power under delta, Edgworth approximation to power under normal critical values, Edgeworth approximation to power under Cornish-Fisher critical values.
#' @examples
#' #For two stage testing paper
#' checktest(5,5,5,5,1.5,quantiles=c(.98,.95),docheck=TRUE,ctl=TRUE)
#' checktest(5,5,5,5,1.5,quantiles=c(.99,.975),docheck=TRUE,ctl=TRUE)
#'
#' @export
   nn<-1
   rho<-.Fortran("justrho",m1=as.integer(m1),m2=as.integer(m2),
      n1=as.integer(n1),n2=as.integer(n2),rho=as.double(0.0),
      delta=as.double(0.0),PACKAGE="TwoStage")
   xvn<-bivcornish::fun.givex(quantiles,rho$rho)
   insider<-.Fortran("inside",m1=as.integer(m1),m2=as.integer(m2),
      n1=as.integer(n1),n2=as.integer(n2),delta=as.double(delta), ana=as.double(c(0,0)),anb=as.double(c(0,0)),
      xvraw=as.double(c(0,0)),xvrawc=as.double(c(0,0)), xvn=as.double(xvn),xvcn=as.double(c(0,0)),size=as.double(0),
      sizec=as.double(0),tail=as.double(c(0,0)),utail=as.double(c(0,0)),
      power=as.double(0),powerc=as.double(0),quantiles=as.double(quantiles),
      docheck=as.integer(docheck),ctl=as.integer(ctl),efg=as.integer(0),PACKAGE="TwoStage")
#  insides<-inside(m1,m2,n1,n2,delta,1-quantiles,xvn)
   if(insider$efg!=0) cat("In checktest after compiled inside, efg non zero, indicating file failure\n")
#  browser()
   checktestout<-list(delta=delta,quantiles=quantiles,m1=m1,n1=n1,m2=m2,n2=n2,cunc=insider$xvn,ccor=insider$xvcn,size=1-insider$size,corsize=1-insider$sizec,power=1-insider$power,uncedspower=1-insider$utail[2],coredspower=1-insider$tail[2])
   return(checktestout)
}
