!######################################
     subroutine univariate(m1,n1,m2,n2,out)
     use pmod
implicit none
     integer m1,n1,m2,n2,mm,nn
     double precision out(4),temp(14)
     double precision realpg,realpgs,realph,realphs,realpd,realpc,realpcs,realpf,realpe
     mm=m1+m2
     nn=n1+n2
!    write(6,*) "In univariate mm=",mm,"nn=",nn,"probs(0+1)",probs(0+1)
     out(1)=mm*nn*probs(0+1)
     out(2)=mm*nn*(probs(0+1) +(mm-1)*probs(1+1) +(nn-1)*probs(9+1) +(mm-1)*(nn-1)*probs(0+1)**2)
     out(3)=out(2) +realpg(m1,n1,m2,n2,temp,4)&
        +realpgs(m1,n1,m2,n2,temp,4)&
        +realpf(m1,n1,m2,n2,temp,5)
     out(4)=out(3)+realph(m1,n1,m2,n2,temp,14)&
        +realphs(m1,n1,m2,n2,temp,14)+realpd(m1,n1,m2,n2,temp,5)&
        +2.0d0*realpc(m1,n1,m2,n2,temp,5)+2.0d0*realpcs(m1,n1,m2,n2,temp,5) &
        +2.0d0*realpf(m1,n1,m2,n2,temp,5)&
        +2.0d0*realpe(m1,n1,m2,n2,temp,5)
!    write(6,*) "In univariate out",out
!    write(6,*) "realph",realph(m1,n1,m2,n2,temp,14)
!    write(6,*) "realphs",realphs(m1,n1,m2,n2,temp,14)
     return
     end
