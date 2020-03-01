      subroutine tmom(m1,n1,m2,n2,mm1,mm2,mm3,mm4,justtwo)
implicit none
      integer m1,n1,m2,n2
      double precision mm1(2),mm2(2,2),mm3(2,2,2),mm4(2,2,2,2)
      double precision out(4),lx(3),ly(3),temp(14)
      double precision realpg,realpgs,realpf,realpk,realpe,realpc,realpks,realpcs,realpd,realph,realphs
      double precision rg,rgs,rf,rk,re,rc,rks,rcs,rd,rh,rhs
      logical justtwo
      call univariate(m1,n1,m2,n2,out)
!     write(6,*) "After univariate out",out(1)
      mm1(2)=out(1)
      mm2(2,2)=out(2)
      mm3(2,2,2)=out(3)
      mm4(2,2,2,2)=out(4)
      call univariate(m1,n1,0,0,out)
      mm1(1)=out(1)
      mm2(1,1)=out(2)
      mm3(1,1,1)=out(3)
      mm4(1,1,1,1)=out(4)
      lx(1)=m1/dble(m1+m2)
      lx(2)=lx(1)*(m1-1)/dble(m1+m2-1)
      lx(3)=lx(2)*(m1-2)/dble(m1+m2-2)
      ly(1)=n1/dble(n1+n2)
      ly(2)=ly(1)*(n1-1)/dble(n1+n2-1)
      ly(3)=ly(2)*(n1-2)/dble(n1+n2-2)
      mm2(1,2)=mm2(2,2)*lx(1)*ly(1)
      if(.not.justtwo) then
         rgs=realpgs(m1,n1,m2,n2,temp,4)
         rg=realpg(m1,n1,m2,n2,temp,4)
         rf=realpf(m1,n1,m2,n2,temp,5)
         rc=realpc(m1,n1,m2,n2,temp,5)
         rcs=realpcs(m1,n1,m2,n2,temp,5)
         rk=realpk(m1,n1,m2,n2,temp,4)
         rks=realpks(m1,n1,m2,n2,temp,4)
         re=realpe(m1,n1,m2,n2,temp,5)
         rd=realpd(m1,n1,m2,n2,temp,5)
         rh=realph(m1,n1,m2,n2,temp,14)
         rhs=realphs(m1,n1,m2,n2,temp,14)
!        write(6,*) "rgs",rgs,"rg",rg,"rcs",rcs,"rc",rc,"rks",rks,"rk",rk,"rks",rks,"rk",rk,"rf",rf,"re",re,"rd",rd
         mm3(1,1,2)=lx(1)*ly(1)*mm2(2,2)+lx(2)*ly(1)*rgs+lx(1)*ly(2)*rg+lx(2)*ly(2)*rf
         mm3(1,2,2)=lx(1)*ly(1)*mm3(2,2,2)
         mm4(1,1,1,2)=lx(1)*ly(1)*mm2(2,2)+3*lx(1)*ly(2)*rg+lx(1)*ly(3)*rk+3*lx(2)*ly(1)*rgs+3*lx(2)*ly(2)*rf+&
            6*lx(2)*ly(2)*re+3*lx(2)*ly(3)*rc+lx(3)*ly(1)*rks+3*lx(3)*ly(2)*rcs+lx(3)*ly(3)*rd
         mm4(1,1,2,2)=lx(1)*ly(1)*mm3(2,2,2)+lx(1)*ly(2)*rhs+lx(2)*ly(1)*rh+lx(2)*ly(2)*(rd+2*rc+2*rcs+2*rf+2*re)
         mm4(1,2,2,2)=lx(1)*ly(1)*mm4(2,2,2,2)
!        write(6,*) "a",mm2(2,2),"b",(m1/dble(m1+m2))*(n1/dble(n1+n2))
      end if
!     write(6,*) "At end of tmom mm1",mm1
      return
      end
