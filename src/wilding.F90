#ifdef DOINT
   integer(kind=8) recursive function wilding(u1,u2,m1,n1,m2,n2,dd,debug) result(out)
#else
   double precision recursive function wilding(u1,u2,m1,n1,m2,n2,dd,debug) result(out)
#endif
implicit none
   logical debug
   integer u1,u2,m1,n1,m2,n2,dd
   logical cc(2)
#ifdef DOINT
   integer(kind=8) zero, one
   zero=0_8
   one=1_8
#else
   double precision zero, one
   zero=0.0d0
   one=1.0d0
#endif
#ifdef DEBUG
   if(debug) write(6,'(a15,7(1x,a3,i3))') "Calling wilding with", "u1=",u1,"u2=",u2, "m1=",m1,"m2=",m2, "n1=",n1,"n2=",n2,"dd=",dd
#endif
   dd=dd+1
   call altern(cc,u1,u2,m1,n1,m2,n2)
   if(cc(2)) then
#ifdef DEBUG
      if(debug) write(6,*) "Branch A dd",dd,"cc=",cc(1),cc(2)
#endif
!     write(6,*) (u1.lt.0),   (u1.gt.(m1*n1)),   (u2.lt.0),   (u2.gt.((m1+m2)*(n1+n2))),   (u2.lt.u1)
      out=zero
   else
      if(cc(1)) then
#ifdef DEBUG
         if(debug) write(6,*) "Branch B dd",dd
#endif
         out=one
      else
#ifdef DEBUG
         if(debug) write(6,*) "Branch C dd",dd
#endif
         out=(m1*wilding(u1,  u2,      m1-1,n1,  m2,n2,dd,debug)+&
             n1*wilding(u1-m1,u2-m1-m2,m1  ,n1-1,m2,n2,dd,debug)+&
             m2*wilding(u1,   u2,      m1  ,n1,m2-1,n2,dd,debug)+&
             n2*wilding(u1,   u2-m1-m2,m1  ,n1,  m2,n2-1,dd,debug))
#ifdef DEBUG
         if(debug) write(6,*) "out",out,"m1+m2+n1+n2",m1+m2+n1+n2
#endif
#ifdef DOINT
#else
         out=out/(m1+m2+n1+n2)
#endif
#ifdef DEBUG
         if(debug) write(6,*) "out",out
#endif
      end if
   end if
   dd=dd-1
#ifdef DEBUG
   if(debug) write(6,*) "From level",dd,"out",out
#endif
   return
   end
