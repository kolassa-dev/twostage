!######################################
     double precision function realpe(m1,n1,m2,n2,out,nparts)
!F &=\isum\jsum\ksum\lsum I_{i j} I_{k \ell} U_2\cr
     use pmod
     integer m1,n1,m2,n2,mm,nn,ii,nparts
     double precision out(max(1,nparts))
     mm=m1+m2
     nn=n1+n2
     out(1)=probs(0+1)*probs(4+1)*(mm-2)*(nn-2)
     out(2)=(mm-2)*(probs(3+1)+probs(5+1))
     out(3)=(nn-2)*(probs(7+1 )+probs(5+1))
     out(4)=3*probs(4+1)
     out(5)=probs(8+1)
     realpe=0.0d0
     do ii=1,nparts
        out(ii)=mm*nn*(mm-1)*(nn-1)*out(ii)
        realpe=realpe+out(ii)
     end do
     return
     end
