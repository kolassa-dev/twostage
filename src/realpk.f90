!######################################
     double precision function realpk(m1,n1,m2,n2,out,nparts)
!F &=\isum\jsum\ksum\lsum I_{i j} I_{k \ell} U_2\cr
     use pmod
     integer m1,n1,m2,n2,mm,nn,ii,nparts
     double precision out(max(1,nparts))
     mm=m1+m2
     nn=n1+n2
     out(1)=probs(1+12)*(mm-1)*(nn-3)*probs(1+0)
     out(2)=3*(mm-1)*probs(7+1 )
     out(3)=(nn-3)*probs(1+11)
     out(4)=3*probs(1+12)
     realpk=0.0d0
     do ii=1,nparts
        out(ii)=mm*nn*(nn-2)*(nn-1)*out(ii)
        realpk=realpk+out(ii)
     end do
     return
     end
