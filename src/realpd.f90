!######################################
     double precision function realpd(m1,n1,m2,n2,out,nparts)
!F &=\isum\jsum\ksum\lsum I_{i j} I_{k \ell} U_2\cr
     use pmod
     integer m1,n1,m2,n2,mm,nn,ii,nparts
     double precision out(max(1,nparts))
     mm=m1+m2
     nn=n1+n2
     out(1)=probs(0+1)**3*(mm-3)*(nn-3)
     out(2)=3*(mm-3)*probs(0+1)*probs(1+1)
     out(3)=3*(nn-3)*probs(0+1)*probs(9+1)
     out(4)=3*probs(0+1)**2
     out(5)=6*probs(4+1)
     realpd=0.0d0
     do ii=1,nparts
        out(ii)=mm*nn*(mm-1)*(nn-1)*(mm-2)*(nn-2)*out(ii)*probs(0+1)
        realpd=realpd+out(ii)
     end do
     return
     end
