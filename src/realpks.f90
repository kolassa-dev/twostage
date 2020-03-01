!######################################
     double precision function realpks(m1,n1,m2,n2,out,nparts)
!F &=\isum\jsum\ksum\lsum I_{i j} I_{k \ell} U_2\cr
     use pmod
     integer m1,n1,m2,n2,mm,nn,ii,nparts
     double precision out(max(1,nparts))
     mm=m1+m2
     nn=n1+n2
     out(1)=probs(1+2)*(nn-1)*(mm-3)*probs(1+0)
     out(2)=(mm-3)*probs(1+6)
     out(3)=3*(nn-1)*probs(1+3)
     out(4)=3*probs(1+2)
     realpks=0.0d0
     do ii=1,nparts
        out(ii)=nn*mm*(mm-2)*(mm-1)*out(ii)
        realpks=realpks+out(ii)
     end do
     return
     end
