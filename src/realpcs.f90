!######################################
     double precision function realpcs(m1,n1,m2,n2,out,nparts)
     use pmod
     integer m1,n1,m2,n2,mm,nn,ii,nparts
     double precision out(max(1,nparts))
     mm=m1+m2
     nn=n1+n2
     out(1)=(mm-3)*(nn-2)*probs(1+1)*probs(0+1)**2
     out(2)=(mm-3)*(probs(0+1)*probs(2+1)+probs(1+1)**2)
     out(3)=(nn-2)*(2*probs(0+1)*probs(4+1)+probs(9+1)*probs(1+1))
     out(4)=3*probs(0+1)*probs(1+1)
     out(5)=probs(3+1)+2*probs(5+1)
     realpcs=0.0d0
     do ii=1,nparts
        out(ii)=mm*nn*(mm-1)*(nn-1)*(mm-2)*out(ii)
        realpcs=realpcs+out(ii)
     end do
     return
     end
