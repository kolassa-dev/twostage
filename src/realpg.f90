!######################################
     double precision function realpg(m1,n1,m2,n2,out,nparts)
     use pmod
     integer m1,n1,m2,n2,mm,nn,ii,nparts
     double precision out(max(1,nparts))
     mm=m1+m2
     nn=n1+n2
     out(1)=probs(0+1)*(mm-1)*(nn-2)*probs(9+1)
     out(2)=2*(mm-1)*probs(1+4)
     out(3)=(nn-2)*probs(1+12)
     out(4)=2*probs(1+9)
     realpg=0.0d0
     do ii=1,nparts
        out(ii)=mm*nn*(nn-1)*out(ii)
        realpg=realpg+out(ii)
     end do
     return
     end
