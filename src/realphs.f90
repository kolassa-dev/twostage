!######################################
     double precision function realphs(m1,n1,m2,n2,out,nparts)
     use pmod
     integer m1,n1,m2,n2,mm,nn,ii,nparts
     double precision out(max(1,nparts))
     mm=m1+m2
     nn=n1+n2
     out(1)=probs(1+9)*(nn-2)*(mm-1)*(probs(1+0)+(nn-3)*probs(1+9)+(mm-2)*probs(1+1)+(nn-3)*(mm-2)*probs(1+0)**2)
     out(2)=2*(nn-2)*(nn-3)*(mm-1)*probs(1+12)*probs(1+0)
     out(3)=2*(nn-2)*(mm-1)*probs(7+1 )
     out(4)=4*(nn-2)*(mm-1)*(mm-2)*probs(4+1)*probs(1+0)
     out(5)=4*(mm-1)*(nn-2)*probs(1+10)
     out(6)=4*(mm-1)*(nn-2)*probs(1+9)*probs(1+0)
     out(7)=(nn-2)*probs(1+12)
     out(8)=(nn-2)*(nn-3)*probs(1+11)
     out(9)=2*(mm-1)*(probs(4+1)+probs(1+8))
     out(10)=2*(mm-1)*(mm-2)*(probs(1+3)+probs(5+1))
     out(11)=4*probs(1+9)
     out(12)=4*(nn-2)*probs(1+12)
     out(13)=8*(mm-1)*probs(4+1)
     out(14)=4*(nn-2)*(mm-1)*probs(7+1 )
     realphs=0.0d0
     do ii=1,nparts
        out(ii)=nn*mm*(nn-1)*out(ii)
        realphs=realphs+out(ii)
     end do
     return
     end
