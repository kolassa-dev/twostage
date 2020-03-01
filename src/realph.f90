!######################################
     double precision function realph(m1,n1,m2,n2,out,nparts)
     use pmod
implicit none
     integer m1,n1,m2,n2,mm,nn,ii,nparts
     double precision out(max(1,nparts))
     mm=m1+m2
     nn=n1+n2
     out(1)=probs(1+1)*(mm-2)*(nn-1)*(probs(1+0)+(mm-3)*probs(1+1)+(nn-2)*probs(1+9)+(mm-3)*(nn-2)*probs(1+0)**2)
     out(2)=2*(mm-2)*(mm-3)*(nn-1)*probs(1+2)*probs(1+0)
     out(3)=2*(mm-2)*(nn-1)*probs(1+3)
     out(4)=4*(mm-2)*(nn-1)*(nn-2)*probs(1+4)*probs(1+0)
     out(5)=4*(nn-1)*(mm-2)*probs(1+5)
     out(6)=4*(nn-1)*(mm-2)*probs(1+1)*probs(0+1)
     out(7)=(mm-2)*probs(1+2)
     out(8)=(mm-2)*(mm-3)*probs(1+6)
     out(9)=2*(nn-1)*(probs(1+4)+probs(1+8))
     out(10)=2*(nn-1)*(nn-2)*(probs(1+7)+probs(1+10))
     out(11)=4*probs(1+1)
     out(12)=4*(mm-2)*probs(1+2)
     out(13)=8*(nn-1)*probs(1+4)
     out(14)=4*(mm-2)*(nn-1)*probs(1+3)
!    write(6,*) "in realph out",out
     realph=0.0d0
     do ii=1,nparts
        out(ii)=mm*nn*(mm-1)*out(ii)
        realph=realph+out(ii)
!       write(6,*) "IN realph ii",ii,"realph",realph
     end do
!    write(6,*) "IN realph realph",realph
     return
     end
