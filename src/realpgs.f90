!######################################
     double precision function realpgs(m1,n1,m2,n2,out,nparts)
     use pmod
     integer m1,n1,m2,n2,mm,nn,ii,nparts
     double precision out(max(1,nparts))
     mm=m1+m2
     nn=n1+n2
     out(1)=(nn-1)*(mm-2)*probs(1+1)*probs(0+1)
     out(2)=(nn-1)*2*probs(4+1)
     out(3)=(mm-2)*probs(2+1)
     out(4)=2*probs(1+1)
     realpgs=0.0d0
     do ii=1,nparts
        out(ii)=mm*nn*(mm-1)*out(ii)
        realpgs=realpgs+out(ii)
     end do
!    write(6,*) "In realpgs probs",(probs(ii),ii=1,5),"nn",nn,"mm",mm,"realpgs",realpgs,"out",(out(ii),ii=1,nparts),"nparts",nparts
     return
     end
