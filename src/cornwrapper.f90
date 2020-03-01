     subroutine cornwrapper(xvraw,xv,xvc,kk1,kk2,kk3,kk4,nn,alpha,alphanew,rho,tointeger,order)
implicit none
     integer nn,done,order,iflg
     character(len=77) fmt
     double precision xv(2),rho,kk3(2,2,2),kk4(2,2,2,2),alpha(2),kk1(2),kk2(2,2),sds(2),xvc(2),alphanew(2),xvraw(2)
     double precision x1,x2,x1p,x1pp,x2p,x2pp,k111,k112,k122,k222,k1111,k1112,k1122,k1222,k2222,temp
     double precision rtn,rtni,x1a,phidens,phibar1
     logical tointeger
!    write(6,*) "Entering cornwrapper, alpha",alpha
     k111=kk3(1,1,1)
!    write(6,*) "In cornwrapper kk1(1)",kk1(1),"kk1(2)",kk1(2)
     k112=kk3(1,1,2)
     k122=kk3(1,2,2)
     k222=kk3(2,2,2)
     k1111=kk4(1,1,1,1)
     k1112=kk4(1,1,1,2)
     k1122=kk4(1,1,2,2)
     k1222=kk4(1,2,2,2)
     k2222=kk4(2,2,2,2)
     sds(1)=sqrt(kk2(1,1))
     sds(2)=sqrt(kk2(2,2))
     alphanew(1)=alpha(1)
     alphanew(2)=alpha(2)
     done=0
     do while(done.lt.2)
        done=done+1
        call makexvn(xv,rho,alphanew,iflg)
        if(iflg.ne.0) write(6,*) "Error from iflg"
!       write(6,*) "In cornwrapper after makexvn, rho=",rho,"alphanew",alphanew,"xv",xv,"alpha",alpha
        x1=xv(1);x2=xv(2)
        call bivcorn(x1,x2,x1p,x1pp,x2p,x2pp,rho,k111,k112,k122,k222,k1111,k1112,k1122,k1222,k2222,nn,alphanew,.false.)
!       write(6,*) "In cornwrapper after bivcorn alpha",alpha
        fmt='(a2,1x,f8.4,1x,a3,1x,f8.4,1x,a4,1x,f8.4,1x,a2,1x,i2,1x,a6,f8.4,1x,a2,1x,f8.4)'
!       write(6,fmt) "x1",x1,"x1p",x1p,"x1pp",x1pp,"nn",nn,"kk1(1)",kk1(1),"sd",sds(1)
!       write(6,fmt) "x2",x2,"x2p",x2p,"x2pp",x2pp,"nn",nn,"kk1(2)",kk1(2),"sd",sds(2)
        xv(1)=sds(1)*x1+kk1(1)
        xv(2)=sds(2)*x2+kk1(2)
!       write(6,*) "In cornwrapper initial normal approx",xv,"alpha",alpha
        temp=x1
        if(order.ge.1) temp=temp+x1p/dsqrt(dble(nn))
        if(order.ge.2) temp=temp+x1pp/(2.0d0*dble(nn))
        xvc(1)=sds(1)*temp+kk1(1)
        temp=x2
        if(order.ge.1) temp=temp+x2p/dsqrt(dble(nn))
        if(order.ge.2) temp=temp+x2pp/(2.0d0*dble(nn))
        xvc(2)=sds(2)*temp+kk1(2)
!       write(6,*) "In cornwrapper corrected normal approx",xvc,"alpha",alpha
!       write(6,*) "tointeger",tointeger,"done",done,"order",order
        if(tointeger.and.(done.eq.1)) then
!          write(6,*) "Mark a xvc(1)",xvc(1)
           xvraw(1)=xvc(1)
           temp=floor(xvc(1))+0.5d0
!          write(6,*) "First component reset with cc from ",xvc(1)," to ",temp
           x1a=(temp-kk1(1))/sds(1)
           rtn=dsqrt(dble(nn))
           rtni=1.0d0/rtn
           alphanew(1)=phibar1(rtn*x1a)
           if(order.ge.1) alphanew(1)=alphanew(1)+&
              phidens(rtn*x1a)*rtni*k111*(x1a**2-1)/6.
           if(order.ge.2) alphanew(1)=alphanew(1)+&
              phidens(rtn*x1a)*rtni**2*k1111*x1a*(x1a**2-3)/24.+&
                 k111**2*x1a*(x1a**4-10*x1a**2+15)/72.
!          write(6,*) "Resetting alpha from",alpha(1)," to ",alphanew(1)
        else
!          write(6,*) "Other branch, xv(2)=",xv(2)
           done=2
        end if
     end do
     if(tointeger) then
        xvc(1)=floor(xvc(1))+0.5d0
        xvraw(2)=xvc(2)
        xvc(2)=floor(xvc(2))+0.5d0
     end if
!    write(6,*) "About to exit cornwrapper alpha",alpha,"xvc",xvc(1),xvc(2)
     return
     end
