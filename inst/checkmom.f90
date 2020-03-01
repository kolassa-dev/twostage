     program checkmom
implicit none
     integer m1,n1,m2,n2,nn
     double precision kk3(2,2,2),kk4(2,2,2,2),rho,sds(2),kk1(2),kk2(2,2),tail(2)
     double precision xvn(2),xvc(2),alpha(2),dinvnr,xvraw(2)
!    double precision ok,okc
     logical done,ctl
     real ta,ua,et,sd,k3,k4
     double precision delta
     integer isel,ifault,ii,jj,efg
     logical badv(14)
     delta=0.0d0
     done=.false.
     nn=1
     open(38,file="lastinput")
     ii=0
     alpha(1)=0.98d0
     alpha(2)=0.95d0
     do while(.not.done)
        ii=ii+1
        write(6,*) "m1,n1,m2,n2,delta"
        read(5,*) m1,n1,m2,n2,delta
        write(38,*) m1,n1,m2,n2,delta
        write(6,*) "Just read",m1,n1,m2,n2,delta
        if((m1+m2+n1+n2).gt.0) then
           call fillps(delta)
           ctl=.true.
           call rstdcum(m1,m2,n1,n2,rho,sds,kk1,kk2,kk3,kk4,delta,ctl,badv,efg)
           if(efg.gt.0) write(6,*) "Error from rstdcum"
           do jj=1,14
              if(badv(jj)) write(6,*) "Moment ",jj,"mismatch"
           end do
           write(6,*) "rho",rho
!          call makexvn(xvn,rho,alpha)
           call cornwrapper(xvraw,xvn,xvc,kk1,kk2,kk3,kk4,nn,alpha,rho,tail,.false.,2)
           write(6,*) "alpha",alpha,"tail",tail
           write(6,'(a30,1x,f8.4,1x,f8.4)') "In checkmom uncorrected critical values",xvn(1),xvn(2)
           write(6,'(a30,1x,f8.4,1x,f8.4)') "In checkmom corrected critical values",xvc(1),xvc(2)
!          call evalu(xv,xvc,alpha,ok,okc)
!          write(6,*) "In checkmom ok",ok,"okc",okc
!          call texout(ii,m1,n1,m2,n2,xvn(1),xvn(2),xvc(1),xvc(2),ok,okc)
           isel=1
           ua=dinvnr(1.0d0-alpha(1),alpha(1))
!          write(6,*) "Before pprank ua",ua
           call pprank(ta,ua,et,sd,k3,k4,m1+n1,min(m1,n1),isel,ifault)
           write(6,*) "m1",m1,"n1",n1,"ta",ta,"k3",k3,"k4",k4,"et",et,"sd",sd,"ua*sd+et",ua*sd+et,"ua",ua
        else
!          call texout(0,m1,n1,m2,n2,xvn(1),xvn(2),xvc(1),xvc(2),ok,okc)
           done=.true.
        end if
     end do
     close(38)
     end
