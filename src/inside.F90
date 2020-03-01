     subroutine inside(m1,m2,n1,n2,delta,ana,anb,xvraw,xvrawc,xvn,xvcn,size,sizec,tail,utail,power,powerc,alpha,docheck,ctli,efg)
implicit none
     integer m1,m2,n1,n2,efg,efg1,ctli
     double precision delta,xvn(2),xvcn(2),size,sizec,power,powerc,tail(2),utail(2),alpha(2),temp(2),xvraw(2),xvrawc(2)
     double precision zero,rho,sds(2),kk1(2),kk2(2,2),kk3(2,2,2),kk4(2,2,2,2),ana(2),anb(2)
     logical ctl,badv(14)
     integer docheck
! Turn off MC/brute force moment calculation.
     ctl=ctli.eq.1
#ifdef DEBUGME
     write(6,*) "docheck",docheck,"ctl",ctl
#endif
     zero=0.0d0
! Using sample sizes, calculate cumulants, plus standard deviations and correlation.
!    call justrho(m1,m2,n1,n2,rho,zero)
     call rstdcum(m1,m2,n1,n2,rho,sds,kk1,kk2,kk3,kk4,zero,ctl,badv,efg1)
!    call makexvn(xvn,rho,alpha,efg)
#ifdef DEBUGME
     write(6,*) "In inside rho",rho
#endif
!    call cornwrapper(xvraw,xvn,xvcn,kk1,kk2,kk3,kk4,1,alpha,alphanew,rho,.false.,2)
! Calculate the order 0 cf approximation, AKA, multivariate normal approx.
! Logical flag in argument 10 tells algorithm to return critical values rounded to integer plus half.
     call cornwrapper(xvraw,temp,xvn,kk1,kk2,kk3,kk4,1,alpha,ana,rho,.true.,0)
! Calculate the order 2 cf approximation.
     call cornwrapper(xvrawc,temp,xvcn,kk1,kk2,kk3,kk4,1,alpha,anb,rho,.true.,2)
#ifdef DEBUGME
     write(6,*) "From inside xvcn",xvcn,"docheck",docheck
#endif
     if(docheck.eq.1) call evalu(xvn,xvcn,size,sizec)
     call fillps(delta)
#ifdef DEBUGME
     write(6,*) "In inside just before second rstdcum"
#endif
     call rstdcum(m1,m2,n1,n2,rho,sds,kk1,kk2,kk3,kk4,delta,ctl,badv,efg)
#ifdef DEBUGME
     write(6,*) "In inside just before edswrapper efg",efg,"efg1",efg1
#endif
     call edswrapper(xvcn,kk1,kk2,kk3,kk4,1,rho,tail,.false.)
     call edswrapper(xvcn,kk1,kk2,kk3,kk4,1,rho,utail,.true.)
! Initialize power and powerc to impossible values so we can see that they don't change.
     power=-1.0d0; powerc=-1.0d0
     if(docheck.eq.1) call evalu(xvn,xvcn,power,powerc)
#ifdef DEBUGME
     write(6,'(a26,2(f6.3,1x),a5,2(f6.3,1x),a6,f8.5,a7,f8.5)') "In inside before evalu xvn",xvn(1),xvn(2),"xvcn ",xvcn(1),xvcn(2),&
        "power ",power,"powerc ",powerc
#endif
     efg=max(efg,efg1)
     return
     end
