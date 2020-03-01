module samplespace
     integer npts
     integer,dimension(:),allocatable::u1,u2
#ifdef BIGINT
     integer(kind=8),dimension(:),allocatable::outv
#else
     integer,dimension(:),allocatable::outv
#endif
end module samplespace
     subroutine rstdcum(m1,m2,n1,n2,rho,sds,kk1,kk2,kk3,kk4,delta,ctl,badv,efg)
     use samplespace
implicit none
     double precision delta
     logical show,ctl,shep
     integer m1,n1,m2,n2,ii,efg,efg1
     logical badv(14)
     character(len=20) filen
     double precision mm0,mm1(2),mm2(2,2),mm3(2,2,2),mm4(2,2,2,2)
     double precision mom1(2),mom2(2,2),mom3(2,2,2),mom4(2,2,2,2)
     double precision kk0,kk1(2),kk2(2,2),kk3(2,2,2),kk4(2,2,2,2)
     double precision rho,sds(2)
#ifdef BIGINT
     integer(kind=8),parameter::one=1_8
#else
     integer,parameter::one=1
#endif
     double precision cnt
     cnt=-one
     show=.false.
     shep=.false.
     if(ctl) then
        npts=0
!npts=0 forces readout to do nothing but count lines, if the exact distribution enumeration exists.
!Otherwise, readout sets npts to the number of points in the sample space.
        if(allocated(outv)) deallocate(outv,u1,u2)
        allocate(outv(1),u1(1),u2(1))
!       call mkfn(m1,n1,m2,n2,filen,pref,delta)
#ifdef DEBUGME
        write(6,*) "Before first readout in rstdcum.  Just counting lines."
#endif
        call readout(outv,u1,u2,npts,m1,m2,n1,n2,cnt,delta,efg1,filen)
#ifdef DEBUGME
        write(6,*) "After first readout in rstdcum, filen",filen
        write(6,'(a5,1x,i6,1x,a20,i1)') "Found",npts,"sample points, efg1=",efg1
#endif
        deallocate(outv,u1,u2)
        call moments(0,0,-1.0d0,mm0,mm1,mm2,mm3,mm4,0)
        allocate(outv(npts),u1(npts),u2(npts))
#ifdef DEBUGME
        write(6,*) "Allocating outv with this many entries:",npts
        write(6,*) "Before second readout in rstdcum"
#endif
        call readout(outv,u1,u2,npts,m1,m2,n1,n2,cnt,delta,efg,filen)
        efg=max(efg,efg1)
#ifdef DEBUGME
        write(6,*) "After second readout in rstdcum"
        write(6,*) "In rstdcum cnt",cnt,"efg after max",efg
#endif
        do ii=1,npts
           call moments(u1(ii),u2(ii),dble(outv(ii))/dble(cnt),mm0,mm1,mm2,mm3,mm4,4)
!          write(6,*) "u1",u1(ii),"u2",u2(ii),"cnt",dble(outv(ii))/dble(cnt)
        end do
#ifdef DEBUGME
        write(6,*) "Moments calculated from enumerated sample space"
#endif
        if(show) call writemoms(mm0,mm1,mm2,mm3,mm4,.false.)
     end if
     call tmom(m1,n1,m2,n2,mom1,mom2,mom3,mom4,.false.)
#ifdef DEBUGME
     write(6,*) "Moments calculated via probability"
#endif
     if(show) call writemoms(mm0,mom1,mom2,mom3,mom4,.false.)
     if(ctl) call cfmoms(mom1,mm1,mom2,mm2,mom3,mm3,mom4,mm4,badv)
     call mom2cum(mom1,mom2,mom3,mom4,kk1,kk2,kk3,kk4)
     if(shep) call corrcum(kk2,kk4)
     kk0=0.0d0
#ifdef DEBUGME
     write(6,*) "In rstdcum Mark a" 
#endif
     if(show) call writemoms(kk0,kk1,kk2,kk3,kk4,.true.)
! This block of code calculates central moments.  First call caches first moments, and
! then add deviations from mean.
!    call moments(0,0,-1.0d0,mm0,mm1,mm2,mm3,mm4,4)
!    do ii=1,npts
!       call moments(u1(ii),u2(ii),dble(outv(ii))/dble(cnt),mm0,mm1,mm2,mm3,mm4,-4)
!    end do
!    call writemoms(mm0,mm1,mm2,mm3,mm4,.false.)
!    call mom2cum(mm1,mm2,mm3,mm4,kk1,kk2,kk3,kk4)
!    kk0=0.0d0
!    call writemoms(kk0,kk1,kk2,kk3,kk4,.true.)
#ifdef DEBUGME
     write(6,*) "In rstdcum m1,n1,m2,n2", m1,n1,m2,n2,"kk1",kk1,"kk2",kk2
#endif
     call stdcum(kk1,kk2,kk3,kk4,sds,rho)
!    write(6,*) "In rstdcum kk3(1,1,1)",kk3(1,1,1),"rho",rho
!    call moments(0,0,-1.0d0,mm0,mm1,mm2,mm3,mm4,4)
!    if(allocated(outv)) deallocate(outv)
     return
     end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     subroutine cfmoms(m1,mm1,m2,mm2,m3,mm3,m4,mm4,badv)
     double precision m1(2),mm1(2),m2(2,2),mm2(2,2),m3(2,2,2),mm3(2,2,2),m4(2,2,2,2),mm4(2,2,2,2)
     double precision eps
     logical badv(14)
     eps=1.0d-3
     call cfone(m1(1),mm1(1),1,eps,badv)
     call cfone(m1(2),mm1(2),2,eps,badv)
     call cfone(m2(1,1),mm2(1,1),3,eps,badv)
     call cfone(m2(1,2),mm2(1,2),4,eps,badv)
     call cfone(m2(2,2),mm2(2,2),5,eps,badv)
     call cfone(m3(1,1,1),mm3(1,1,1),6,eps,badv)
     call cfone(m3(1,1,2),mm3(1,1,2),7,eps,badv)
     call cfone(m3(1,2,2),mm3(1,2,2),8,eps,badv)
     call cfone(m3(2,2,2),mm3(2,2,2),9,eps,badv)
     call cfone(m4(1,1,1,1),mm4(1,1,1,1),10,eps,badv)
     call cfone(m4(1,1,1,2),mm4(1,1,1,2),11,eps,badv)
     call cfone(m4(1,1,2,2),mm4(1,1,2,2),12,eps,badv)
     call cfone(m4(1,2,2,2),mm4(1,2,2,2),13,eps,badv)
     call cfone(m4(2,2,2,2),mm4(2,2,2,2),14,eps,badv)
     return
     end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     subroutine cfone(a,b,ii,eps,badv)
     double precision a,b,eps
     integer ii
     logical badv(14)
     badv(ii)=(abs(a-b).gt.(a*eps))
!    if(badv(ii)) write(6,*) "In cfone moment discripancy",ii,a, b
     return
     end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     subroutine corrcum(kk2,kk4)
     double precision kk2(2,2),kk4(2,2,2,2)
     kk2(1,1)=kk2(1,1)-1/12.0d0
     kk2(2,2)=kk2(2,2)-1/12.0d0
     kk4(1,1,1,1)=kk4(1,1,1,1)+1/12.0d1
     kk4(2,2,2,2)=kk4(2,2,2,2)+1/12.0d1
     return
     end
