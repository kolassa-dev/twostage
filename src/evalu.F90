     subroutine evalu(xv,xvc,ok,okc)
     use samplespace
implicit none
     double precision xv(2),xvc(2),ok,okc
!    integer(kind=8) cnt(3)
     double precision cnt(3)
     integer ii
     cnt(1)=0
     cnt(2)=0
     cnt(3)=0
#ifdef DEBUGME
     write(6,*) "In evalu npts=",npts
#endif
     do ii=1,npts
        if((u1(ii).gt.xv(1)).and.(u2(ii).gt.xv(2))) cnt(1)=cnt(1)+outv(ii)
        if((u1(ii).gt.xvc(1)).and.(u2(ii).gt.xvc(2))) cnt(2)=cnt(2)+outv(ii)
        cnt(3)=cnt(3)+outv(ii)
#ifdef DEBUGME
        write(6,*) "ii",ii,"cnt",cnt
#endif
     end do
     ok=dble(cnt(1))/dble(cnt(3))
     okc=dble(cnt(2))/dble(cnt(3))
#ifdef DEBUGME
     write(6,*) "In evalu okc=",okc
#endif
     return
     end
