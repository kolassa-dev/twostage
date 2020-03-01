   program test1
implicit none
   integer u1,u2,m1,n1,m2,n2
   integer lu1,uu1,lu2,uu2
   logical debug,dump
!  double precision wilding
#ifdef DOINT   
   integer(kind=8) wilding, wildingo,tot
#else
   double precision wilding, wildingo,tot,eps
#endif
   double precision mm0,mm1(2),mm2(2,2),mm3(2,2,2),mm4(2,2,2,2),umoms3(2,2,2),umoms4(2,2,2,2)
   character(len=1),allocatable,dimension(:):: pic
   dump=.false.
   eps=1.0d-4
   u1=1!force readinput to read us as well as ns and ms
   mm0=-1.0d0!scrap space given to readinput.  Forces double precision input not to be read.
   open(38,file="lastinput");close(38)!Start with empty file lastinput
   call readinput(u1,u2,m1,n1,m2,n2,mm0)
   if((u1.lt.0).or.(u2.lt.0)) then
      debug=.true.
      lu1=abs(u1); uu1=lu1; lu2=abs(u2); uu2=lu2
   else
      debug=.false.
      lu1=0;uu1=m1*n1;lu2=0;uu2=(m1+m2)*(n1+n2)
   end if
!  write(6,*) debug
   allocate(pic((m1+m2)*(n1+n2)+1))
   tot=0.0d0
   if(dump) write(30,'(a2,1x,a2,1x,a8,1x,a8)') "u1","u2","p*24","u1 u2 u2"
   do u1=lu1,uu1
      do u2=max(lu2,u1),uu2
         call wildings(u1,u2,m1,n1,m2,n2,wildingo,debug)
         tot=tot+wildingo
         if(wildingo>0.0d0) then
            pic(u2+1)="*"
         else
            pic(u2+1)="-"
         end if
         if(dump) write(30,'(i2,1x,i2,1x,f8.4,1x,f8.4)') u1,u2,wildingo*24,u1*u2**2*1.
         call moments(u1,u2,wildingo,mm0,mm1,mm2,mm3,mm4,4)
      end do
!     write(6,*) pic
   end do
!  call writemoms(mm0,mm1,mm2,mm3,mm4)
!  write(6,*) u4(m1,n1)
!  write(6,*) u4(m1+m2,n1+n2)
!  write(6,*) "tot",tot
   call mix3(m1,n2,m2,n2,umoms3)
   call mix4(m1,n2,m2,n2,umoms4)
   if(abs(umoms3(1,1,1)-mm3(1,1,1)).gt.eps) write(6,*) "111 fails"
   if(abs(umoms3(2,2,2)-mm3(2,2,2)).gt.eps) write(6,*) "222 fails"
   if(abs(umoms3(1,2,2)-mm3(1,2,2)).gt.eps) write(6,*) "122 fails"
   if(abs(umoms4(1,1,1,1)-mm4(1,1,1,1)).gt.eps) write(6,*) "1111 fails"
   if(abs(umoms4(2,2,2,2)-mm4(2,2,2,2)).gt.eps) write(6,*) "2222 fails"
   if(abs(umoms4(1,2,2,2)-mm4(1,2,2,2)).gt.eps) write(6,*) "1222 fails"
   end
