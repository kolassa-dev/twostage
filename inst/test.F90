   program test
implicit none
   integer u1,u2,m1,n1,m2,n2,dd
   logical debug
   character*20 filen
   double precision scrap
!  double precision wilding
#ifdef DOINT   
   integer(kind=8) wilding, wildingo
#else
   double precision wilding, wildingo
#endif
   debug=.true.
   u1=1!force readinput to read us as well as ns and ms
   scrap=-1.0d0!scrap input to readinput forcing double not to be read.
   open(38,file="lastinput");close(38)!Start with empty file lastinput
   call readinput(u1,u2,m1,n1,m2,n2,scrap)
   if(debug) then
      call mkfn(m1,n1,m2,n2,filen,"u",0.00d0)
      write(6,*) "Filename",filen
   end if
   dd=0
   write(6,*) wilding(u1,u2,m1,n1,m2,n2,dd,debug)
   call wildings(u1,u2,m1,n1,m2,n2,wildingo,debug)
   write(6,*) "wildingo",wildingo
!  call mix3(m1,n2,m2,n2,u112,u122)
!  write(6,*) "u112,u122",u112,u122
   end
