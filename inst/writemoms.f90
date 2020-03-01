   subroutine writemoms(mm0,mm1,mm2,mm3,mm4,cumu)
implicit none
   double precision mm0,mm1(2),mm2(2,2),mm3(2,2,2),mm4(2,2,2,2)
   integer ii,jj,kk,ll
   character(len=30) fmt
   character(len=2) pref
   character(len=9) lab
   logical cumu
   if(cumu) then
      pref="kk"
      lab="cumulants"
   end if
   if(.not.cumu) then
      pref="mm"
      lab="moments  "
   end if
   fmt='(100(a2,a1,    5x,f12.3,1x))'
   write(6,fmt) pref,"0",mm0
   fmt(12:16)="1i1,4"
   write(6,*) "First",lab
   write(6,fmt) (pref," ",ii,mm1(ii),ii=1,2)
   fmt(12:16)="2i1,3"
   write(6,*) "Second",lab
   do ii=1,2
      write(6,fmt) (pref," ",ii,jj,mm2(ii,jj),jj=ii,2)
   end do
   fmt(12:16)="3i1,2"
   write(6,*) "Third",lab
   do ii=1,2
      do jj=ii,2
         write(6,fmt) (pref," ",ii,jj,kk,mm3(ii,jj,kk),kk=jj,2)
      end do 
   end do
   fmt(12:16)="4i1,1"
   write(6,*) "Fourth",lab
   do ii=1,2
      do jj=ii,2
         do kk=jj,2
            write(6,fmt) (pref," ",ii,jj,kk,ll,mm4(ii,jj,kk,ll),ll=kk,2)
         end do 
      end do 
   end do
   return
   end
