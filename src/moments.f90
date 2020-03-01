   subroutine moments(u1,u2,p,mm0,mm1,mm2,mm3,mm4,nmom)
! Calculate exact moments from input enumeration of sample space.
implicit none
   logical,allocatable,save::first
   double precision,save::lastm(2)
   double precision mm0,mm1(2),mm2(2,2),mm3(2,2,2),mm4(2,2,2,2),p,uv(2)
   integer u1,u2,ii,jj,kk,ll,nmom
!  write(6,*) "At the beginning of moments"
   if(p.lt.0) then
      lastm(1)=mm1(1)
      lastm(2)=mm1(2)
      if(allocated(first)) deallocate(first)
!     write(6,*) "Reset first"
   else
      if(nmom.gt.0) then
         uv(1)=u1; uv(2)=u2
      else
         uv(1)=u1-lastm(1); uv(2)=u2-lastm(2)
      end if
      if(.not.allocated(first)) then
!        write(6,*) "In moments First unallocated; allocating and resetting array"
         allocate(first)
         mm0=0.0d0;
         do ii=1,2
            mm1(ii)=0.0d0
            do jj=1,2
               mm2(ii,jj)=0.0d0
               do kk=1,2
                  mm3(ii,jj,kk)=0.0d0
                  do ll=1,2
                     mm4(ii,jj,kk,ll)=0.0d0
                  end do
               end do
            end do
         end do
      end if
      mm0=mm0+p
      do ii=1,2
         if(nmom.gt.0) mm1(ii)=mm1(ii)+uv(ii)*p
         do jj=ii,2
            mm2(ii,jj)=mm2(ii,jj)+uv(ii)*uv(jj)*p
            do kk=jj,2
               mm3(ii,jj,kk)=mm3(ii,jj,kk)+uv(ii)*uv(jj)*uv(kk)*p
               do ll=kk,2
                  mm4(ii,jj,kk,ll)=mm4(ii,jj,kk,ll)+uv(ii)*uv(jj)*uv(kk)*uv(ll)*p
               end do
            end do 
         end do 
      end do
   end if
!  write(6,*) "At the beginning of moments"
   return
   end
