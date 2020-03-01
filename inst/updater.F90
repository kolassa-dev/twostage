   subroutine updater(result,out,mult,mult1)
   integer mult
#ifdef BIGINT
   integer(kind=8) result,out,mult1
#else
   integer result,out,mult1
#endif
#ifdef DEBUGME
   write(6,*) "In updater mult=",mult,"out=",out,"result=",result
#endif
!  if(dble(huge(result)).lt.(dble(result)+dble(out)*dble(mult)*dble(mult1))) then
!     write(6,*) "Integer too big"
!     stop
!  end if
   result=result+out!*int(mult,8)*mult1
!  write(6,*) int(mult,8)*mult1
   return
   end
