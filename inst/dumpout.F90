!##############################################################
     subroutine dumpout(out,m1,n1,m2,n2,cnt)
implicit none
     integer m1,n1,m2,n2
#ifdef BIGINT
     integer(kind=8) cnt,out(n1*m1+1,(n1+n2)*(m1+m2)+1)
#else
     integer(kind=8) cnt,out(n1*m1+1,(n1+n2)*(m1+m2)+1)
#endif
     integer ii,jj
     character(LEN=20) ::  fmt
!    integer kk
     fmt="(   (f8.4,1x)      )"
     write(fmt(2:4),"(i3)") n1*m1+1
!    kk=0
     do jj=1,(n1+n2)*(m1+m2)+1
!       do ii=1,n1*m1+1
!          kk=kk+out(ii,jj)
!       end do
        write(6,fmt) (dble(out(ii,jj))/cnt,ii=1,n1*m1+1)
     end do
!    write(6,*) "kk,cnt",kk,cnt
     return
     end
