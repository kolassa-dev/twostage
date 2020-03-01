module fmtout
     character(len=20) :: outfmt = "(i3,1x,i3,1x,i20   )"
end module fmtout
!################################################
     subroutine dumpout1(out,m1,n1,m2,n2,pref,delta)
use fmtout
implicit none
     double precision delta
     integer m1,n1,m2,n2
#ifdef BIGINT
     integer(kind=8) out(n1*m1+1,(n1+n2)*(m1+m2)+1)
#else
     integer out(n1*m1+1,(n1+n2)*(m1+m2)+1)
#endif
     integer ii,jj
     character(len=1):: pref
     character(len=20):: filen
     call mkfn(m1,n1,m2,n2,filen,pref,delta)
     write(6,*) "Writing to ",filen
     open(35,file=filen)
     do ii=1,n1*m1+1
        do jj=1,(n1+n2)*(m1+m2)+1
           write(35,outfmt) ii-1,jj-1,out(ii,jj)
        end do
     end do
     close(35)
     return
     end
