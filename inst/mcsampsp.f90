     subroutine mcsamp(m1,n1,m2,n2,delta,nsamp)
implicit none
     integer m1,n1,m2,n2,n
     double precision delta
     integer(kind=8) nsamp
     integer(kind=8),dimension(:,:),allocatable::out
     logical done
     done=.false.
     if(m1<0) then
        write(6,*) "No reason for negative sample size here"
        stop
     end if
     n=m1+m2+n1+n2
     if(n.gt.0) then
        allocate(out(n1*m1+1,(n1+n2)*(m1+m2)+1))
        call randmat(nsamp,m1,n1,m2,n2,out,delta)
        call dumpout1(out,m1,n1,m2,n2,"m",delta)
        deallocate(out)
     else
        done=.true.
     end if
     return
     end

     subroutine randmat(nsamp,m1,n1,m2,n2,out,delta)
implicit none
     integer m1,n1,m2,n2,jj,u1,u2,kk
     real snorm
     double precision delta
     integer (kind=8) out(n1*m1+1,(n1+n2)*(m1+m2)+1),nsamp,ii
     double precision,allocatable,dimension(:):: xvec,yvec
     allocate(xvec(m1+m2),yvec(n1+n2))
     call zerou(out,m1,n1,m2,n2)
     do ii=1,nsamp
        u1=0;u2=0
        do jj=1,m1+m2
           xvec(jj)=dble(snorm())
        end do 
        do jj=1,n1+n2
           yvec(jj)=dble(snorm())-delta
        end do
        do jj=1,m1+m2
           do kk=1,n1+n2
              if(yvec(kk).gt.xvec(jj)) then
                 u2=u2+1
                 if((jj.le.m1).and.(kk.le.n1)) u1=u1+1
              end if
           end do
        end do
        out(u1+1,u2+1)=out(u1+1,u2+1)+1
     end do
     return
     deallocate(xvec,yvec)
     end

