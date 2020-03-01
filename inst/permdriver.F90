     program permdriver
implicit none
#ifdef BIGINT
     integer(kind=8),dimension(:,:),allocatable::out
     integer(kind=8) cnt
#else
     integer,dimension(:,:),allocatable::out
     integer cnt
#endif
     integer m1,n1,m2,n2,n,efg,toobig
     integer u1,u2!Placeholders for readinput.
     double precision delta,alpha(2)
     logical done
     done=.false.
     open(38,file="lastinput")
     close(38)
     do while(.not.done)
        delta=0.0d0!Forces delta to be reread in readinput.
        u1=-1!Suppresses reading of u1 and u2, and forces reading of delta
        call readinput(u1,u2,m1,n1,m2,n2,delta,alpha)
        call fillps(delta)
        n=m1+m2+n1+n2
        if(n.gt.0) then
           allocate(out(n1*m1+1,(n1+n2)*(m1+m2)+1))
           if(abs(delta).lt.1.0d-8) then
              call allperm(m1,n1,m2,n2,delta,cnt,out,efg,toobig)
              if(efg.gt.0) then
                 write(6,*) "Delta too large in allperm"; stop
              end if
              if(toobig.gt.0) then
                 write(6,*) "Too many permutations in allperm"; stop
              end if
           else
#ifdef BIGINT
              cnt=10000000_8
#else
              cnt=1000000000_8
#endif
              call mcsamp(m1,n1,m2,n2,delta,cnt)
           end if
           write(6,*) "About to call dumpout1"
           call dumpout1(out,m1,n1,m2,n2,"u",delta)
           deallocate(out)
        else
           done=.true.
        end if
     end do
     end
