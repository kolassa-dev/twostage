!################################################
     subroutine readout(outv,u1,u2,npts,m1,m2,n1,n2,cnt,delta,efg,filen)
implicit none
     integer npts,efg,ll,ii
     double precision cnt
#ifdef BIGINT
     integer(kind=8) outv(max(npts,1)),mcsamp
     integer(kind=8),parameter::zero=0_8
#ifdef DOGCF
     integer(kind=8),allocatable,dimension(:)::copy,scrap
     integer(kind=8):: igcd,bignpts
#endif
#else
     integer outv(max(npts,1)),mcsamp
     integer,parameter::zero=0,one=1
#ifdef DOGCF
     integer,allocatable,dimension(:)::copy,scrap
     integer:: igcd,bignpts
#endif
#endif
     integer u1(max(npts,1)),u2(max(npts,1))
     integer m1,m2,n1,n2
     double precision delta
     character(len=20) filen
     character(len=1) pref
#ifdef DEBUGME
     write(6,*) "Entered readout"
#endif
     mcsamp=10000000
     pref="u"
     efg=0
     if(abs(delta).gt.1.0d-6) pref="m"
     if((m1.ge.0).and.(n1.ge.0).and.(m2.ge.0).and.(n2.ge.0)) then
#ifdef DEBUGME
        write(6,*) "In readout Input sample sizes large enough to go in readout"
#endif
        call mkfn(m1,n1,m2,n2,filen,pref,delta)
#ifdef DEBUGME
        if(npts.eq.0) write(6,'(a14,1x,a20,a6,f8.6)') "Examining file ",filen," delta=",delta
#endif
        open(35,file=filen,status='old',iostat=efg)
        if(efg.eq.0) then
#ifdef DEBUGME
           write(6,*) "In readout in branch when desired file exists; npts",npts,"delta",delta,"filen",filen
#endif
           ll=0
           if(npts.eq.0) then
              do while(ll.eq.0)
                 read(35,*,iostat=ll)
                 if(ll.eq.0) npts=npts+1
              end do
#ifdef DEBUGME
              write(6,*) "In readout found this many lines",npts
#endif
           else
              cnt=zero
#ifdef DOGCF
              allocate(copy(npts),scrap(npts))
              bignpts=zero
#endif
              do ii=1,npts
!                write(6,*) "ii",ii,"npts",npts
!                read(35,'(i3,1x,i3,1x,i15   )') u1(ii),u2(ii),outv(ii)
                 read(35,*) u1(ii),u2(ii),outv(ii)
                 cnt=cnt+outv(ii)
#ifdef DOGCF
                 if(outv(ii).ne.zero) then
                    bignpts=bignpts+one
                    copy(bignpts)=outv(ii)
                 end if
#endif
              end do
#ifdef DOGCF
              call gcdn(bignpts,copy,scrap,igcd)
              write(6,*) "gcd",igcd,"bignpts",bignpts
              deallocate(copy,scrap)
#endif
!             write(6,*) "u1",(u1(ii),ii=1,npts)
           end if
!          write(6,*) "In readout in branch when desired file exists just completed reading"
        else
#ifdef DEBUGME
           write(6,*) "In readout in branch when desired file does not exist; npts",npts
#endif
           if(npts.eq.0) then
              npts=(m1*n1+1)*((m1+m2)*(n1+n2)+1)
              efg=0
           else
!             write(6,*) "In readout Input file not found",efg,"m1,m2,n1,n2",m1,m2,n1,n2,"npts",npts
              write(6,*) "Calling randmat1"
!             open(39,file="todolist",position="append")
!             write(39,*) m1,n1,m2,n2
!             close(39)
              call randmat1(mcsamp,m1,n1,m2,n2,outv,u1,u2,delta)
              cnt=mcsamp
              efg=0
!             stop
           end if
        end if
!       write(6,*) "In readout outv",outv
        close(35)
     end if
     return
     end

!#############################################################################
     subroutine randmat1(nsamp,m1,n1,m2,n2,out,u1v,u2v,delta)
implicit none
     integer m1,n1,m2,n2,jj,u1,u2,kk
     real snorm
     double precision delta
     integer u1v((n1*m1+1)*((n1+n2)*(m1+m2)+1)),u2v((n1*m1+1)*((n1+n2)*(m1+m2)+1))
     double precision,allocatable,dimension(:):: xvec,yvec
#ifdef BIGINT
     integer(kind=8) out((n1*m1+1)*((n1+n2)*(m1+m2)+1)),nsamp,ii,zero
     zero=0_8
#else
     integer out((n1*m1+1)*((n1+n2)*(m1+m2)+1)),nsamp,ii,zero
     zero=0
#endif
     call rndstart()
     allocate(xvec(m1+m2),yvec(n1+n2))
!    write(6,*) "m1,n1,m2,n2",m1,n1,m2,n2
     do u1=0,n1*m1
        do u2=0,(n1+n2)*(m1+m2)
!          write(6,*) u1,u2,u1*((n1+m1)*(n2+m2)+1)+u2+1
           out(u1*((n1+n2)*(m1+m2)+1)+u2+1)=0
           u1v(u1*((n1+n2)*(m1+m2)+1)+u2+1)=u1
           u2v(u1*((n1+n2)*(m1+m2)+1)+u2+1)=u2
        end do
     end do
     write(6,*) "In randmat1 nsamp",nsamp
     do ii=1,nsamp
        u1=0;u2=0
        do jj=1,m1+m2
           xvec(jj)=dble(snorm())
!          write(6,*) "xvec(jj)",xvec(jj)
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
        out(u1*((m1+m2)*(n1+n2)+1)+u2+1)=out(u1*((m1+m2)*(n1+n2)+1)+u2+1)+1
     end do
     do u1=0,n1*m1
        do u2=0,(n1+n2)*(m1+m2)
           jj=u1*((n1+n2)*(m1+m2)+1)+u2+1
           if(out(jj)>0) write(66,*) u1v(jj),u2v(jj),out(jj)
        end do
     end do
     return
     deallocate(xvec,yvec)
     call rndend()
     end
