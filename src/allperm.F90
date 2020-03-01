     subroutine allperm(m1,n1,m2,n2,delta,cnt,out,efg,toobig)
implicit none
     integer m1,n1,m2,n2,n,b,u1,u2
     double precision delta
#ifdef TIMEME
     integer(kind=8) start,end
#endif
#ifdef BIGINT
     integer(kind=8) cnt, out(n1*m1+1,(n1+n2)*(m1+m2)+1)
     integer(kind=8),parameter::one=1_8,zero=0_8
#else
     integer cnt, out(n1*m1+1,(n1+n2)*(m1+m2)+1)
     integer,parameter::one=1,zero=0
#endif
     integer efg,toobig
     integer,dimension(:),allocatable::perm
     double precision dperm
     logical predicted
#ifdef ENABLECHECK
     double precision uv(4)
     integer ii,nparth,npartg,npartf,npartd,npartk,nparte,npartc,npartl
     logical check,star
     double precision,allocatable,dimension(:):: hvparts,gvparts,fvparts,dvparts,kvparts,evparts,cvparts,lvparts
     double precision,allocatable,dimension(:):: hreal,greal,freal,dreal,kreal,ereal,creal
     double precision aaa,realph,realphs,realpgs,realpg,realpcs,realpc,realpks,realpk,realpd,realpe,realpf
     integer parths, partgs, partcs, partks, parth, partg, partc, partk, partf, partd, parte,partl
     integer phs, pgs, pcs, pks, ph, pg, pc, pk, pf, pd, pe,pl
!    write(6,*) "Checking mark 1"
     check=.true.
     star=.false.
     nparth=14
     npartf=5
     npartd=5
     npartc=5
     nparte=5
     npartk=4
     npartg=4
     npartl=5
     if(check) then
        call fillps(delta)
        allocate(hvparts(nparth),gvparts(npartg),fvparts(npartf),dvparts(npartd),&
         kvparts(npartk),evparts(nparte),cvparts(npartc),lvparts(npartl))
         allocate(hreal(nparth),greal(npartg),freal(npartf),dreal(npartd),kreal(npartk),ereal(nparte),creal(npartc))
        hvparts(1)=-1.0
        gvparts(1)=-1.0
        fvparts(1)=-1.0
        dvparts(1)=-1.0
        kvparts(1)=-1.0
        evparts(1)=-1.0
        cvparts(1)=-1.0
        lvparts(1)=-1.0
     end if
#else
!    write(6,*) "Not checking mark 1"
#endif
     efg=0
     if(abs(delta).gt.1.0d-6) efg=1
     if(efg.eq.0) then
        predicted=.false.
        b=1
        allocate(perm(m1+m2+n1+n2))
        call labfill(perm,m1,n1,m2,n2,dperm)
!       write(6,*) "Number of permutations",anint(dperm),"perm",perm
!       call ifill(perm,m1,n1,m2,n2)
        call zerou(out,m1,n1,m2,n2)
        n=m1+n1+m2+n2
!       write(6,*) "Mark 1, n",n
        cnt=zero
#ifdef TIMEME
        start=time8()
#endif
!       write(6,*) "Mark 2"
#ifdef STANDALONE
        open(39,file="allperm.log",position="append")
        write(39,*) m1,n1,m2,n2
!       write(39,*) "Starting time",ctime(start)
        close(39)
#endif
        toobig=0
        do while((n.gt.0).and.(toobig.eq.0))
!          write(6,*) "Mark 3"
           cnt=cnt+one
!          write(6,*) "cnt",cnt
           if(((.01*dperm)<cnt).and.(.not.predicted)) then
!             end=time8()
#ifdef STANDALONE
              open(39,file="allperm.log",position="append")
!             write(39,*) "Predict end at ",ctime(end+99*(end-start))
              close(39)
#endif
              predicted=.true.
           end if
           toobig=0
           if(cnt.ge.huge(cnt)) toobig=1
           if(toobig.eq.0) then
              call mwwlt(perm,m1,n1,m2,n2,u1,u2)
!             call mwwit(perm,m1,n1,m2,n2,u1,u2)
#ifdef ENABLECHECK
              if(check) then
                 if(star) then
                    phs= parths(m1,n1,m2,n2,perm,hvparts,nparth)
                    pgs= partgs(m1,n1,m2,n2,perm,gvparts,npartg)
                    pcs= partcs(m1,n1,m2,n2,perm,cvparts,npartc)
                    pks= partks(m1,n1,m2,n2,perm,kvparts,npartk)
                 else
                    ph= parth(m1,n1,m2,n2,perm,hvparts,nparth)
                    pg= partg(m1,n1,m2,n2,perm,gvparts,npartg)
                    pc= partc(m1,n1,m2,n2,perm,cvparts,npartc)
                    pk= partk(m1,n1,m2,n2,perm,kvparts,npartk)
                 end if
                 pf= partf(m1,n1,m2,n2,perm,fvparts,npartf)
                 pd= partd(m1,n1,m2,n2,perm,dvparts,npartd)
                 pe= parte(m1,n1,m2,n2,perm,evparts,nparte)
                 pl= partl(m1,n1,m2,n2,perm,lvparts,npartl)
              end if
#endif
              out(u1+1,u2+1)=out(u1+1,u2+1)+one
!             write(6,'(100(i3,1x))') (perm(ii),ii=1,n),u1,u2
              call nextp(perm,n,b)
!             write(6,*) "Mark 4"
           endif
        end do
!       write(6,*) "After loop cnt",cnt
#ifdef ENABLECHECK
        if(toobig.eq.0) then
           if(check) then
!             write(6,*) "Checking"
              if(star) then
!                write(6,*) "Doing star versions"
                 aaa= realphs(m1,n1,m2,n2,hreal,nparth)!Star version exists
                 call checkone(hreal,hvparts,nparth,cnt,"H")
                 aaa= realpgs(m1,n1,m2,n2,greal,npartg)
                 call checkone(greal,gvparts,npartg,cnt,"G")
                 aaa= realpcs(m1,n1,m2,n2,creal,npartc)!Star version exists
                 call checkone(creal,cvparts,npartc,cnt,"C")
                 aaa= realpks(m1,n1,m2,n2,kreal,npartk)!Star version exists
                 call checkone(kreal,kvparts,npartk,cnt,"K")
              else
!                write(6,*) "Doing non-star versions"
                 aaa= realph(m1,n1,m2,n2,hreal,nparth)!Star version exists
                 call checkone(hreal,hvparts,nparth,cnt,"H")
                 aaa= realpg(m1,n1,m2,n2,greal,npartg)
                 call checkone(greal,gvparts,npartg,cnt,"G")
                 aaa= realpc(m1,n1,m2,n2,creal,npartc)!Star version exists
                 call checkone(creal,cvparts,npartc,cnt,"C")
                 aaa= realpk(m1,n1,m2,n2,kreal,npartk)!Star version exists
                 call checkone(kreal,kvparts,npartk,cnt,"K")
              end if
              aaa= realpf(m1,n1,m2,n2,freal,npartf)
              call checkone(freal,fvparts,npartf,cnt,"F")
              aaa= realpd(m1,n1,m2,n2,dreal,npartd)
              call checkone(dreal,dvparts,npartd,cnt,"D")
              aaa= realpe(m1,n1,m2,n2,ereal,nparte)
              call checkone(ereal,evparts,nparte,cnt,"E")
              deallocate(hvparts,gvparts,fvparts,dvparts,kvparts,evparts,cvparts,lvparts)
              deallocate(hreal,greal,freal,dreal,kreal,ereal,creal)
           end if
           call univariate(m1,n1,m2,n2,uv)
!          write(6,*) "Univariate Moments",(uv(ii),ii=1,4)
           call univariate(m1,n1,0,0,uv)
!          write(6,*) "Univariate Moments",(uv(ii),ii=1,4)
!          write(6,*) "Mark 5"
        endif
#endif
     endif
     deallocate(perm)
!    write(6,*) "Mark 6"
     end
!################
     subroutine copyme(realv,tpo,nnn)
     integer nnn,ii
     double precision realv(nnn),tpo(nnn)
     do ii=1,nnn
        tpo(ii)=realv(ii)
     end do
     return
     end
!##############################################################
     subroutine labfill(perm,m1,n1,m2,n2,dperm)
implicit none
     integer m1,n1,m2,n2,ii
     integer perm(m1+m2+n1+n2)
     double precision dperm
     do ii=1,m1
        perm(ii)=1
     end do
     do ii=1,n1
        perm(m1+ii)=2
     end do
     do ii=1,m2
        perm(m1+n1+ii)=3
     end do
     do ii=1,n2
        perm(m1+n1+m2+ii)=4
     end do
     dperm=gamma(m1+n1+m2+n2+1.0d0)/(gamma(m1+1.0d0)*gamma(m2+1.0d0)*gamma(n1+1.0d0)*gamma(n2+1.0d0))
!    write(6,*) "dperm",dperm
     return
     end
!##############################################################
     subroutine ifill(perm,m1,n1,m2,n2)
implicit none
     integer m1,n1,m2,n2,ii
     integer perm(m1+m2+n1+n2)
     do ii=1,m1+n1+m2+n2
        perm(ii)=ii
     end do
     return
     end
!##############################################################
     subroutine mwwit(perm,m1,n1,m2,n2,u1,u2)
implicit none
     integer m1,n1,m2,n2,u1,u2
     integer perm(m1+n1+m2+n2)
     integer ii,jj
     u1=0; u2=0
     do ii=1,m1
        do jj=1,n1
           if(perm(jj+m1+m2).gt.perm(ii)) u1=u1+1
        end do
     end do
     do ii=1,m1+m2
        do jj=1,n1+n2
           if(perm(jj+m1+m2).gt.perm(ii)) u2=u2+1
        end do
     end do
     return
     end
!##############################################################
     subroutine mwwlt(perm,m1,n1,m2,n2,u1,u2)
implicit none
     integer m1,n1,m2,n2,u1,u2
     integer perm(m1+n1+m2+n2)
     integer ii,jj
     u1=0; u2=0
     do ii=1,m1+n1+m2+n2-1
        do jj=ii+1,m1+n1+m2+n2
           if((perm(ii).eq.1).and.(perm(jj).eq.2)) then
               u1=u1+1
               u2=u2+1
           end if
           if((perm(ii).eq.1).and.(perm(jj).eq.4)) u2=u2+1
           if((perm(ii).eq.3).and.(perm(jj).eq.2)) u2=u2+1
           if((perm(ii).eq.3).and.(perm(jj).eq.4)) u2=u2+1
        end do
     end do
     return
     end
!##############################################################
     subroutine zerou(out,m1,n1,m2,n2)
implicit none
     integer m1,n1,m2,n2
#ifdef BIGINT
     integer(kind=8) out(n1*m1+1,(n1+n2)*(m1+m2)+1)
     integer(kind=8),parameter:: zero=0_8
#else
     integer out(n1*m1+1,(n1+n2)*(m1+m2)+1)
     integer,parameter:: zero=0
#endif
     integer ii,jj
     do ii=1,n1*m1+1
        do jj=1,(n1+n2)*(m1+m2)+1
           out(ii,jj)=zero
        end do
     end do
     return
     end
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
!##############################################################
     subroutine mkimat(m1,n1,m2,n2,perm,imat)
     integer m1,n1,m2,n2,perm(m1+n1+m2+n2),aa,ii,jj,bb
     integer imat(m1+m2,n1+n2)
     ii=0
     jj=0
     do aa=1,m1+n1+m2+n2
        if((perm(aa).eq.1).or.(perm(aa).eq.3)) then
           ii=ii+1
           do bb=1,jj
              imat(ii,bb)=0
           end do
           do bb=jj+1,n1+n2
              imat(ii,bb)=1
           end do
        else
           jj=jj+1
        end if
     end do
!    write(6,*) "perm",(perm(aa),aa=1,n1+m1+n2+m2)
!    do aa=1,m1+m2
!       write(6,*) "imat",(imat(aa,jj),jj=1,n1+n2)
!    end do
     return
     end
