!######################################
     integer function partd(m1,n1,m2,n2,perm,parts,nparts)
!F &=\isum\jsum\ksum\lsum I_{i j} I_{k \ell} U_2\cr
implicit none
     integer m1,n1,m2,n2,perm(m1+n1+m2+n2),aa,ii,jj,kk,ufi,nparts,ll,bb,hh,gg
     double precision monster(2)
     double precision parts(nparts)
     double precision,allocatable,dimension(:)::partx
     integer,allocatable,dimension(:,:)::imat
     logical,allocatable,dimension(:):: ux,uy
     allocate(ux(m1+m2),uy(n1+n2),partx(nparts))
     do aa=1,nparts
        partx(aa)=0
     end do
     if(parts(1).lt.0) then
        do aa=1,nparts
           parts(aa)=0
        end do
     end if
     do aa=1,m1+m2
        ux(aa)=.true.
     end do
     do bb=1,n1+n2
        uy(bb)=.true.
     end do
     allocate(imat(m1+m2,n1+n2))
     call mkimat(m1,n1,m2,n2,perm,imat)
     partd     =0;monster(2)=0
     do ii=1,m1+m2
        do jj=1,n1+n2
           do kk=1,m1+m2
              if(kk.ne.ii) then
                 do ll=1,n1+n2
                    if(ll.ne.jj) then
                       do gg=1,m1+m2
                          if((gg.ne.ii).and.(gg.ne.kk)) then
                             do hh=1,n1+n2
                                if((hh.ne.jj).and.(hh.ne.ll)) then
                                   partd     =partd     +imat(ii,jj)*imat(kk,ll)*imat(gg,hh)
                                   ux(ii)=.false.;ux(kk)=.false.;uy(ll)=.false.;uy(jj)=.false.;&
                                            ux(gg)=.false.;uy(hh)=.false.
                                   partx(1)=partx(1)+imat(ii,jj)*imat(kk,ll)*imat(gg,hh)*ufi(m1+m2,n1+n2,imat,ux,uy)
                                   ux(ii)=.true.;ux(kk)=.true.;uy(ll)=.true.;uy(jj)=.true.;ux(gg)=.true.;uy(hh)=.true.
                                   do aa=1,m1+m2
                                      if((aa.ne.ii).and.(aa.ne.kk).and.(aa.ne.gg)) then
                                         partx(2)=partx(2)+imat(ii,jj)*imat(kk,ll)*imat(gg,hh)*&
                                            (imat(aa,jj)+imat(aa,ll)+imat(aa,hh))
                                      end if
                                   end do!aa
                                   do bb=1,n1+n2
                                      if((bb.ne.jj).and.(bb.ne.ll).and.(bb.ne.hh)) then
                                         partx(3)=partx(3)+imat(ii,jj)*imat(kk,ll)*imat(gg,hh)*&
                                            (imat(ii,bb)+imat(kk,bb)+imat(gg,bb))
!                                        write(6,*) "ii,jj,kk,ll,bb",ii,jj,kk,ll,bb,"partx(3)",partx(3)
                                      end if
                                   end do!aa
                                   partx(4)=partx(4)+3*imat(ii,jj)*imat(kk,ll)*imat(gg,hh)
                                   partx(5)=partx(5)+imat(ii,jj)*imat(kk,ll)*imat(gg,hh)*&
                                       (imat(ii,ll)+imat(ii,hh)+imat(kk,jj)+imat(kk,hh)+imat(gg,jj)+imat(gg,ll))
                                end if!hh
                             end do !hh
                          end if!gg
                       end do!gg
                    end if!ll
                 end do!ll
              end if
           end do!kk
        end do!jj
     end do!ii
     partd     =partd     *ufi(m1+m2,n1+n2,imat,ux,uy)
     do aa=1,nparts
        parts(aa)=parts(aa)+partx(aa)
        monster(2)=monster(2)+partx(aa)
     end do
     if(partd     .ne.monster(2)) then
        write(6,*) "partd partd     ",partd     ,"monster(2)",monster(2),"This many parts",nparts,"These parts",partx
     end if
!    write(6,*) "monster",partd     ,monster(2)
     deallocate(ux,uy,imat,partx)
     return
     end
