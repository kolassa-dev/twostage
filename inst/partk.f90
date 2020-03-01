!######################################
     integer function partk(m1,n1,m2,n2,perm,parts,nparts)
!F &=\isum\jsum\ksum\lsum I_{i j} I_{k \ell} U_2\cr
implicit none
     integer m1,n1,m2,n2,perm(m1+n1+m2+n2),aa,ii,jj,ufi,nparts,ll,bb,hh
     double precision monster(2)
     double precision parts(nparts)
     double precision,allocatable,dimension(:)::partx
     integer,allocatable,dimension(:,:)::imat
     logical,allocatable,dimension(:):: ux,uy
     allocate(ux(m1+m2),uy(n1+n2),partx(nparts))
     partk     =0; monster(2)=0
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
     partk     =0;monster(2)=0
     do ii=1,m1+m2
        do jj=1,n1+n2
           do ll=1,n1+n2
              if(ll.ne.jj) then
                 do hh=1,n1+n2
                    if((hh.ne.jj).and.(hh.ne.ll)) then
                       partk     =partk     +imat(ii,jj)*imat(ii,ll)*imat(ii,hh)
                       ux(ii)=.false.;uy(ll)=.false.;uy(hh)=.false.;uy(jj)=.false.
                       partx(1)=partx(1)+imat(ii,jj)*imat(ii,ll)*imat(ii,hh)*ufi(m1+m2,n1+n2,imat,ux,uy)
                       ux(ii)=.true.;uy(ll)=.true.;uy(jj)=.true.;uy(hh)=.true.
                       do aa=1,m1+m2
                          if(aa.ne.ii) then
                             partx(2)=partx(2)+imat(ii,jj)*imat(ii,ll)*imat(ii,hh)*(imat(aa,jj)+imat(aa,ll)+imat(aa,hh))
                          end if
                       end do!aa
                       do bb=1,n1+n2
                          if((bb.ne.jj).and.(bb.ne.ll).and.(bb.ne.hh)) then
                             partx(3)=partx(3)+imat(ii,jj)*imat(ii,ll)*imat(ii,hh)*imat(ii,bb)
                          end if
                       end do!bb
                       partx(4)=partx(4)+imat(ii,jj)*imat(ii,ll)*imat(ii,hh)*3
                    end if!hh
                 end do !hh
              end if!ll
           end do!ll
        end do!jj
     end do!ii
!    do ii=1,m1+m2
!       write(6,*) "imat(",ii,",)",(imat(ii,jj),jj=1,n1+n2)
!    end do!ii
     partk     =partk     *ufi(m1+m2,n1+n2,imat,ux,uy)
     do aa=1,nparts
        parts(aa)=parts(aa)+partx(aa)
        monster(2)=monster(2)+partx(aa)
     end do
     if(partk     .ne.monster(2)) then
        write(6,*) "partk partk     ",partk     ,"monster(2)",monster(2),"This many parts",nparts,"These parts",partx
     end if
!    write(6,*) "monster",partk     ,monster(2)
     deallocate(ux,uy,imat,partx)
     return
     end
