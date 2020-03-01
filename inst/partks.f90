!######################################
     integer function partks(m1,n1,m2,n2,perm,parts,nparts)
implicit none
     integer m1,n1,m2,n2,perm(m1+n1+m2+n2),aa,ii,jj,ufi,nparts,bb,gg,lead,kk
     double precision monster(2)
     double precision parts(nparts)
     double precision,allocatable,dimension(:)::partx
     integer,allocatable,dimension(:,:)::imat
     logical,allocatable,dimension(:):: ux,uy
     allocate(ux(m1+m2),uy(n1+n2),partx(nparts))
     partks    =0; monster(2)=0
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
     partks    =0;monster(2)=0
     do ii=1,m1+m2
        do jj=1,n1+n2
           do kk=1,m1+m2
              if(kk.ne.ii) then
                 do gg=1,m1+m2
                    if((gg.ne.ii).and.(gg.ne.kk)) then
                       lead=imat(ii,jj)*imat(kk,jj)*imat(gg,jj)
                       partks    =partks    +lead
                       ux(ii)=.false.;ux(kk)=.false.;ux(gg)=.false.;uy(jj)=.false.
                       partx(1)=partx(1)+lead*ufi(m1+m2,n1+n2,imat,ux,uy)
                       ux(ii)=.true.;ux(kk)=.true.;ux(gg)=.true.;uy(jj)=.true.
                       do aa=1,m1+m2
                          if((aa.ne.ii).and.(aa.ne.kk).and.(aa.ne.gg)) then
                             partx(2)=partx(2)+lead*imat(aa,jj)
                          end if
                       end do!aa
                       do bb=1,n1+n2
                          if(bb.ne.jj) then
                             partx(3)=partx(3)+lead*(imat(ii,bb)+imat(kk,bb)+imat(gg,bb))
                          end if
                       end do!bb
                       partx(4)=partx(4)+lead*3
                    end if!gg
                 end do !gg
              end if!kk
           end do!kk
        end do!jj
     end do!ii
!    do ii=1,m1+m2
!       write(6,*) "imat(",ii,",)",(imat(ii,jj),jj=1,n1+n2)
!    end do!ii
     partks    =partks    *ufi(m1+m2,n1+n2,imat,ux,uy)
     do aa=1,nparts
        parts(aa)=parts(aa)+partx(aa)
        monster(2)=monster(2)+partx(aa)
     end do
     if(partks    .ne.monster(2)) then
        write(6,*) "partk partks    ",partks    ,"monster(2)",monster(2),"This many parts",nparts,"These parts",partx
     end if
!    write(6,*) "monster",partks    ,monster(2)
     deallocate(ux,uy,imat,partx)
     return
     end
