!######################################
     integer function parte(m1,n1,m2,n2,perm,parts,nparts)
!\isum\jsum\ksum\lsum I_{i j} I_{k j} I_{k \ell} U_2
implicit none
     integer m1,n1,m2,n2,perm(m1+n1+m2+n2),aa,ii,jj,kk,ufi,nparts,ll,bb
     double precision monster(2)
     integer lead
     double precision parts(nparts)
     double precision,allocatable,dimension(:)::partx
     integer,allocatable,dimension(:,:)::imat
     logical,allocatable,dimension(:):: ux,uy
     allocate(ux(m1+m2),uy(n1+n2),partx(nparts))
     parte     =0; monster(2)=0
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
!    do ii=1,m1+m2
!       write(6,*) "imat",(imat(ii,jj),jj=1,n1+n2)
!    end do
     parte     =0;monster(2)=0
     do ii=1,m1+m2
        do jj=1,n1+n2
           do kk=1,m1+m2
              if(kk.ne.ii) then
                 do ll=1,n1+n2
                    if(ll.ne.jj) then
                       lead=imat(ii,jj)*imat(kk,jj)*imat(kk,ll)
!                      write(6,*) "lead",lead
                       parte     =parte     +lead
                       ux(ii)=.false.;ux(kk)=.false.;uy(ll)=.false.;uy(jj)=.false.
!    write(6,*) "ux",ux,"uy",uy
                       partx(1)=partx(1)+lead*ufi(m1+m2,n1+n2,imat,ux,uy)
                       ux(ii)=.true.;ux(kk)=.true.;uy(ll)=.true.;uy(jj)=.true.
                       do aa=1,m1+m2
                          if((aa.ne.ii).and.(aa.ne.kk)) then
                             partx(2)=partx(2)+lead*(imat(aa,jj)+imat(aa,ll))
                          end if !aa
                       end do!aa
                       do bb=1,n1+n2
                          if((bb.ne.jj).and.(bb.ne.ll)) then
                             partx(3)=partx(3)+lead*(imat(ii,bb)+imat(kk,bb))
                          end if !aa
                       end do!aa
                       partx(4)=partx(4)+lead*3
                       partx(5)=partx(5)+lead*imat(ii,ll)
                    end if!ll
                 end do!ll
              end if
           end do!kk
        end do!jj
     end do!ii
!    write(6,*) "ux",ux,"uy",uy
     parte     =parte     *ufi(m1+m2,n1+n2,imat,ux,uy)
     do aa=1,nparts
        parts(aa)=parts(aa)+partx(aa)
        monster(2)=monster(2)+partx(aa)
     end do
     if(parte     .ne.monster(2)) then
        write(6,*) "parte parte     ",parte     ,"monster(2)",monster(2),"This many parts",nparts,"These parts",partx
     end if
     deallocate(ux,uy,imat,partx)
     return
     end
