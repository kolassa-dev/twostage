!######################################
     integer function partg(m1,n1,m2,n2,perm,parts,nparts)
implicit none
     integer m1,n1,m2,n2,perm(m1+n1+m2+n2),aa,ii,jj,ufi,nparts,ll,bb
     double precision monster(2)
     integer lead
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
     do aa=1,n1+n2
        uy(aa)=.true.
     end do
     allocate(imat(m1+m2,n1+n2))
     call mkimat(m1,n1,m2,n2,perm,imat)
     partg     =0;monster(2)=0
     do ii=1,m1+m2
        do jj=1,n1+n2
           do ll=1,n1+n2
              if(ll.ne.jj) then
                 lead=imat(ii,jj)*imat(ii,ll)
                 partg     =partg     +lead
                 ux(ii)=.false.;uy(ll)=.false.;uy(jj)=.false.
                 partx(1)=partx(1)+lead*ufi(m1+m2,n1+n2,imat,ux,uy)
                 ux(ii)=.true.;uy(ll)=.true.;uy(jj)=.true.
                 do aa=1,m1+m2
                    if(aa.ne.ii) partx(2)=partx(2)+lead*(imat(aa,jj)+imat(aa,ll))
                 end do
                 do bb=1,n1+n2
                    if((bb.ne.jj).and.(bb.ne.ll)) partx(3)=partx(3)+lead*imat(ii,bb)
                 end do
                 partx(4)=partx(4)+lead*2
              end if
           end do
        end do
     end do
!    write(6,*) "ux",ux,"uy",uy
     partg     =partg     *ufi(m1+m2,n1+n2,imat,ux,uy)
     do aa=1,nparts
        parts(aa)=parts(aa)+partx(aa)
        monster(2)=monster(2)+partx(aa)
     end do
!    write(31,*) partx(2)
     if(partg     .ne.monster(2)) then
        write(6,*) "partg partg     ",partg     ,"monster(2)",monster(2),"This many parts",nparts,"These parts",partx
     end if
     deallocate(ux,uy,imat,partx)
     return
     end
