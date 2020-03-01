!######################################
     integer function partl(m1,n1,m2,n2,perm,parts,nparts)
!F &=\isum\jsum\ksum\lsum I_{i j} I_{k \ell} U_2\cr
implicit none
     integer m1,n1,m2,n2,perm(m1+n1+m2+n2),aa,ii,jj,kk,ufi,nparts,ll,bb
     double precision monster(2)
     integer partcs,partc,partd,parte,partf
     double precision parts(nparts)
     double precision,allocatable,dimension(:)::partx,temp
     integer,allocatable,dimension(:,:)::imat
     logical,allocatable,dimension(:):: ux,uy
     allocate(ux(m1+m2),uy(n1+n2),partx(nparts))
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
     partl=0;monster(2)=0
     do ii=1,m1+m2
        do jj=1,n1+n2
           do kk=1,m1+m2
              if(kk.ne.ii) then
                 do ll=1,n1+n2
                    if(ll.ne.jj) then
                       partl=partl+imat(ii,jj)*imat(kk,ll)
                    end if!ll
                 end do!ll
              end if
           end do!kk
        end do!jj
     end do!ii
     partl=partl*ufi(m1+m2,n1+n2,imat,ux,uy)**2
     allocate(temp(5))
     partx(1)=1.0d0*partd(m1,n1,m2,n2,perm,temp,5)
     partx(2)=2.0d0*partc(m1,n1,m2,n2,perm,temp,5)
     partx(3)=2.0d0*partcs(m1,n1,m2,n2,perm,temp,5)
     partx(4)=2.0d0*partf(m1,n1,m2,n2,perm,temp,5)
     partx(5)=2.0d0*parte(m1,n1,m2,n2,perm,temp,5)
     deallocate(temp)
     do aa=1,nparts
        parts(aa)=parts(aa)+partx(aa)
        monster(2)=monster(2)+partx(aa)
     end do
     if(partl     .ne.monster(2)) then
        write(6,*) "partl partl     ",partl     ,"monster(2)",monster(2),"This many parts",nparts,"These parts",partx
     end if
!    write(6,*) "monster",partl     ,monster(2)
     deallocate(ux,uy,imat,partx)
     return
     end
