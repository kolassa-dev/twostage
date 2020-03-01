!######################################
     integer function parths(m1,n1,m2,n2,perm,parts,nparts)
implicit none
     integer m1,n1,m2,n2,perm(m1+n1+m2+n2),aa,ii,jj,ufi,bb,cc,dd,nparts,ll
     double precision parts(nparts)
     double precision monster(2)
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
     parths    =0; monster(2)=0
     do ii=1,m1+m2
        do jj=1,n1+n2
           do ll=1,n1+n2
              if(ll.ne.jj) then
                 parths    =parths    +imat(ii,jj)*imat(ii,ll)
                 ux(ii)=.false.; uy(ll)=.false.; uy(jj)=.false.
                 partx(1)=partx(1)+imat(ii,jj)*imat(ii,ll)*ufi(m1+m2,n1+n2,imat,ux,uy)**2
                 partx(6)=partx(6)+4*imat(ii,jj)*imat(ii,ll)*ufi(m1+m2,n1+n2,imat,ux,uy)
                 partx(11)=partx(11)+4*imat(ii,jj)*imat(ii,ll)
                 do bb=1,n1+n2
                    if((bb.ne.jj).and.(bb.ne.ll)) then
                       uy(bb)=.false.
!                      write(6,*) "ii=",ii,"jj,ll,bb=",jj,ll,bb,"ux",ux,"uy",uy
                       partx(2)=partx(2)+2*imat(ii,jj)*imat(ii,ll)*imat(ii,bb)*ufi(m1+m2,n1+n2,imat,ux,uy)
                       partx(7)=partx(7)+imat(ii,jj)*imat(ii,ll)*imat(ii,bb)
                       partx(12)=partx(12)+4*imat(ii,jj)*imat(ii,ll)*imat(ii,bb)!Note similarity to partx(7)
                       uy(bb)=.true.
                       do dd=1,n1+n2
                          if((dd.ne.jj).and.(dd.ne.ll).and.(dd.ne.bb)) then
                             partx(8)=partx(8)+imat(ii,jj)*imat(ii,ll)*imat(ii,bb)*imat(ii,dd)
                          end if
                       end do
                       do aa=1,m1+m2
                          if(aa.ne.ii) then
                             partx(3)=partx(3)+2*imat(ii,jj)*imat(ii,ll)*imat(ii,bb)*imat(aa,bb)
                             partx(5)=partx(5)+2*imat(ii,jj)*imat(ii,ll)*imat(aa,bb)*( imat(aa,jj)+ imat(aa,ll))
                             partx(14)=partx(14)+2*imat(ii,jj)*imat(ii,ll)*imat(ii,bb)*(imat(aa,jj)+imat(aa,ll))
                          end if
                      end do
                   end if
                 end do!bb
                 do aa=1,m1+m2
                    if(aa.ne.ii) then
                       ux(aa)=.false.
                       partx(4)=partx(4)+2*imat(ii,jj)*imat(ii,ll)*(imat(aa,jj)+imat(aa,ll))*ufi(m1+m2,n1+n2,imat,ux,uy)
                       partx(13)=partx(13)+4*imat(ii,jj)*imat(ii,ll)*(imat(aa,jj)+imat(aa,ll))
                       ux(aa)=.true.
                       partx(9)=partx(9)+2*imat(ii,jj)*imat(ii,ll)*imat(aa,jj)*(1+imat(aa,ll))
                       do cc=1,m1+m2
                          if((cc.ne.aa).and.(cc.ne.ii)) then
                             partx(10)=partx(10)+2*imat(ii,jj)*imat(ii,ll)*imat(cc,jj)*(imat(aa,jj)+imat(aa,ll))
                          end if
                       end do
                    end if
                 end do
                 ux(ii)=.true.; uy(ll)=.true.; uy(jj)=.true.
              end if
           end do
        end do
     end do
!    write(6,*) "ux",ux,"uy",uy
     parths    =parths    *ufi(m1+m2,n1+n2,imat,ux,uy)**2
     do aa=1,nparts
        parts(aa)=parts(aa)+partx(aa)
        monster(2)=monster(2)+partx(aa)
     end do
     if(parths    .ne.monster(2)) then
        write(6,*) "parths    ",parths    ,"monster(2)",monster(2),"perm",perm,"partx"
        do aa=1,nparts
           write(6,*) aa,partx(aa)
        end do
     end if
     deallocate(ux,uy,imat,partx)
     return
     end
