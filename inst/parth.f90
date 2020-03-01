!######################################
     integer function parth(m1,n1,m2,n2,perm,parts,nparts)
     integer m1,n1,m2,n2,perm(m1+n1+m2+n2),aa,ii,jj,kk,ufi,bb,cc,dd,nparts
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
     parth     =0;monster(2)=0
     do ii=1,m1+m2
        do jj=1,n1+n2
           do kk=1,m1+m2
              if(kk.ne.ii) then
                 parth     =parth     +imat(ii,jj)*imat(kk,jj)
                 ux(ii)=.false.; ux(kk)=.false.; uy(jj)=.false.
                 partx(1)=partx(1)+imat(ii,jj)*imat(kk,jj)*ufi(m1+m2,n1+n2,imat,ux,uy)**2
                 partx(6)=partx(6)+4*imat(ii,jj)*imat(kk,jj)*ufi(m1+m2,n1+n2,imat,ux,uy)
                 partx(11)=partx(11)+4*imat(ii,jj)*imat(kk,jj)
                 do aa=1,m1+m2
                    if((aa.ne.ii).and.(aa.ne.kk)) then
                       ux(aa)=.false.
                       partx(2)=partx(2)+2*imat(ii,jj)*imat(kk,jj)*imat(aa,jj)*ufi(m1+m2,n1+n2,imat,ux,uy)
                       partx(7)=partx(7)+imat(ii,jj)*imat(kk,jj)*imat(aa,jj)
                       partx(12)=partx(12)+4*imat(ii,jj)*imat(kk,jj)*imat(aa,jj)!Note similarity to partx(7)
                       ux(aa)=.true.
                       do cc=1,m1+m2
                         if((cc.ne.ii).and.(cc.ne.kk).and.(cc.ne.aa)) then
                            partx(8)=partx(8)+imat(ii,jj)*imat(kk,jj)*imat(aa,jj)*imat(cc,jj)
                         end if
                      end do
                      do bb=1,n1+n2
                         if(bb.ne.jj) then
                            partx(3)=partx(3)+2*imat(ii,jj)*imat(kk,jj)*imat(aa,jj)*imat(aa,bb)
                            partx(5)=partx(5)+2*imat(ii,jj)*imat(kk,jj)*imat(aa,bb)*( imat(ii,bb)+ imat(kk,bb))
                            partx(14)=partx(14)+2*imat(ii,jj)*imat(kk,jj)*imat(aa,jj)*(imat(ii,bb)+imat(kk,bb))
                         end if
                      end do
                   end if
                end do
                do bb=1,n1+n2
                   if(bb.ne.jj) then
                      uy(bb)=.false.
                      partx(4)=partx(4)+2*imat(ii,jj)*imat(kk,jj)*(imat(ii,bb)+imat(kk,bb))*ufi(m1+m2,n1+n2,imat,ux,uy)
                      partx(13)=partx(13)+4*imat(ii,jj)*imat(kk,jj)*(imat(ii,bb)+imat(kk,bb))
                      uy(bb)=.true.
                      partx(9)=partx(9)+2*imat(ii,jj)*imat(kk,jj)*imat(ii,bb)*imat(ii,bb)*(1+imat(kk,bb))
                      do dd=1,n1+n2
                         if((dd.ne.bb).and.(dd.ne.jj)) then
                            partx(10)=partx(10)+2*imat(ii,jj)*imat(kk,jj)*imat(ii,dd)*(imat(ii,bb)+imat(kk,bb))
                         end if
                      end do
                   end if
                end do
                ux(ii)=.true.; ux(kk)=.true.; uy(jj)=.true.
             end if
          end do
       end do
     end do
!    write(6,*) "ux",ux,"uy",uy
     parth     =parth     *ufi(m1+m2,n1+n2,imat,ux,uy)**2
     do aa=1,nparts
        parts(aa)=parts(aa)+partx(aa)
        monster(2)=monster(2)+partx(aa)
     end do
     if(parth     .ne.monster(2)) write(6,*) "parth     ",parth     ,"monster(2)",monster(2)
     deallocate(ux,uy,imat,partx)
     return
     end
