!######################################
     integer function ufi(mm,nn,imat,ux,uy)
     integer mm,nn,imat(mm,nn)
     logical ux(mm),uy(nn)
     integer ii,jj
     ufi=0
     do ii=1,mm
        do jj=1,nn
           if(ux(ii).and.uy(jj)) ufi=ufi+imat(ii,jj)
        end do
     end do
!    write(6,*) "ufi",ufi
     return
     end
