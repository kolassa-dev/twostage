      subroutine stdcum(kk1,kk2,kk3,kk4,sds,rho)
implicit none
      double precision kk2(2,2),kk3(2,2,2),kk4(2,2,2,2),sds(2),rho,kk1(2)
      integer ii,jj,kk,ll
      sds(1)=sqrt(kk2(1,1))
      sds(2)=sqrt(kk2(2,2))
      rho=kk2(1,2)/(sds(1)*sds(2))
!     write(6,*) "In stdcum rho",rho,"kk2(1,2)",kk2(1,2),"kk2(1,1)",kk2(1,1),"kk2(2,2)",kk2(2,2)
      do ii=1,2
         do jj=1,2
            do kk=1,2
               kk3(ii,jj,kk)=kk3(ii,jj,kk)/( sds(ii)* sds(jj)* sds(kk))
               do ll=1,2
                  kk4(ii,jj,kk,ll)=kk4(ii,jj,kk,ll)/(sds(ii)*sds(jj)*sds(kk)*sds(ll))
               end do
            end do
         end do
      end do
      return
      end
