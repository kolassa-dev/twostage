      subroutine makexvn(xvn,rho,alpha,ii)
implicit none
      double precision rho, alpha(2),xvn(2),dinvnr
      integer ii
!     write(6,*) "Entering makexvn rho",rho,"alpha",alpha
      xvn(1)=dinvnr(1.0d0-alpha(1),alpha(1))
      call isecnorm(xvn(1),xvn(2),alpha(2),rho,ii)
!     if(ii.ne.0) write(6,*) "Error from isecnorm"
      return
      end
