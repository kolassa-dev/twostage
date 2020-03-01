     subroutine edswrapper(xv,kk1,kk2,kk3,kk4,nn,rho,tail,justnorm)
implicit none
     integer nn
     double precision xv(2),rho,kk3(2,2,2),kk4(2,2,2,2),alpha(2),kk1(2),kk2(2,2),sds(2),bvnd
     double precision x1,x2,tail(2),k111,k112,k122,k222,k1111,k1112,k1122,k1222,k2222
     logical justnorm
     x1=(xv(1)-kk1(1))/sqrt(kk2(1,1))
     x2=(xv(2)-kk1(2))/sqrt(kk2(2,2))
     k112=kk3(1,1,2)
     k122=kk3(1,2,2)
     k222=kk3(2,2,2)
     k1111=kk4(1,1,1,1)
     k1112=kk4(1,1,1,2)
     k1122=kk4(1,1,2,2)
     k1222=kk4(1,2,2,2)
     k2222=kk4(2,2,2,2)
     sds(1)=sqrt(kk2(1,1))
     sds(2)=sqrt(kk2(2,2))
     tail(2)=bvnd(x1,x2,rho)
!    write(6,*) "before bivtail tail(1)",tail(1)
     if(.not.justnorm) call bivtail(x1,x2,tail,rho,k111,k112,k122,k222,k1111,k1112,k1122,k1222,k2222,nn,alpha)
!    write(6,*) "Edgeworth approximaton",tail(1),tail(2)
!    write(6,*) "xv",xv,"sds",sds
     return
     end
