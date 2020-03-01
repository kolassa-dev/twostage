     subroutine justrho(m1,m2,n1,n2,rho,delta)
implicit none
     double precision delta
     integer m1,n1,m2,n2
     double precision mom1(2),mom2(2,2),mom3(2,2,2),mom4(2,2,2,2)
     double precision rho,sds(2)
     call fillps(delta)
     call tmom(m1,n1,m2,n2,mom1,mom2,mom3,mom4,.true.)
     sds(1)=sqrt(mom2(1,1)-mom1(1)**2)
     sds(2)=sqrt(mom2(2,2)-mom1(2)**2)
     rho=(mom2(1,2)-mom1(1)*mom1(2))/(sds(1)*sds(2))
     return
     end
