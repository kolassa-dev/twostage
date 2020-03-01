module pmod
     double precision probs(13)
end module pmod
subroutine fillps(delta)
     use pmod
     implicit none
     double precision delta
     double precision lower(4),upper(4),correl(6),error,v,abseps,releps,vb
     integer ii,infin(4),maxpts
!    write(6,*) "On input to fillps delta",delta
     abseps=1.0d-10
     releps=1.0d-10
     maxpts=4000
     do ii=1,4
        lower(ii)=delta/sqrt(2.0d0)
        upper(ii)=10000d0
        infin(ii)=1
     end do
     call sadmvn(1,lower,upper,infin,correl,maxpts,abseps,releps,error,v,ii)
!  1 format('Integrating ',i2,' value ',f8.4,' status ',i1,' should be ',f8.4)
!    write(6,1) 0,v,ii,0.5d0
     probs(1+0)=v
     correl(1)=.5
     call sadmvn(2,lower,upper,infin,correl,maxpts,abseps,releps,error,v,ii)
!    write(6,1) 1,v,ii,0.3333333333333333d0
     probs(1+1)=v
     correl(1)=.5; correl(2)=.5 ; correl(3)=.5
     call sadmvn(3,lower,upper,infin,correl,maxpts,abseps,releps,error,v,ii)
!    write(6,1) 2,v,ii,0.25d0
     probs(2+1)=v
     correl(1)=0.5d0; correl(2)=0.5d0 ; correl(3)=0.5d0;correl(4)=0.0d0;correl(5)=0.0d0;correl(6)=0.5d0
     call sadmvn(4,lower,upper,infin,correl,maxpts,abseps,releps,error,v,ii)
!    write(6,1) 3,v,ii,0.15d0
     probs(3+1)=v
     correl(1)=0.5d0; correl(2)=0.5d0 ; correl(3)=0.0d0
     call sadmvn(3,lower,upper,infin,correl,maxpts,abseps,releps,error,v,ii)
!    write(6,1) 4,v,ii,5.0d0/24.0d0
     probs(4+1)=v
     correl(1)=0.5d0; correl(2)=0.5d0 ; correl(3)=0.0d0;correl(4)=0.0d0;correl(5)=0.0d0;correl(6)=0.5d0
     call sadmvn(4,lower,upper,infin,correl,maxpts,abseps,releps,error,v,ii)
!    write(6,1) 5,v,ii,2.0d0/15.0d0
     probs(5+1)=v
     correl(1)=0.5d0; correl(2)=0.5d0 ; correl(3)=0.5d0; correl(4)=0.5d0 ; correl(5)=0.5d0; correl(6)=0.5d0
     call sadmvn(4,lower,upper,infin,correl,maxpts,abseps,releps,error,v,ii)
!    write(6,1) 6,v,ii,0.2d0
     probs(6+1)=v
     correl(1)=0.5d0; correl(2)=0.5d0 ; correl(3)=0.0d0; correl(4)=0.5d0 ; correl(5)=0.0d0; correl(6)=0.5d0
     call sadmvn(4,lower,upper,infin,correl,maxpts,abseps,releps,error,v,ii)
!    write(6,1) 7,v,ii,0.15d0
     probs(7+1)=v
     correl(1)=0.0d0;correl(2)=0.5d0;correl(3)=0.5d0
     call sadmvn(3,lower,upper,infin,correl,maxpts,abseps,releps,error,v,ii)
     correl(1)=0.5d0;correl(2)=0.5d0;correl(3)=0.0d0;infin(1)=0;upper(1)=delta/sqrt(2.0d0); lower(1)=-1000d0
     call sadmvn(3,lower,upper,infin,correl,maxpts,abseps,releps,error,vb,ii)
     infin(1)=1;lower(1)=delta/sqrt(2.0d0); upper(1)=1000d0
!    write(6,1) 8,v-vb,ii,1.0d0/6.0d0
     probs(8+1)=v-vb
     correl(1)=.5
     call sadmvn(2,lower,upper,infin,correl,maxpts,abseps,releps,error,v,ii)
!    write(6,1) 9,v,ii,1.0d0/3.0d0
     probs(9+1)=v
     correl(1)=0.5d0; correl(2)=0.5d0 ; correl(3)=0.0d0;correl(4)=0.0d0;correl(5)=0.5d0;correl(6)=0.0d0
     call sadmvn(4,lower,upper,infin,correl,maxpts,abseps,releps,error,v,ii)
!    write(6,1) 10,v,ii,2.0d0/15.0
     probs(10+1)=v
     correl(1)=0.5d0; correl(2)=0.5d0 ; correl(3)=0.5d0; correl(4)=0.5d0 ; correl(5)=0.5d0; correl(6)=0.5d0
     call sadmvn(4,lower,upper,infin,correl,maxpts,abseps,releps,error,v,ii)
!    write(6,1) 11,v,ii,0.2d0
     probs(11+1)=v
     correl(1)=.5; correl(2)=.5 ; correl(3)=.5
     call sadmvn(3,lower,upper,infin,correl,maxpts,abseps,releps,error,v,ii)
!    write(6,1) 12,v,ii,2.5d-1
     probs(12+1)=v
     return
     end
