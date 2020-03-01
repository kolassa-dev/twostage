module pmod
     double precision probs(13)
end module pmod
subroutine fillps(delta)
     use pmod
     double  precision delta
     probs(1+0)=1.0d0/2.0d0
     probs(1+1)=1.0d0/3.0d0
     probs(2+1)=1.0d0/4.0d0
     probs(3+1)=3.0d0/20.0d0
     probs(4+1)=5.0d0/24.0d0
     probs(5+1)=2.0d0/15.0d0
     probs(6+1)=1.0d0/5.0d0
     probs(7+1)=3.0d0/20.0d0
     probs(8+1)=1.0d0/6.0d0
     probs(9+1)=1.0d0/3.0d0
     probs(10+1)=2.0d0/15.0d0
     probs(11+1)=1.0d0/5.0d0
     probs(12+1)=1.0d0/4.0d0
     return
     end
