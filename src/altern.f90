   subroutine altern(cc,u1,u2,m1,n1,m2,n2)
   logical cc(2)
   logical dd(5)
   integer u1,u2,m1,n1,m2,n2
   dd(1)=(m1.lt.0).or.(m2.lt.0).or.(n1.lt.0).or.(n2.lt.0)
   dd(2)=(u1.lt.0).or.(u1.gt.(m1*n1)).or.(u2.lt.0).or.(u2.gt.((m1+m2)*(n1+n2))).or.(u2.lt.u1)
   dd(3)=((n1.eq.0).or.(m1.eq.0)).and.(u1.ne.0)
   dd(4)=((n2.eq.0).and.(m2.eq.0)).and.(u2.ne.u1)
   dd(5)=(u2-u1).gt.(n2*(m1+m2))
   cc(1)=(m1.eq.0).and.(m2.eq.0).and.(n1.eq.0).and.(n2.eq.0)
   cc(2)=dd(1).or.dd(2).or.dd(3).or.dd(4)!.or.dd(5)
   return
   end
