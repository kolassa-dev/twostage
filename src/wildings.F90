     subroutine wildings(u1,u2,m1,n1,m2,n2,out,debug)
implicit none
     integer u1,u2,m1,n1,m2,n2,dd
#ifdef DOINT
     integer(kind=8) out,wilding
#else
     double precision out,wilding
#endif
     logical debug
     dd=0
     out=wilding(u1,u2,m1,n1,m2,n2,dd,debug)
     return
     end
