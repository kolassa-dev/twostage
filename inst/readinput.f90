   subroutine readinput(u1,u2,m1,n1,m2,n2,delta,alpha)
   integer u1,u2,m1,n1,m2,n2
   double precision delta,alpha(2)
   open(38,file="lastinput",position="append")
   if(u1.lt.0) then
      if(delta.gt.-5.0d-1) then
         write(6,*) "m1,n1,m2,n2,delta,alpha (2 values)"
         read(5,*) m1,n1,m2,n2,delta,alpha(1),alpha(2)
         write(38,*) m1,n1,m2,n2,delta,alpha(1),alpha(2)
         write(6,'(4(a3,i3,1x),2(a6,f6.3))') "m1=",m1,"n1=",n1,"m2=",m2,"n2=",n2,"delta=",delta,"alpha=",alpha(1),alpha(2)
      else
         write(6,*) "m1,n1,m2,n2"
         read(5,*) m1,n1,m2,n2
         write(38,*) m1,n1,m2,n2
         write(6,'(4(a3,i3,1x))') "m1=",m1,"n1=",n1,"m2=",m2,"n2=",n2
      end if
   else
      write(6,*) "u1,u2,m1,n1,m2,n2"
      read(5,*) u1,u2,m1,n1,m2,n2
      write(38,*) u1,u2,m1,n1,m2,n2
      write(6,'(6(a3,i3,1x))') "u1=",u1,"u2=",u2,"m1=",m1,"n1=",n1,"m2=",m2,"n2=",n2
   end if
   close(38)
   end
