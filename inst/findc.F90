   subroutine findc(outv,u1v,u2v,u1,u2,npts,out,mult,m1,m2,n1,n2)
!outv: counts from comparison file
!u1v: values of first MW stat correspoding to entries in outv
!u2v: values of second MW stat correspoding to entries in outv
!u1: Value of first MW stat to look up in the table.
!u2: Value of second MW stat to look up in the table.
!npts: Number of lines in the comparison file
!out: Returned number looked up from table, representing the number of times
!    this pair of statistic values is observed among all (m1+m2+n1+n2)!/
!    (m1!m2!n1!n2!) permutation of group lables.
!mult: m1!m2!n1!n2!, representing the factor by which out must be multiplied
!     to give the count of times the pair of values is in the table among all
!     (m1+m2+n1+n2)! permuations of item labels.
   integer m1,m2,n1,n2
   integer u1,u2,npts
   integer u1v(npts),u2v(npts)
#ifdef BIGINT
   integer(kind=8) outv(npts),out,mult
   integer(kind=8),parameter::one=1_8,zero=0_8
#else
   integer outv(npts),out,mult
   integer,parameter::one=1,zero=0
#endif
   integer(kind=8),dimension(:),allocatable,save:: factv
   integer ii
   logical cc(2)
   if(.not.allocated(factv)) then
      allocate(factv(15))
      factv(1)=1
      do ii=2,15
         factv(ii)=factv(ii-1)*(ii-1)
      end do
   end if
#ifdef DEBUGME
   write(6,*) "In findc m1,m2,n1,n2",m1,m2,n1,n2,"u1,u2",u1,u2
   write(6,*) "u1v",(u1v(ii),ii=1,npts)
   write(6,*) "u2v",(u2v(ii),ii=1,npts)
   write(6,*) "outv",(outv(ii),ii=1,npts)
#endif
   out=-one
   call altern(cc,u1,u2,m1,n1,m2,n2)
   if(cc(2)) then
#ifdef DEBUGME
      write(6,*) "Condition 2 says output 0"
#endif
      out=zero
!  else
!     if(.not.(npts.eq.((m1*n1+1)*((m1+m2)*(n1+n2)+1)))) then
!        write(6,*) "Found unexpected number of points",npts,"u1,u2",u1,u2
!        stop
!     end if
   end if
   ii=0
   do while((out.lt.zero).and.(ii.lt.npts)) 
      ii=ii+1
#ifdef DEBUGME
      write(6,'(a3,4(1x,i4),2(l1,1x),2(i3,1x))') "xxx",u1,u1v(ii),u2,u2v(ii),cc(1),cc(2),out,outv(ii)
#endif
      if((u1.eq.u1v(ii)).and.(u2.eq.u2v(ii))) then
#ifdef DEBUGME
         write(6,*) "Match"
#endif
         out=outv(ii)
      end if
   end do
   if(out.lt.0) then
      write(6,*) "Warning: Did not find match, u1",u1,"u2",u2
      write(6,*) "u1v",(u1v(ii),ii=1,npts)
      write(6,*) "u2v",(u2v(ii),ii=1,npts)
      write(6,*) "outv",(outv(ii),ii=1,npts)
      write(6,*) "m1,m2,n1,n2",m1,m2,n1,n2
      stop
   else
      out=out
!     mult=factv(m1+1)*factv(m2+1)*factv(n1+1)*factv(n2+1)
#ifdef DEBUGME
      write(6,'(6(a3,i3,1x),2(l1,1x),a4,i3)') "m1=",m1, "n1=",n1, "m2=",m2, "n2=",n2,"u1=",u1,"u2=",u2,cc(1),cc(2),"out=",out
#endif
   end if
   return
   end
