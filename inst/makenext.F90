   program makenext
use fmtout
implicit none
   integer m1,n1,m2,n2,u1,u2,efg,ii,jj
   logical allok,first
!  integer(kind=8),allocatable,dimension(:),save::factv
   character*20 filen,fileo
#ifdef BIGINT
   integer(kind=8),allocatable,dimension(:)::outv1,outv2,outv3,outv4
   integer(kind=8)::out,result
   integer(kind=8),parameter::zero=0_8
!  integer(kind=8):: bot
   integer(kind=8):: mult
#else
   integer,allocatable,dimension(:)::outv1,outv2,outv3,outv4
   integer::out,result
   integer,parameter::zero=0
!  integer:: bot
   integer:: mult
#endif
   double precision cnt
   integer,allocatable,dimension(:)::u11,u21,u12,u22,u13,u23,u14,u24
   integer npts1,npts2,npts3,npts4
   double precision scrap
!  if(.not.allocated(factv)) then
!     allocate(factv(15))
!     factv(1)=1
!     do ii=2,15
!        factv(ii)=factv(ii-1)*(ii-1)
!     end do
!  end if
   open(38,file="lastinput");close(38)!Start with empty file lastinput
   ii=1
   first=.true.
   do while(ii.gt.0) 
      scrap=-1.0d0
      u1=-1
      if(first) then
         m1=0; n1=0; m2=0; n2=0
      else
         call readinput(u1,u2,m1,n1,m2,n2,scrap)
         ii=m1+n1+m2+n2
      end if
      if((ii.gt.0).or.first) then
         call mkfn(m1,n1,m2,n2,fileo,"u",0.0d0)
         open(32,file=fileo,status="old",iostat=jj)!Check to see if the file already exists.  If so, quit.
!        write(6,*) "iostat=",jj
         if(first) then
            first=.false.
            jj=0
            open(32,file=fileo)
            write(32,outfmt) 0,0,1
            close(32)
         end if
         if(jj.ne.0) then
            allok=.true.
            allocate(outv1(1),u11(1),u21(1))
            npts1=0
            call readout(outv1,u11,u21,npts1,m1-1,m2,n1,n2,cnt,0.0d0,efg,filen)
            deallocate(outv1,u11,u21)
            if(efg.eq.0) then
               allocate(outv1(npts1),u11(npts1),u21(npts1))
               call readout(outv1,u11,u21,npts1,m1-1,m2,n1,n2,cnt,0.0d0,efg,filen)
!              write(6,*) "npts1",npts1
!              write(6,*) "npts1",npts1,"u11",(u11(jj),jj=1,npts1)
            else
               write(6,*) "Failed for file",filen
               allok=.false.
            end if
            allocate(outv2(1),u12(1),u22(1))
            npts2=0
            call readout(outv2,u12,u22,npts2,m1,m2-1,n1,n2,cnt,0.0d0,efg,filen)
            deallocate(outv2,u12,u22)
            if(efg.eq.0) then
               allocate(outv2(npts2),u12(npts2),u22(npts2))
               call readout(outv2,u12,u22,npts2,m1,m2-1,n1,n2,cnt,0.0d0,efg,filen)
!              write(6,*) "npts2",npts2
!              write(6,*) "npts2",npts2,"u12",(u12(jj),jj=1,npts2)
            else
               write(6,*) "Failed for file",filen
               allok=.false.
            end if
            allocate(outv3(1),u13(1),u23(1))
            npts3=0
            call readout(outv3,u13,u23,npts3,m1,m2,n1-1,n2,cnt,0.0d0,efg,filen)
            deallocate(outv3,u13,u23)
            if(efg.eq.0) then
               allocate(outv3(npts3),u13(npts3),u23(npts3))
               call readout(outv3,u13,u23,npts3,m1,m2,n1-1,n2,cnt,0.0d0,efg,filen)
!              write(6,*) "npts3",npts3
!              write(6,*) "npts3",npts3,"u13",(u13(jj),jj=1,npts3)
            else
               write(6,*) "Failed for file",filen
               allok=.false.
            end if
            allocate(outv4(1),u14(1),u24(1))
            npts4=0
            call readout(outv4,u14,u24,npts4,m1,m2,n1,n2-1,cnt,0.0d0,efg,filen)
            deallocate(outv4,u14,u24)
            if(efg.eq.0) then
               allocate(outv4(npts4),u14(npts4),u24(npts4))
               call readout(outv4,u14,u24,npts4,m1,m2,n1,n2-1,cnt,0.0d0,efg,filen)
!              write(6,*) "npts4",npts4
!              write(6,*) "npts4",npts4,"u14",(u14(jj),jj=1,npts4)
            else
               write(6,*) "Failed for file",filen
               allok=.false.
            end if
!           write(6,*) "Done checking input files, allok",allok
            if(allok) then
               open(32,file=fileo,status="new",iostat=jj)
               if(jj.ne.0) then
                  write(6,*) "Trouble opeining file ",filen
                  stop
               end if
#ifdef DEBUGME
               write(6,*) "At top of loop"
#endif
               do u1=0,m1*n1
                  do u2=0,(m1+m2)*(n1+n2)
#ifdef DEBUGME
                     write(6,*) "u1,u2",u1,u2
#endif
                     result=zero
#ifdef DEBUGME
                     write(6,*) "Comparison 1"
#endif
                     call findc(outv1,u11,u21,u1,u2,npts1,out,mult,m1-1,m2,n1,n2)
                     call updater(result,out,m1,mult)
#ifdef DEBUGME
                     write(6,*) "Comparison 2"
#endif
                     call findc(outv2,u12,u22,u1,u2,npts2,out,mult,m1,m2-1,n1,n2)
                     call updater(result,out,m2,mult)
#ifdef DEBUGME
                     write(6,*) "Comparison 3"
#endif
                     call findc(outv3,u13,u23,u1-m1,u2-m1-m2,npts3,out,mult,m1,m2,n1-1,n2)
                     call updater(result,out,n1,mult)
#ifdef DEBUGME
                     write(6,*) "Comparison 4"
#endif
                     call findc(outv4,u14,u24,u1,u2-m1-m2,npts4,out,mult,m1,m2,n1,n2-1)
                     call updater(result,out,n2,mult)
#ifdef DEBUGME
                     write(6,*) "result before rescaling",u1, u2, result
#endif
!                    bot=factv(m1+1)*factv(m2+1)*factv(n1+1)*factv(n2+1)
!                    write(6,*) "bot",bot
!                    if(mod(result,bot).ne.zero) then 
!                       write(6,*) "Unanticipated remainder"
!                       stop
!                    end if
!                    result=result/bot
!                    write(6,*) "result after  rescaling",u1, u2, result
!                    write(6,'(i3,1x,i3,1x,i20   )') u1,u2,result
!                    if(result.gt.zero) write(32,outfmt) u1,u2,result
                     write(32,outfmt) u1,u2,result
                  end do
               end do
               close(32)
            else
!              write(6,*) "Adding to the to do list"
               open(39,file="todolist",position="append")
               write(39,*) m1,n1,m2,n2
               close(39)
            end if
            if(allocated(outv1)) deallocate(outv1,u11,u21)
            if(allocated(outv2)) deallocate(outv2,u12,u22)
            if(allocated(outv3)) deallocate(outv3,u13,u23)
            if(allocated(outv4)) deallocate(outv4,u14,u24)
!           write(6,*) "End of deallocation"
         end if
      end if
   end do
   end
!#####################################################
