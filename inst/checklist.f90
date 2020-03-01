   program checklist
   logical lmatch,templ(2)
   character*20 filen
   integer ii,jj,kk,ll,nc
   integer,dimension(:,:),allocatable::list
   write(6,*) "file name?"
   read(5,*) filen
   nlines=0
   allocate(list(1,4))
   call readlist(nlines,list,filen,nc)
   deallocate(list)
   write(6,*) "Found ",nlines," lines"
   allocate(list(nlines+1,4))
   call readlist(nlines,list,filen,nc)
   ii=1
   lmatch=.true.
   do while((ii.le.nlines).and.lmatch)
      ii=ii+1
      write(6,'(a14,i5,a10,4(i2,1x))') "Checking line ",ii," which is ",(list(ii,jj),jj=1,4)
      jj=0
      do while((jj.lt.4).and.lmatch)
         jj=jj+1
!        write(6,*) "Checking component ",jj
         lmatch=list(ii,jj).le.0!Component jj of line ii doesn't need a match if it is negative.
         kk=0
         do while((kk.lt.(ii-1)).and.(.not.lmatch))
            kk=kk+1
!           write(6,'(a22,i5,a10,4(i2,1x))') "Checking against line ",kk," which is ",(list(kk,ll),ll=1,4)
            lmatch=.true.
            ll=0
            do while(ll.lt.4)
               ll=ll+1
               templ(1)=(ll.eq.jj).and.(list(kk,ll).eq.(list(ii,ll)-1))
               templ(2)=(ll.ne.jj).and.(list(kk,ll).eq.list(ii,ll))
!              write(6,*) "templ",templ,list(kk,ll),(list(ii,ll)-1),"ll,jj",ll,jj
               lmatch=lmatch.and.(templ(1).or.templ(2))
!              write(6,*) "Checking component ",ll," for line ",kk,"lmatch",lmatch
            end do
         end do
      end do
   end do
   write(6,*) "lmatch",lmatch
   end
   subroutine readlist(nlines,list,filen,nc)
   integer nlines,list(nlines+1,4),ii,nc,jj
   character*20 filen
   if(nlines.eq.0) then
      nc=0
      do while(filen((nc+1):(nc+1)).ne." ")
         nc=nc+1
      end do
      open(41,file=filen(1:nc),status='old')
      ii=0
      nlines=0
      do while(ii.eq.0)
         read(41,*,iostat=ii)
         if(ii.eq.0) nlines=nlines+1
      end do
      close(41)
   else
      list(1,1)=0; list(1,2)=0; list(1,3)=0; list(1,4)=0
      open(41,file=filen(1:nc),status='old')
      do ii=1,nlines
         read(41,*) (list(ii+1,jj),jj=1,4)
      end do
      close(41)
   end if
   return
   end
