!###############################################################################
     subroutine checkone(tpo,obsd,np,cnt,slab)
     integer np,ii
     double precision tpo(np),obsd(np),pa
     logical mistake
     integer(kind=8) cnt
     character(LEN=1) slab
     mistake=.false.
     do ii=1,np
         pa=obsd(ii)/dble(cnt)
         if(abs(pa-tpo(ii)).gt.0.0001) then
            write(6,*) "parts(",ii,") ave",pa,"calc",tpo(ii)
            mistake=.true.
         end if
      end do 
      if(mistake) write(6,*) "All ",slab," moments not accurate."
      if(.not.mistake) write(6,*) "All ",slab," moments accurate."
      return
      end
