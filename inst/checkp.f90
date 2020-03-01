     subroutine checkp(delta,nsamp)
     use pmod
implicit none
     real snorm
     double precision delta
     double precision xv(4),yv(4),mcp(13)
     integer(kind=8) nsamp,ii
     integer(kind=8) out(13)
     integer i,j,k,l,a,b,c,d,jj
     i=1;j=1;k=2;l=2;a=3;b=3;c=4;d=4
!    delta=0.0d0
     do ii=1,13
        out(ii)=0_8
     end do
     do ii=1,nsamp
        do jj=1,4
           xv(jj)=dble(snorm())
        end do 
        do jj=1,4
           yv(jj)=dble(snorm())-delta
        end do
        if((xv(i).lt.yv(j))) out(1)=out(1)+1_8
        if((xv(i).lt.yv(j)).and.(xv(k).lt.yv(j))) out(2)=out(2)+1_8
        if((xv(i).lt.yv(j)).and.(xv(k).lt.yv(j)).and.(xv(a).lt.yv(j))) out(3)=out(3)+1_8
        if((xv(i).lt.yv(j)).and.(xv(k).lt.yv(j)).and.(xv(a).lt.yv(j)).and.(xv(a).lt.yv(b))) out(4)=out(4)+1_8
        if((xv(i).lt.yv(j)).and.(xv(k).lt.yv(j)).and.(xv(i).lt.yv(b))) out(5)=out(5)+1_8
        if((xv(i).lt.yv(j)).and.(xv(k).lt.yv(j)).and.(xv(i).lt.yv(b)).and.(xv(a).lt.yv(b))) out(6)=out(6)+1_8
        if((xv(i).lt.yv(j)).and.(xv(k).lt.yv(j)).and.(xv(a).lt.yv(j)).and.(xv(c).lt.yv(j))) out(7)=out(7)+1_8
        if((xv(i).lt.yv(j)).and.(xv(k).lt.yv(j)).and.(xv(i).lt.yv(b)).and.(xv(i).lt.yv(d))) out(8)=out(8)+1_8
        if((xv(i).lt.yv(j)).and.(xv(k).lt.yv(j)).and.(xv(i).lt.yv(b)).and.(xv(k).lt.yv(b))) out(9)=out(9)+1_8
        if((xv(i).lt.yv(j)).and.(xv(i).lt.yv(l))) out(10)=out(10)+1_8
        if((xv(i).lt.yv(j)).and.(xv(k).lt.yv(j)).and.(xv(i).lt.yv(d)).and.(xv(k).lt.yv(b))) out(11)=out(11)+1_8
        if((xv(i).lt.yv(j)).and.(xv(i).lt.yv(l)).and.(xv(i).lt.yv(b)).and.(xv(i).lt.yv(d))) out(12)=out(12)+1_8
        if((xv(i).lt.yv(j)).and.(xv(i).lt.yv(l)).and.(xv(i).lt.yv(b))) out(13)=out(13)+1_8
     end do
     do ii=1,13
        mcp(ii)=dble(out(ii))/dble(nsamp)
        write(6,'(i2,x,3(f8.4,x))') ii-1,mcp(ii),probs(ii),abs(probs(ii)-mcp(ii))/mcp(ii)
     end do
     end
