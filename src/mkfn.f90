!###############################################################
     subroutine mkfn(m1,n1,m2,n2,filen,pref,delta)
     integer m1,n1,m2,n2,ii
     character(len=20) filen
     character(len=1) pref
     double precision delta
     filen=" out            .csv"
     write(filen( 5: 6),"(i2)") m1
     write(filen(1:1),"(a1)") pref
     if(m1<10) filen(5:5)="x"
     write(filen( 7: 8),"(i2)") n1
     if(n1<10) filen(7:7)="x"
     write(filen( 9:10),"(i2)") m2
     if(m2<10) filen(9:9)="x"
     write(filen(11:12),"(i2)") n2
     if(n2<10) filen(11:11)="x"
     ii=int(delta*100)
     write(filen(13:16),"(i4)") ii
     if(ii<1000) write(filen(13:13),"(i1)") 0
     if(ii<100) write(filen(14:14),"(i1)") 0
     if(ii<10) write(filen(15:15),"(i1)") 0
     return
     end
