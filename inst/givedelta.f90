     subroutine givedelta(m1,n1,m2,n2,confi,beta,delta)
implicit none
     double precision confi, beta,zbeta,zalpha,altmean,scrap,delta,se
     integer m1,n1,m2,n2
     integer status
     call cdfnor(2,confi,1.0d0-confi,zalpha,0.0d0,1.0d0,status,scrap)
     write(6,*) "zalpha",zalpha,"Error flag",status
     call cdfnor(2,1.0d0-beta,beta,zbeta,0.0d0,1.0d0,status,scrap)
     write(6,*) "zbeta",zbeta,"Error flag",status
     se=sqrt((m1+m2+n1+n2+1)/dble(12.0*(m1+m2)*(n1+n2)))
     altmean=0.5d0+(zalpha+zbeta)*se
     call cdfnor(2,altmean,1.0d0-altmean,delta,0.0d0,1.0d0,status,scrap)
     write(6,*) "altmean",altmean,"se",se,"status",status,"delta",delta,"confi",confi
     return
     end
