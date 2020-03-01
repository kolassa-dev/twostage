     program checktest
implicit none
     integer m1,n1,m2,n2,nn,u1,u2
     double precision confi(2),rho,&
         power,powerc,tail(2),xvraw(2),xvn(2),xvcn(2),utail(2),size,sizec,xvrawc(2),ana(2),anb(2)
     logical done,flip
     integer efg,ii,docheck,ctli
!    real ta,et,sd,k3,k4
     double precision delta,zero,one
     rho=0.0!Just to remove unused variable warning.
     zero=0.0d0
     one=1.0d0
     done=.false.
     nn=1
     flip=.true.
     open(38,file="lastinput")
     open(37,file="output")
     open(36,file="work")
     write(36,*) "Normal-Based            ","Cornish-Fisher Based"
     write(36,*) "Stage 1--------- Stage 2","Stage 1--------- Stage 2"
     write(36,*) "Raw CV New Level Raw CV ","Raw CV New Level Raw CV "
     close(36)
     write(37,*) "                                  ","----  Critical Values ------", &
         " ----- Size --- ", "  ---- Power ----------"
     write(37,*) "delta alpha1  alpha2 m1 n1 m2 n2  ","Uncorrected      Corrected   "," Unc.     Corr. ", &
         "  Empir.  Normal  Edge-"
     write(37,*) "                                  ","c1        c2     c1      c2","                ", &
         "                  worth"
     close(37)
     ii=0
     ctli=1
     do while(.not.done)
        ii=ii+1
        u1=-1
        delta=0.0d0
        call readinput(u1,u2,m1,n1,m2,n2,delta,confi)
        if((m1+m2+n1+n2).gt.0) then
!          confi(1)=0.9000; confi(2)=0.8000
!          confi(1)=0.9800; confi(2)=0.9500
!          confi(1)=0.9900; confi(2)=0.9750
           if(delta.lt.0.0d0) then
              call givedelta(m1,n1,m2,n2,confi(2),0.2d0,delta)
           end if
           call fillps(zero)
!          call justrho(m1,m2,n1,n2,rho,zero)
!          call makexvn(xvn,rho,confi)
!          write(6,*) "xvn",xvn,"rho",rho
           xvcn(1)=0.0; xvcn(2)=0.0; size=0.0; sizec=0.0; docheck=1
           xvraw(1)=xvn(1); xvraw(2)=xvn(2);
           call inside(m1,m2,n1,n2,delta,ana,anb,xvraw,xvrawc,xvn,xvcn,size,sizec,tail,utail,power,powerc,confi,docheck,ctli,efg)
!          write(6,*) "Just exited inside"
!          write(6,*) delta,1.0d0-confi(1),1.0d0-confi(2),m1,n1,m2,n2,&
!             xvraw(1),xvraw(2),xvn(1),xvn(2),xvcn(1),xvcn(2),1.0d0-size,1.0d0-sizec,&
!             1.0d0-powerc,1.0d0-utail(2),1.0d0-tail(2)
           if(efg.ne.0) write(6,*) "Error flag from read of sample file",efg
           open(unit=37,file="output",status="old",position="append",iostat=u2)
           if(u2.ne.0) write(6,*) "Big trouble"
           if(flip) then
              xvraw(1)=m1*n1-xvraw(1)
              xvn(1)=m1*n1-xvn(1)
              xvcn(1)=m1*n1-xvcn(1)
              xvrawc(1)=m1*n1-xvrawc(1)
              xvraw(2)=(m1+m2)*(n1+n2)-xvraw(2)
              xvn(2)=(m1+m2)*(n1+n2)-xvn(2)
              xvcn(2)=(m1+m2)*(n1+n2)-xvcn(2)
              xvrawc(2)=(m1+m2)*(n1+n2)-xvrawc(2)
           end if
           write(37,'(3(f6.3,1x),4(i2,1x),4(f7.3,1x),5(f7.4,1x))') delta,1.0d0-confi(1),1.0d0-confi(2),m1,n1,m2,n2,&
!             xvraw(1),xvraw(2),&
              xvn(1),xvn(2),xvcn(1),xvcn(2),1.0d0-size,1.0d0-sizec,&
              1.0d0-powerc,1.0d0-utail(2),1.0d0-tail(2)
           open(unit=36,file="work",status="old",position="append",iostat=u2)
           write(36,'(2(f7.3,1x,f7.4,1x,f7.3,1x))') xvraw(1),ana(1),xvraw(2),xvrawc(1),anb(1),xvrawc(2)
           close(unit=36)
!          write(6,*) "Just finished writing an output line"
           close(unit=37,iostat=u1)
!          write(6,*) "Just finished closing a file"
           call texout(ii,m1,n1,m2,n2,xvn(1),xvn(1),xvcn(2),xvcn(2),1.0d0-size,1.0d0-sizec)
        else
           done=.true.
        end if
     end do
     call texout(-2,m1,n1,m2,n2,xvn(1),xvn(1),xvcn(2),xvcn(2),1.0d0-size,1.0d0-sizec)
     close(38)
     end
     subroutine givedelta(m1,n1,m2,n2,confi,beta,delta)
implicit none
     double precision confi, beta,zbeta,zalpha,altmean,scrap,delta,se
     integer m1,n1,m2,n2
     integer u2
     call cdfnor(2,confi,1.0d0-confi,zalpha,0.0d0,1.0d0,u2,scrap)
!    write(6,*) "zalpha",zalpha,"Error flag",u2
     call cdfnor(2,1.0d0-beta,beta,zbeta,0.0d0,1.0d0,u2,scrap)
!    write(6,*) "zbeta",zbeta,"Error flag",u2
     se=sqrt((m1+m2+n1+n2+1)/dble(12.0*(m1+m2)*(n1+n2)))
     altmean=0.5d0+(zalpha+zbeta)*se
!    write(6,*) "altmean",altmean,"se",se
     call cdfnor(2,altmean,1.0d0-altmean,delta,0.0d0,1.0d0,u2,scrap)
!    write(6,*) "u2",u2,"delta",delta
     return
     end
