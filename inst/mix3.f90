       subroutine mix3(m1,n1,m2,n2,umoms    )
       integer n1,m1,n2,m2,NN,MM
       double precision upower3,umoms(2,2,2)
       NN=n1+n2
       MM=m1+m2
       umoms(1,1,1)=upower3(m1,n1)
       umoms(2,2,2)=upower3(MM,NN)
       umoms(1,2,2)=upower3(MM,NN)*(1.0*m1*n1/(MM*NN))
       umoms(2,1,2)=umoms(1,2,2)
       umoms(2,2,1)=umoms(1,2,2)
       return
       end
       subroutine mix4(m1,n1,m2,n2,umoms)
       integer n1,m1,n2,m2,NN,MM
       double precision umoms(2,2,2,2),upower4
       NN=n1+n2
       MM=m1+m2
       umoms(1,1,1,1)=upower4(m1,n1)
       umoms(1,2,2,2)=upower4(MM,NN)*(m1*n1/(1.0*MM*NN))
       umoms(2,1,2,2)=umoms(1,2,2,2)
       umoms(2,2,1,2)=umoms(1,2,2,2)
       umoms(2,2,2,1)=umoms(1,2,2,2)
       umoms(2,2,2,2)=upower4(MM,NN)
       return
       end
