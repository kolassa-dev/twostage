      subroutine dewei(m1,n1,m2,n2,mom1,mom2,mom3,mom4)
      integer m1,n1,m2,n2
      double precision mom1(2),mom2(2,2),mom3(2,2,2),mom4(2,2,2,2)
      mom1(1)=
     -  (m1*n1)/2.
      mom1(2)=
     -  ((m1 + m2)*(n1 + n2))/2.
      mom2(1,1)=
     -  (m1*n1*(1 + m1 + n1 + 3*m1*n1))/12.
      mom2(2,2)=
     -          ((m1 + m2)*(n1 + n2)*
     -    (1 + m1 + m2 + n1 + n2 + 3*(m1 + m2)*(n1 + n2)))/12.
      mom3(1,1,1)      =
     -  (m1**2*(1 + m1)*n1**2*(1 + n1))/8.
      mom3(2,2,2)      =
     -  ((m1 + m2)**2*(1 + m1 + m2)*(n1 + n2)**2*(1 + n1 + n2))/8.
      mom4(1,1,1,1)      =
     -          (m1*n1*(-2*n1*(1 + n1)**2 + 
     -      m1*(-2 - n1 + 6*n1**2 + 5*n1**3) + 
     -      m1**3*(-2 + 5*n1 + 30*n1**2 + 15*n1**3) + 
     -      m1**2*(-4 + 6*n1 + 40*n1**2 + 30*n1**3)))/240.
      mom4(2,2,2,2)      =
     -          ((m1 + m2)*(n1 + n2)*
     -    (-2*(n1 + n2)*(1 + n1 + n2)**2 + 
     -      (m1 + m2)*(-2 - n1 - n2 + 6*(n1 + n2)**2 + 
     -         5*(n1 + n2)**3) + 
     -      (m1 + m2)**3*(-2 + 5*(n1 + n2) + 30*(n1 + n2)**2 + 
     -         15*(n1 + n2)**3) + 
     -      (m1 + m2)**2*(-4 + 6*(n1 + n2) + 40*(n1 + n2)**2 + 
     -         30*(n1 + n2)**3)))/240.
      mom2(1,2)    =
     -  (m1*n1*(1 + m1 + m2 + n1 + n2 + 3*(m1 + m2)*(n1 + n2)))/12.
      mom3(1,1,2)     =
     -          (m1*n1*((m1 + m2)*(1 + n1)*(n1 + n2) + 
     -      m1*((m1 + m2)*(n1 + n2) + 
     -         n1*(2 + 2*(m1 + m2) + 2*(n1 + n2) + 
     -            3*(m1 + m2)*(n1 + n2)))))/24.
      mom3(1,2,2) =
     -  (m1*(m1 + m2)*(1 + m1 + m2)*n1*(n1 + n2)*(1 + n1 + n2))/8.
      mom4(1,1,1,2)=
     -          (m1*n1*(-2*n1*(1 + n1)*(1 + m1 + m2 + n1 + n2) + 
     -      m1*(1 + n1)*((-2 + 5*n1)*(1 + n1 + n2) + 
     -         (m1 + m2)*(-2 + 5*n1*(1 + 3*(n1 + n2)))) + 
     -      m1**2*((-2 + 5*n1 + 15*n1**2)*(1 + n1 + n2) + 
     -         (m1 + m2)*(-2 + 15*n1**2*(1 + n1 + n2) + 
     -            5*n1*(1 + 3*(n1 + n2))))))/240.
      mom4(1,2,2,2)=
     -          (m1*n1*(-2*(n1 + n2)*(1 + n1 + n2)**2 + 
     -      (m1 + m2)*(-2 - n1 - n2 + 6*(n1 + n2)**2 + 
     -         5*(n1 + n2)**3) + 
     -      (m1 + m2)**3*(-2 + 5*(n1 + n2) + 30*(n1 + n2)**2 + 
     -         15*(n1 + n2)**3) + 
     -      (m1 + m2)**2*(-4 + 6*(n1 + n2) + 40*(n1 + n2)**2 + 
     -         30*(n1 + n2)**3)))/240.
      mom4(1,1,2,2) =
     -          (m1*n1*((m1 + m2)**2*(1 + n1)*
     -       (-2 + 5*(n1 + n2) + 15*(n1 + n2)**2) - 
     -      2*(1 + n1 + n2)*(n1 + n2 + n1*(2 + 3*(n1 + n2))) + 
     -      (m1 + m2)*(-2 + n1 + n2 + 5*(n1 + n2)**2 + 
     -         n1*(-6 - 3*(n1 + n2) + 5*(n1 + n2)**2)) + 
     -      m1*(2*(1 + n1 + n2)*
     -          (-2 - n1 - n2 + n1*(4 + 5*(n1 + n2))) + 
     -         (m1 + m2)**2*
     -          (-6 + 5*(n1 + n2) + 15*(n1 + n2)**2 + 
     -            5*n1*(2 + 15*(n1 + n2) + 9*(n1 + n2)**2)) + 
     -         (m1 + m2)*(-10 - 3*(n1 + n2) + 5*(n1 + n2)**2 + 
     -            n1*(18 + 95*(n1 + n2) + 75*(n1 + n2)**2)))))/720.
      mom2(2,1)=mom2(1,2)
      mom3(1,2,1)=mom3(1,1,2)
      mom3(2,1,1)=mom3(1,1,2)
      mom3(2,1,2)=mom3(1,2,2)
      mom3(2,2,1)=mom3(1,2,2)
      mom4(1,1,2,1)= mom4(1,1,1,2)
      mom4(1,2,1,1)= mom4(1,1,1,2)
      mom4(2,1,1,1)= mom4(1,1,1,2)
      mom4(2,2,2,1)= mom4(1,2,2,2)
      mom4(2,2,1,2)= mom4(1,2,2,2)
      mom4(2,1,2,2)= mom4(1,2,2,2)
      mom4(1,2,2,1)= mom4(1,1,2,2)
      mom4(1,2,1,2)= mom4(1,1,2,2)
      mom4(2,2,1,1)= mom4(1,1,2,2)
      mom4(2,1,2,1)= mom4(1,1,2,2)
      mom4(2,1,1,2)= mom4(1,1,2,2)
      return
      end
