      double precision function upower2(m,n)
implicit none
      integer m,n
      upower2=m*n/2.+m*n*(m-1)*(n-1)/4.+m*n*(m-1)/3.+m*n*(n-1)/3.
      return
      end
      double precision function upower3(m,n)
implicit none
      integer m,n
      double precision upower2
      upower3=3*upower2(m,n)*m*n/2.-m**3*n**3/4.
      return
      end
      double precision function upower4(m,n)
implicit none
      integer m,n
      upower4=(m**4*n**4)/16. + (m**4*n**3)/8. + (m**4*n**2)/48. - (m**4*n)/120. + (m**3*n**4)/8. +&
 (m**3*n**3)/6. + (m**3*n**2)/40. - (m**3*n)/60. + (m**2*n**4)/48. + (m**2*n**3)/40. - &
(m**2*n**2)/240. - (m**2*n)/120. - (m*n**4)/120. - (m*n**3)/60. - (m*n**2)/120.
      return
      end
