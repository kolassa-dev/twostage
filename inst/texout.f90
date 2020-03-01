      subroutine texout(ii,m1,n1,m2,n2,x1,x2,x1c,x2c,ok,okc)
      integer ii,m1,n1,m2,n2
      double precision x1,x2,x1c,x2c,ok,okc
      if(ii.eq.1) then
         write(33,*) "\begin{tabular}{|c|c|c|c|c|c|c|c|c|c|}\hline"
         write(33,*) "$m_1$&$n_1$&$m_2$&$n_2$&\multicolumn{2}{c|}{Uncorrected}&"
         write(33,*) "\multicolumn{2}{c|}{Corrected}&\multicolumn{2}{c|}{True Size}\\"
         write(33,*) "  &  &  & & $x_1$     &  $x_2$    &$x_1$       & $x_2$&Naive&Cor.\\"
         write(33,*) "\hline"
      end if
      if(ii.ge.1) write(33,'(4(i2,"&"),4(f5.2,"&"),f6.4,"&",f6.4,"\\")') m1,n1,m2,n2,x1,x2,x1c,x2c,1.0d0-ok,1.0d0-okc
      if(ii.le.0) write(33,*) "\hline\end{tabular}"
      return
      end
