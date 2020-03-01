      program checkpdriver
      double precision delta
      integer(kind=8) nsamp
      delta=1.0d0
      nsamp=100000000_8
      call fillps(delta)
      call checkp(delta,nsamp)
      end
