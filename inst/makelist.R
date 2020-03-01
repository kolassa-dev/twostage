makelist<-function(mx){
   mat<-array(NA,c((1+mx)^4,4))
   scores<-rep(NA,(1+mx)^4)
   hh<-0
   for(ii in 0:mx) for(jj in 0:mx) for(kk in 0:mx) for(ll in 0:mx){
      hh<-hh+1
      mat[hh,1]<-ii
      mat[hh,2]<-jj
      mat[hh,3]<-kk
      mat[hh,4]<-ll
      scores[hh]<-sum(sort(mat[hh,])*((1+mx)^(3:0)))
   }
   outmat<-mat[order(scores),]
   return(rbind(outmat[-1,],outmat[1,]))
}
write.table(makelist(10),file="all",quote=FALSE,col.names=FALSE,
   row.names=FALSE)
