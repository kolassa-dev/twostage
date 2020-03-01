xx<-array(NA,c(10000000,8))
for(j in seq(dim(xx)[1])) xx[j,]<-sample(seq(dim(xx)[2]),dim(xx)[2])
yy<-unique(as.data.frame(xx))
names(yy)<-c("i","j","k","l","a","b","c","d")
#Calculate E[I_ij]=1/2=pi0
mean((yy[,"i"]<yy[,"j"]))
#Calculate E[I_ij I_kj]=1/3=pi1
mean( (yy[,"i"]<yy[,"j"])& (yy[,"k"]<yy[,"j"]))
#Calculate E[I_ij I_kj I_aj ]=1/4=pi2
mean((yy[,"i"]<yy[,"j"])&(yy[,"k"]<yy[,"j"])&(yy[,"a"]<yy[,"j"]))
#Calculate E[I_ij I_kj I_aj I_ab]=3/20=pi3
mean((yy[,"i"]<yy[,"j"])&(yy[,"k"]<yy[,"j"])&(yy[,"a"]<yy[,"j"])&(yy[,"a"]<yy[,"b"]))
#Calculate E[I_ij I_kj I_ib]=5/24=pi4
mean((yy[,"i"]<yy[,"j"])&(yy[,"k"]<yy[,"j"])&(yy[,"i"]<yy[,"b"]))
#Calculate E[I_ij I_kj I_ib I_ab]=2/15=pi5
mean((yy[,"i"]<yy[,"j"])&(yy[,"k"]<yy[,"j"])&(yy[,"i"]<yy[,"b"])&(yy[,"a"]<yy[,"b"]))
#Calculate E[I_ij I_kj I_aj I_cj]=1/5=pi6
mean((yy[,"i"]<yy[,"j"])&(yy[,"k"]<yy[,"j"])&(yy[,"a"]<yy[,"j"])&(yy[,"c"]<yy[,"j"]))
#Calculate E[I_ij I_kj I_ib I_id]=3/20=pi7.  Note will not be same as above under Ha
mean((yy[,"i"]<yy[,"j"])&(yy[,"k"]<yy[,"j"])&(yy[,"i"]<yy[,"b"])&(yy[,"i"]<yy[,"d"]))
#Calculate E[I_ij I_kj I_ib I_kb]=1/6=pi8
mean((yy[,"i"]<yy[,"j"])&(yy[,"k"]<yy[,"j"])&(yy[,"i"]<yy[,"b"])&(yy[,"k"]<yy[,"b"]))
#Calculate E[I_ij I_ib ]=1/3=pi9
mean((yy[,"i"]<yy[,"j"])&(yy[,"i"]<yy[,"b"]))
#Calculate E[I_ij I_kj I_id I_kb]=2/15=pi10
mean((yy[,"i"]<yy[,"j"])&(yy[,"k"]<yy[,"j"])&(yy[,"i"]<yy[,"d"])&(yy[,"k"]<yy[,"b"]))
#Calculate E[I_ij I_il I_aj]=5/24=pi11
mean((yy[,"i"]<yy[,"j"])&(yy[,"i"]<yy[,"b"])&(yy[,"a"]<yy[,"j"]))
#Calculate E[I_ij I_il I_ib]=1/4=pi12
mean((yy[,"i"]<yy[,"j"])&(yy[,"i"]<yy[,"b"])&(yy[,"i"]<yy[,"d"]))
#Calculate E[I_ij I_il I_ib I_kb]=3/20=pi13
mean((yy[,"i"]<yy[,"j"]) &(yy[,"i"]<yy[,"l"]) &(yy[,"i"]<yy[,"b"]) &(yy[,"k"]<yy[,"b"]))
#Calculate E[I_ij I_il I_ib I_id]=1/5=pi_14
mean((yy[,"i"]<yy[,"j"])&(yy[,"i"]<yy[,"l"])&(yy[,"i"]<yy[,"b"])&(yy[,"i"]<yy[,"d"]))
#Calculate E[I_ij I_il I_al I_cj]=2/15=pi_15
mean((yy[,"i"]<yy[,"j"])&(yy[,"i"]<yy[,"l"])&(yy[,"a"]<yy[,"l"])&(yy[,"c"]<yy[,"j"]))
#Calculate E[I_ij I_kl I_il ]=5/24=pi_16
mean((yy[,"i"]<yy[,"j"])&(yy[,"k"]<yy[,"l"])&(yy[,"i"]<yy[,"l"]))
