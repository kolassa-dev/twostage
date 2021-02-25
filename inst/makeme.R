system("/bin/rm tempout")
for(m1 in 2:9) for(m2 in 2:9) for(n1 in 2:9) for (n2 in 2:9){
   cat(m1," ",n1," ",m1," ",n2," -1 .98 .95\n",file="tempout",append=TRUE)
}
cat("0 0 0 0 -1 .98 .95\n",file="tempout",append=TRUE)
