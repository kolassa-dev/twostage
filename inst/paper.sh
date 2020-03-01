make libdcdf.a 
make libran.a 
make allperm.exe 
make checktest.exe 
echo "5 5 5 5 1.5 .98 .95 \n 0 0 0 0 0 0 0\n" | allperm.exe 
echo "5 5 5 5 1.5 .98 .95 \n5 5 5 5 1.5 .99 .975  \n 0 0 0 0 0 0 0\n" | checktest.exe
