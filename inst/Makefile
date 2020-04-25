# FFLAGS = -O3 -C -u --pedantic -Wall 
SFFLAGS = -O3 -C -u --pedantic -Wall
FFLAGS = -O3 -C -u --pedantic -Wall -ftrapv -fimplicit-none -Wall -Wline-truncation -Wcharacter-truncation -Wsurprising -Waliasing -Wunused-parameter -fcheck=all -std=f2008 -pedantic -fbacktrace  -g
# FFLAGS = -O3 -C -u --pedantic -Wall -fbacktrace -g
FC = gfortran
LIBS = -L /home/kolassa/lib -ldcdf -lran 
LIBS = -L . -ldcdf -lran 
RESEARCH = ~/Research1
checktest.exe: checktest.o readout.o mkfn.o moments.o writemoms.o mom2cum.o stdcum.o rstdcum.o cornwrapper.o terms.o isecnorm.o tvpack.o evalu.o pprank.o score.o tmom.o univariate.o realpg.o realpgs.o realpf.o realph.o realphs.o realpd.o realpc.o realpcs.o realpe.o fillp.o realpk.o realpks.o mvnpack.o edswrapper.o inside.o makexvn.o justrho.o gcf.o readinput.o texout.o rndstart.o
	$(FC)    $(FFLAGS) -o checktest.exe checktest.o readout.o mkfn.o moments.o writemoms.o mom2cum.o stdcum.o rstdcum.o cornwrapper.o terms.o isecnorm.o tvpack.o evalu.o pprank.o score.o tmom.o univariate.o realpg.o realpgs.o realph.o realphs.o realpd.o realpc.o realpcs.o realpf.o realpe.o fillp.o realpk.o realpks.o mvnpack.o edswrapper.o inside.o makexvn.o justrho.o gcf.o readinput.o texout.o rndstart.o $(LIBS)
checkmom.exe: checkmom.o readout.o mkfn.o moments.o writemoms.o mom2cum.o stdcum.o rstdcum.o cornwrapper.o terms.o isecnorm.o tvpack.o evalu.o pprank.o score.o tmom.o univariate.o realpg.o realpgs.o realpf.o realph.o realphs.o realpd.o realpc.o realpcs.o realpe.o fillp.o realpk.o realpks.o mvnpack.o texout.o makexvn.o
	$(FC)    $(FFLAGS) -o checkmom.exe checkmom.o readout.o mkfn.o moments.o writemoms.o mom2cum.o stdcum.o rstdcum.o cornwrapper.o terms.o isecnorm.o tvpack.o evalu.o pprank.o score.o tmom.o univariate.o realpg.o realpgs.o realph.o realphs.o realpd.o realpc.o realpcs.o realpf.o realpe.o fillp.o realpk.o realpks.o  mvnpack.o texout.o makexvn.o $(LIBS)
dwilding1.exe: dwilding.o test1.o dwildings.o mix3.o readinput.o writemoms.o upower.o altern.o moments.o
	$(FC)    $(FFLAGS) -o dwilding1.exe dwilding.o test1.o dwildings.o mix3.o readinput.o writemoms.o upower.o altern.o moments.o
iwilding1.exe: iwilding.o itest1.o iwildings.o readinput.o altern.o
	$(FC)    $(FFLAGS) -o iwilding1.exe iwilding.o itest1.o iwildings.o readinput.o altern.o
dwilding.exe: dwilding.o test.o dwildings.o readinput.o mkfn.o altern.o
	$(FC)    $(FFLAGS) -o dwilding.exe dwilding.o test.o dwildings.o readinput.o mkfn.o altern.o
iwilding.exe: iwilding.o itest.o readinput.o iwildings.o altern.o mkfn.o
	$(FC)    $(FFLAGS) -o iwilding.exe iwilding.o itest.o readinput.o iwildings.o altern.o mkfn.o
allperm.exe: allperm.o nextp.o dumpout1.o mkfn.o realphs.o realph.o parth.o parths.o ufi.o partg.o realpg.o fillp.o partf.o realpf.o partd.o realpd.o realpk.o partk.o realpks.o partks.o parte.o realpe.o partc.o realpc.o realpcs.o partcs.o partgs.o realpgs.o partl.o univariate.o permdriver.o mvnpack.o mcsampsp.o checkone.o readinput.o
	$(FC)    $(FFLAGS) -o allperm.exe allperm.o nextp.o dumpout1.o mkfn.o realphs.o realph.o parth.o parths.o ufi.o partg.o realpg.o fillp.o partf.o realpf.o partd.o realpd.o realpk.o partk.o realpks.o partks.o parte.o realpe.o partc.o realpc.o realpcs.o partcs.o partgs.o realpgs.o partl.o univariate.o permdriver.o mvnpack.o mcsampsp.o checkone.o readinput.o $(LIBS)
checkp.exe: checkpdriver.o checkp.o fillp.o mvnpack.o
	$(FC)    $(FFLAGS) -o checkp.exe checkp.o fillp.o checkpdriver.o $(LIBS) mvnpack.o
makenext.exe: makenext.o altern.o readout.o mkfn.o readinput.o updater.o findc.o dumpout1.o
	$(FC)    $(FFLAGS) -o makenext.exe makenext.o altern.o readout.o mkfn.o readinput.o updater.o findc.o dumpout1.o
permdriver.o: permdriver.F90
	$(FC)    $(FFLAGS) -c -DBIGINT permdriver.F90
allperm.o: ../src/allperm.F90
	$(FC)    $(FFLAGS) -c -DBIGINT -DSTANDALONE ../src/allperm.F90
univariate.o: ../src/univariate.f90 fillp.o
	$(FC)    $(FFLAGS) -c ../src/univariate.f90
justrho.o: ../src/justrho.f90
	$(FC)    $(FFLAGS) -c ../src/justrho.f90
makexvn.o: ../src/makexvn.f90
	$(FC)    $(FFLAGS) -c ../src/makexvn.f90
checkone.o: checkone.f90
	$(FC)    $(FFLAGS) -c checkone.f90
partg.o: partg.f90
	$(FC)    $(FFLAGS) -c partg.f90
partc.o: partc.f90
	$(FC)    $(FFLAGS) -c partc.f90
partcs.o: partcs.f90
	$(FC)    $(FFLAGS) -c partcs.f90
partgs.o: partgs.f90
	$(FC)    $(FFLAGS) -c partgs.f90
partd.o: partd.f90
	$(FC)    $(FFLAGS) -c partd.f90
parte.o: parte.f90
	$(FC)    $(FFLAGS) -c parte.f90
partl.o: partl.f90
	$(FC)    $(FFLAGS) -c partl.f90
partk.o: partk.f90
	$(FC)    $(FFLAGS) -c partk.f90
partks.o: partks.f90
	$(FC)    $(FFLAGS) -c partks.f90
partf.o: partf.f90
	$(FC)    $(FFLAGS) -c partf.f90
parths.o: parths.f90
	$(FC)    $(FFLAGS) -c parths.f90
parth.o: parth.f90
	$(FC)    $(FFLAGS) -c parth.f90
ufi.o: ufi.f90
	$(FC)    $(FFLAGS) -c ufi.f90
realpk.o: ../src/realpk.f90
	$(FC)    $(FFLAGS) -c ../src/realpk.f90
realpks.o: ../src/realpks.f90
	$(FC)    $(FFLAGS) -c ../src/realpks.f90
realpg.o: ../src/realpg.f90
	$(FC)    $(FFLAGS) -c ../src/realpg.f90
realphs.o: ../src/realphs.f90 fillp.o
	$(FC)    $(FFLAGS) -c ../src/realphs.f90
realph.o: ../src/realph.f90
	$(FC)    $(FFLAGS) -c ../src/realph.f90
realpf.o: ../src/realpf.f90
	$(FC)    $(FFLAGS) -c ../src/realpf.f90
realpc.o: ../src/realpc.f90
	$(FC)    $(FFLAGS) -c ../src/realpc.f90
realpcs.o: ../src/realpcs.f90
	$(FC)    $(FFLAGS) -c ../src/realpcs.f90
realpgs.o: ../src/realpgs.f90
	$(FC)    $(FFLAGS) -c ../src/realpgs.f90
realpd.o: ../src/realpd.f90
	$(FC)    $(FFLAGS) -c ../src/realpd.f90
realpe.o: ../src/realpe.f90
	$(FC)    $(FFLAGS) -c ../src/realpe.f90
writemoms.o: writemoms.f90
	$(FC)    $(FFLAGS) -c writemoms.f90
dumpout1.o: dumpout1.F90
	$(FC)    $(FFLAGS) -c -DBIGINT dumpout1.F90
u4.o: u4.f90
	$(FC)    $(FFLAGS) -c u4.f90
itest1.o: itest1.F90
	$(FC)    $(FFLAGS) -o itest1.o -c -DDOINT itest1.F90
readinput.o: readinput.f90
	$(FC)    $(FFLAGS) -c readinput.f90
dwilding.o: ../src/wilding.F90
	 $(FC)    $(FFLAGS) -o dwilding.o -DDEBUG -c ../src/wilding.F90
test.o: test.F90
	$(FC)    $(FFLAGS) -o test.o -c test.F90
test1.o: test1.F90
	$(FC)    $(FFLAGS) -o test1.o -c test1.F90
iwilding.o: ../src/wilding.F90
	$(FC)    $(FFLAGS) -o iwilding.o -c ../src/wilding.F90 -DDOINT -DDEBUG
itest.o: test.F90
	$(FC)    $(FFLAGS) -o itest.o -c test.F90 -DDOINT
clean:
	rm -f *.o *.exe *.so ../src/*.o ../src/*.so gcf.f uout* mout* copyme fort.66 fort.33
distclean: clean
	rm -f dwilding.f90 *.temp.* libdcdf.a libran.a 
dwildings.o: ../src/wildings.F90
	$(FC)    $(FFLAGS) -o dwildings.o -c ../src/wildings.F90
iwildings.o: ../src/wildings.F90
	$(FC)    $(FFLAGS) -o iwildings.o -DDOINT -c ../src/wildings.F90
mix3.o: mix3.f90
	$(FC)    $(FFLAGS) -c mix3.f90
upower.o: upower.f90
	$(FC)    $(FFLAGS) -c upower.f90
checktest.o: checktest.f90
	$(FC)    $(FFLAGS) -c checktest.f90
checkmom.o: checkmom.f90
	$(FC)    $(FFLAGS) -c checkmom.f90
moments.o: ../src/moments.f90
	$(FC)    $(FFLAGS) -c ../src/moments.f90
mkfn.o: ../src/mkfn.f90
	$(FC)    $(FFLAGS) -c ../src/mkfn.f90
readout.o: ../src/readout.F90
	$(FC)    $(FFLAGS) -c -DBIGINT ../src/readout.F90 -DDEBUGME
mom2cum.o: ../src/mom2cum.f
	$(FC)    $(FFLAGS) -c ../src/mom2cum.f
stdcum.o: ../src/stdcum.f90
	$(FC)    $(FFLAGS) -c ../src/stdcum.f90
rstdcum.o: ../src/rstdcum.F90
	$(FC)    $(FFLAGS) -c -DBIGINT ../src/rstdcum.F90 # -DDEBUGME
evalu.o: ../src/evalu.F90
	$(FC)    $(FFLAGS) -c ../src/evalu.F90 # -DDEBUGME
cornwrapper.o: ../src/cornwrapper.f90
	$(FC)    $(FFLAGS) -c ../src/cornwrapper.f90
pprank.temp.f: #$(RESEARCH)/Wilcoxon/Fortran/pprank.f
	cp $(RESEARCH)/Wilcoxon/Fortran/pprank.f pprank.temp.f
../src/terms.temp.f: #$(RESEARCH)/Chen/Cornish/src/terms.f
	cp $(RESEARCH)/Chen/Cornish/src/terms.f ../src/terms.temp.f
../src/isecnorm.temp.f90: #$(RESEARCH)/Chen/Cornish/src/isecnorm.f90
	cp $(RESEARCH)/Chen/Cornish/src/isecnorm.f90 ../src/isecnorm.temp.f90
../src/nextp.temp.f90: #$(RESEARCH)/Metzvol/MultNonParam/src/nextp.f90
	cp $(RESEARCH)/Metzvol/MultNonParam/src/nextp.f90 ../src/nextp.temp.f90
terms.o: ../src/terms.temp.f
	$(FC)    $(FFLAGS) -o terms.o -c ../src/terms.temp.f
tvpack.o: ../src/tvpack.temp.f
	$(FC)    $(SFFLAGS) -o tvpack.o -c ../src/tvpack.temp.f
isecnorm.o: ../src/isecnorm.temp.f90
	$(FC)    $(FFLAGS) -o isecnorm.o -c ../src/isecnorm.temp.f90
nextp.o: ../src/nextp.temp.f90
	$(FC)    $(FFLAGS) -o nextp.o -c ../src/nextp.temp.f90
pprank.o: pprank.temp.f
	$(FC)    $(FFLAGS) -c -o pprank.o pprank.temp.f
score.o: score.f
	$(FC)    $(FFLAGS) -c score.f
fillp.o: ../src/fillp.f90
	$(FC)    $(FFLAGS) -c -o fillp.o ../src/fillp.f90
tmom.o: ../src/tmom.f90
	$(FC)    $(FFLAGS) -c ../src/tmom.f90
dewei.o: dewei.f
	$(FC)    $(FFLAGS) -c dewei.f
mcsampsp.o: mcsampsp.f90
	$(FC)    $(FFLAGS) -c mcsampsp.f90
# Get bvnd
../src/tvpack.temp.f:
	wget -O - http://www.math.wsu.edu/faculty/genz/software/fort77/tvpack.f | tail -n +75 > ../src/tvpack.temp.f
../src/mvnpack.temp.f:
	wget -O - http://www.math.wsu.edu/faculty/genz/software/fort77/mvnpack.f | tail -n +101 > ../src/mvnpack.temp.f
mvnpack.o: ../src/mvnpack.temp.f
	$(FC)    $(SFFLAGS) -c -o mvnpack.o ../src/mvnpack.temp.f
edswrapper.o: ../src/edswrapper.f90
	$(FC)    $(FFLAGS) -c ../src/edswrapper.f90
texout.o: texout.f90
	$(FC)    $(FFLAGS) -c texout.f90
inside.o: ../src/inside.F90
	$(FC)    $(FFLAGS) -c ../src/inside.F90 -DDEBUGME
checkpdriver.o: checkpdriver.f90
	$(FC)    $(FFLAGS) -c checkpdriver.f90
checkp.o: checkp.f90
	$(FC)    $(FFLAGS) -c checkp.f90
make local: ../src/mvnpack.temp.f pprank.temp.f ../src/tvpack.temp.f ../src/isecnorm.temp.f90 ../src/terms.temp.f ../src/nextp.temp.f90
	touch mvnpack.temp.f
altern.o:  ../src/altern.f90
	$(FC)    $(FFLAGS) -c ../src/altern.f90
makenext.o: makenext.F90 dumpout1.o
	$(FC)    $(FFLAGS) -DBIGINT -c makenext.F90
updater.o: updater.F90
	$(FC)    $(FFLAGS) -DBIGINT -c updater.F90
findc.o: findc.F90
	$(FC)    $(FFLAGS) -DBIGINT -c findc.F90
checklist.exe: checklist.f90
	$(FC)    $(FFLAGS) -o checklist.exe checklist.f90
gcf.f:
	wget http://netlib.org/toms/386.gz ; gunzip 386.gz ; mv 386 gcf.f
gcf.o: gcf.f
	$(FC)    $(SFFLAGS) -fdefault-integer-8 -c gcf.f
rndstart.o: rndstart.f
	$(FC)    $(FFLAGS) -c rndstart.f
outoflibs:
	cp /tmp/Kolassa/ranlib.f/src/snorm.f /tmp/Kolassa/ranlib.f/src/ranf.f /tmp/Kolassa/ranlib.f/src/ignlgi.f /tmp/Kolassa/ranlib.f/src/setall.f /tmp/Kolassa/ranlib.f/src/inrgcm.f /tmp/Kolassa/ranlib.f/src/mltmod.f /tmp/Kolassa/ranlib.f/src/getcgn.f /tmp/Kolassa/ranlib.f/src/initgn.f /tmp/Kolassa/ranlib.f/src/qrgnin.f /tmp/Kolassa/dcdflib.f/src/dinvnr.f /tmp/Kolassa/dcdflib.f/src/stvaln.f /tmp/Kolassa/dcdflib.f/src/devlpl.f /tmp/Kolassa/dcdflib.f/src/cumnor.f /tmp/Kolassa/dcdflib.f/src/ipmpar.f /tmp/Kolassa/dcdflib.f/src/spmpar.f ../src; 
cleanoutoflibs:
	/bin/rm ../src/snorm.f ../src/ranf.f ../src/ignlgi.f ../src/setall.f ../src/inrgcm.f ../src/mltmod.f ../src/getcgn.f ../src/initgn.f ../src/qrgnin.f ../src/dinvnr.f ../src/stvaln.f ../src/devlpl.f ../src/cumnor.f ../src/ipmpar.f ../src/spmpar.f
copyme: ../src/isecnorm.temp.f90 ../src/terms.temp.f ../src/nextp.temp.f90 pprank.temp.f ../src/mvnpack.temp.f ../src/tvpack.temp.f
	echo "done" > copyme
libran.a:
	mkdir tempdir; wget http://www.netlib.org/random/ranlib.f.tar.gz; gzip -dc ranlib.f.tar.gz |(umask 0; cd tempdir; tar xvf -); cd tempdir/ranlib.f/src; gfortran -c *.f; ar -r libran.a *.o; cp libran.a ../../.. ; cd ../../.. ; rm ranlib.f.tar.gz ; rm -r tempdir

libdcdf.a:
	mkdir tempdir; wget www.netlib.org/random/dcdflib.f.tar.gz ; gzip -dc dcdflib.f.tar.gz | (umask 0; cd tempdir; tar xvf -); cd tempdir/dcdflib.f/src; gfortran -c  *.f; ar -r ../../../libdcdf.a *.o
