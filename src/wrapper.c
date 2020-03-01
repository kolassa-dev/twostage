#include <R.h>
#include <Rmath.h>

void F77_SUB(rndstart)(void) { GetRNGstate(); }
void F77_SUB(rndend)(void) { PutRNGstate(); }
float F77_SUB(snorm)(void) { return norm_rand(); }
double F77_SUB(dinvnr)(double *p, double *q) { 
   double pp = *p;
   double qq = *q;
   double out;
   printf("pp %f",pp);
   out=qnorm(pp, 0.0, 1.0,1,0); 
   printf("out %f",out);
   return out;
}
