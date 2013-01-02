/*
------------------------------------------------------------------------------
rand64.c: A pseudo-random number generator for 64-bit machines.
based on Isaac64 by Bob Jenkins, 1996.  Public Domain.
------------------------------------------------------------------------------
*/

#ifndef RAND64
#include "common.h"
#include "rand64.h"
#endif

extern    u64 RandResult[RANDSIZ], RandCount;
static    u64 mm[RANDSIZ];
static    u64 aa = 0, bb = 0, cc = 0;

#define mix(a,b,c,d,e,f,g,h) { \
   a -=  e; f ^= h>>9;  h +=  a; \
   b -=  f; g ^= a<<9;  a +=  b; \
   c -=  g; h ^= b>>23; b +=  c; \
   d -=  h; a ^= c<<15; c +=  d; \
   e -=  a; b ^= d>>14; d +=  e; \
   f -=  b; c ^= e<<20; e +=  f; \
   g -=  c; d ^= f>>17; f +=  g; \
   h -=  d; e ^= g<<14; g +=  h; \
}

#define ind(mm,x)  (*(u64 *)((u8 *)(mm) + ((x) & ((RANDSIZ-1)<<3))))

#define rngstep(mix,a,b,mm,m,m2,r,x) { \
  x = *m;  \
  a = (mix) + *(m2++); \
  *(m++) = y = ind(mm,x) + a + b; \
  *(r++) = b = ind(mm,y>>RANDSIZL) + x; \
}

void isaac64(void) {
  register u64 a,b,x,y,*m,*m2,*r,*mend;
  m = mm; r = RandResult;
  a = aa; b = bb + (++cc);
  for (m = mm, mend = m2 = m+(RANDSIZ/2); m<mend; ) {
    rngstep(~(a^(a<<21)), a, b, mm, m, m2, r, x);
    rngstep(  a^(a>>5)  , a, b, mm, m, m2, r, x);
    rngstep(  a^(a<<12) , a, b, mm, m, m2, r, x);
    rngstep(  a^(a>>33) , a, b, mm, m, m2, r, x);
  }
  for (m2 = mm; m2<mend; ) {
    rngstep(~(a^(a<<21)), a, b, mm, m, m2, r, x);
    rngstep(  a^(a>>5)  , a, b, mm, m, m2, r, x);
    rngstep(  a^(a<<12) , a, b, mm, m, m2, r, x);
    rngstep(  a^(a>>33) , a, b, mm, m, m2, r, x);
  }
  bb = b; aa = a;
}

void randinit(int flag) {
   int i;
   u64 a,b,c,d,e,f,g,h;
   aa = bb = cc = (u64)0;
   a = b = c = d = e = f = g = h = (u64)0x9e3779b97f4a7c13;    /* golden ratio */

   for (i = 0; i<4; ++i) {                  /* scramble it */
     mix(a,b,c,d,e,f,g,h);
   }

   for (i = 0; i<RANDSIZ; i +=  8) {        /* fill in mm[] with messy stuff */
     if (flag) {                      /* use all the information in the seed */
       a +=  RandResult[i  ]; b +=  RandResult[i+1];
       c +=  RandResult[i+2]; d +=  RandResult[i+3];
       e +=  RandResult[i+4]; f +=  RandResult[i+5];
       g +=  RandResult[i+6]; h +=  RandResult[i+7];
     }
     mix(a,b,c,d,e,f,g,h);
     mm[i  ] = a; mm[i+1] = b; mm[i+2] = c; mm[i+3] = d;
     mm[i+4] = e; mm[i+5] = f; mm[i+6] = g; mm[i+7] = h;
   }

   if (flag) {  /* do a second pass to make all of the seed affect all of mm */
     for (i = 0; i<RANDSIZ; i +=  8) {
       a +=  mm[i  ]; b +=  mm[i+1]; c +=  mm[i+2]; d +=  mm[i+3];
       e +=  mm[i+4]; f +=  mm[i+5]; g +=  mm[i+6]; h +=  mm[i+7];
       mix(a,b,c,d,e,f,g,h);
       mm[i  ] = a; mm[i+1] = b; mm[i+2] = c; mm[i+3] = d;
       mm[i+4] = e; mm[i+5] = f; mm[i+6] = g; mm[i+7] = h;
     }
   }

   isaac64();              /* fill in the first set of results */
   RandCount = RANDSIZ;    /* prepare to use the first set of results */
}

