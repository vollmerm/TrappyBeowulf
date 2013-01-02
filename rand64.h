/*
------------------------------------------------------------------------------
rand64.h: definitions for a psuedo-random number generator
based on Isaac64 by Bob Jenkins, 1996, Public Domain
modifications by Ron Murawski, 2001
------------------------------------------------------------------------------
*/

#ifndef RAND64
#define RAND64

#define RANDSIZL   (8)
#define RANDSIZ    (1<<RANDSIZL)

/*  ---- Define some globals ---- */

u64 RandResult[RANDSIZ], RandCount;

/* ----- function prototypes ----- */

void randinit(int flag);
void isaac64(void);

/* ----- Call rand64() to retrieve a single 64-bit random value ----- */

#define Rand64() \
   (!RandCount-- ? (isaac64(), RandCount=RANDSIZ-1, RandResult[RandCount]) : \
                 RandResult[RandCount])

#endif  /* RAND64 */

