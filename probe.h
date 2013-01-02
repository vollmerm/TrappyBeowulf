/**********************************
 *    probe.h                     *
 *    Colin Frayn                 *
 *    April 2001                  *
 **********************************/

/*
   Contains the specific defines for probe.c
*/

#ifndef PROBE_H
#define PROBE_H

void InitialiseTB(void);
int InitialiseCount(int *, int, BITBOARD);
int ProbeEGTB(Board *, int *, int);

#endif /* PROBE_H */
