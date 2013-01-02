/**********************************
 *    checks.h                    *
 *    Colin Frayn                 *
 *    March 2001                  *
 **********************************/

/*
   Contains the specific defines for checks.c
*/

#ifndef CHECKS_H
#define CHECKS_H

int InCheck(const Board *, const int);
BOOL InCM(Board *, const int);
int GivesCheck(const Board *, const MOVE);

#endif /* CHECKS_H */
