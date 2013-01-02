/**********************************
 *    attacks.h                   *
 *    Colin Frayn                 *
 *    March 2001                  *
 **********************************/

/*
   Contains the specific defines for attacks.c
*/

#ifndef ATTACKS_H
#define ATTACKS_H

void GenerateAttacks(Board *);
BITBOARD AttacksTo(const Board *, const int);
int SEE(const Board *,const int, const int, const int);
BITBOARD ReevaluateAttacks(const Board *, BITBOARD, const int, const int);
int EvaluateOwnership(Board *,int);

#endif /* ATTACKS_H */
