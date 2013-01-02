/**********************************
 *    tactics.h                   *
 *    Colin Frayn                 *
 *    March 2001                  *
 **********************************/

/*
   Contains the specific defines for tactics.c
*/

#ifndef TACTICS_H
#define TACTICS_H

int TactPawn(const int,const Board *,const int);
int TactRook(const int,const Board *,const int);
int TactKnight(const int,const Board *,const int);
int TactBishop(const int,const Board *,const int);
int TactQueen(const int,const Board *,const int);
int TactKing(const int,const Board *,const int);

int BackwardsPawn(const int,const Board *,const int, const BOOL);
int KingDefence(const Board *, const int, const int);
int TacticsPositional(const Board *);
int DriveAway(const int, const Board *, const int);
int ScorePassedPawn(int,Board *,int *);

#endif /* TACTICS_H */
