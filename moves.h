/**********************************
 *    moves.h                     *
 *    Colin Frayn                 *
 *    March 2001                  *
 **********************************/

/*
   Contains the specific defines for checks.c
*/

#ifndef MOVES_H
#define MOVES_H

MOVE *GenerateMoves(const Board *,const int, MOVE *);
MOVE *GenerateCaptures(const Board *, const int, MOVE *);
MOVE *GenerateCapturesTo(const Board *, const int, const int, MOVE *);
MOVE *GenerateBlockingMoves(const Board *, const int, const int, MOVE *);
MOVE *GenerateCheckEvasions(const Board *, const int, MOVE *,const int);
MOVE *GeneratePromotions(const Board *, const int, MOVE *);
MOVE *GenerateNonQuiescentMoves(const Board *, const int, MOVE *);
int Check_Castle(Board *,const int);
BOOL IsLegalCastle(Board *, const MOVE);
long int CountMoves(Board *, const int, const int);
void PrintMove(const MOVE,const BOOL,FILE *);
Undo DoMove(Board *,const MOVE);
void UndoMove(Board *,const MOVE,const Undo);
BITBOARD QueenMoves(const Board *,int);
BITBOARD RookMoves(const Board *,int);
BITBOARD BishopMoves(const Board *,int);

#endif /* MOVES_H */
