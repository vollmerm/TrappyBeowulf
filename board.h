/**********************************
 *    board.h                     *
 *    Colin Frayn                 *
 *    March 2001                  *
 **********************************/

/*
  Contains the specific defines for board.c
 */

#ifndef BOARD_H
#define BOARD_H

BITBOARD RotateBoard_R90(const BITBOARD);
BITBOARD RotateBoard_L90(const BITBOARD);
BITBOARD RotateBoard_R45(const BITBOARD);
BITBOARD RotateBoard_L45(const BITBOARD);
BITBOARD InvRotateBoard_R45(const BITBOARD);
BITBOARD InvRotateBoard_L45(const BITBOARD);
int Count(const BITBOARD);
int FirstPiece(const BITBOARD);
void PrintBoard(const Board);
void PrintBitboard(const BITBOARD);
void SetBoard(Board *, char *, BOOL);
void ResetBoard(Board *);
int Pts(Board, const int);
void PrintPiece(int);

#endif          /* BOARD_H */
