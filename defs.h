/**********************************
 *    defs.h                      *
 *    Colin Frayn                 *
 *    March 2001                  *
 **********************************/

/*
  This file includes all the definitions for various board mappings.
 */

#ifndef DEFS_H
#define DEFS_H

/* Global variables defined and used in main.c.  But these aren't the nasty kind of
 * global variables that you were warned against.  These are *nice* global variables. */
BITBOARD Mask[64],InvMask[64],FileMask[8],RankMask[8],DiagMaska1h8[64],DiagMaska8h1[64];
BITBOARD MovesRank[64][256],MovesFile[64][256],Movesa1h8[64][256],Movesa8h1[64][256];
BITBOARD KnightMoves[64],KingMoves[64],RookMask[64],BishopMask[64];
BITBOARD PawnAttacksBlack[64],PawnAttacksWhite[64],PawnMovesBlack[64],PawnMovesWhite[64];
BITBOARD IMR90[64],MR90[64],IMR45[64],MR45[64],IML45[64],ML45[64];
BITBOARD CastleMaskR90[4],CastleMaskR45[4],CastleMaskL45[4],DoubleKnightMove[64];
BITBOARD QueenMask[64],FullBoard,FileUpMask[64],FileDownMask[64];
BITBOARD AboveMask[8],BelowMask[8],LeftMask[8],RightMask[8], FrontMask[64], BackMask[64];
BITBOARD WhiteKingSide, WhiteQueenSide, BlackKingSide, BlackQueenSide;
BITBOARD LLCorner, LRCorner, ULCorner, URCorner, PawnShieldMaskWhite[64], PawnShieldMaskBlack[64];
BITBOARD RankLeftMask[64], RankRightMask[64], OffsideMask[8], SideMask[64], FlankMask[64];
BITBOARD FlankMaskDown[64], FlankMaskUp[64], PairMaskUp[64], PairMaskDown[64], PairMask[64];
BITBOARD WhiteMask,BlackMask,FPMask1,FPMask2,KingSafetyMask[64], CentreMask, EdgeMask;
BITBOARD DirR[64],DirL[64],DirUR[64],DirUL[64],DirDL[64],DirDR[64];
BITBOARD KnightAttackPromotionW[64], KnightAttackPromotionB[64];
int DiagonalMask_a1h8[64],DiagonalMask_a8h1[64], SideAttack[6];
int NPos, SpaceWon[6][64], SpaceDefended[6][64];
int InvRotateL45[64],InvRotateR45[64],Flip[64],Distance[64][64], KingSafetyPP[17][7];
int Gamestage[256], SquareColour[64], KingArea[64][25],RookTrapped[6];
int CentreBoard[64],CornerBoard2[64],CornerBoard3[64],CentreBoard2[64], Backward[7][8];
KeyType RandomTable[64][13],CastleKey[16];
Openpos *Openings;

/* square -> square mappings for a rotation of 90 degrees anti-clockwise */
int RotateL90[64] = {
  h8,h7,h6,h5,h4,h3,h2,h1,
  g8,g7,g6,g5,g4,g3,g2,g1,
  f8,f7,f6,f5,f4,f3,f2,f1,
  e8,e7,e6,e5,e4,e3,e2,e1,
  d8,d7,d6,d5,d4,d3,d2,d1,
  c8,c7,c6,c5,c4,c3,c2,c1,
  b8,b7,b6,b5,b4,b3,b2,b1,
  a8,a7,a6,a5,a4,a3,a2,a1,
};

/* square -> square mappings for a rotation of 90 degrees clockwise */
int RotateR90[64] = {
  a1,a2,a3,a4,a5,a6,a7,a8,
  b1,b2,b3,b4,b5,b6,b7,b8,
  c1,c2,c3,c4,c5,c6,c7,c8,
  d1,d2,d3,d4,d5,d6,d7,d8,
  e1,e2,e3,e4,e5,e6,e7,e8,
  f1,f2,f3,f4,f5,f6,f7,f8,
  g1,g2,g3,g4,g5,g6,g7,g8,
  h1,h2,h3,h4,h5,h6,h7,h8
};

/* Formatting the rotation board like this makes it much easier to see
 * what is really going on.  These are just mappings effectively, mapping
 * each square in the source bitboard onto a new square in the rotated bitboard
 * such that diagonals are now lined up horizontally.*/

/* This board is used for working out leading diagonals (a1h8 sense).  Effectively
 * it represents the square-square mappings for a rotation of 45 degrees clockwise */
int RotateR45[64] = {
a8,
a7,b8,
a6,b7,c8,
a5,b6,c7,d8,
a4,b5,c6,d7,e8,
a3,b4,c5,d6,e7,f8,
a2,b3,c4,d5,e6,f7,g8,
a1,b2,c3,d4,e5,f6,g7,h8,
b1,c2,d3,e4,f5,g6,h7,
c1,d2,e3,f4,g5,h6,
d1,e2,f3,g4,h5,
e1,f2,g3,h4,
f1,g2,h3,
g1,h2,
h1
};

/* This board is used for working out trailing diagonals (a8h1 sense). Effectively
 * it represents the square-square mappings for a rotation of 45 degrees anti-clockwise */
int RotateL45[64] = {
h8,
g8,h7,
f8,g7,h6,
e8,f7,g6,h5,
d8,e7,f6,g5,h4,
c8,d7,e6,f5,g4,h3,
b8,c7,d6,e5,f4,g3,h2,
a8,b7,c6,d5,e4,f3,g2,h1,
a7,b6,c5,d4,e3,f2,g1,
a6,b5,c4,d3,e2,f1,
a5,b4,c3,d2,e1,
a4,b3,c2,d1,
a3,b2,c1,
a2,b1,
a1   
};

/* These simply store the length of the diagonal in the required sense */
int DiagonalLength_a1h8[64] = {
  1,2,3,4,5,6,7,8,
  2,3,4,5,6,7,8,7,
  3,4,5,6,7,8,7,6,
  4,5,6,7,8,7,6,5,
  5,6,7,8,7,6,5,4,
  6,7,8,7,6,5,4,3,
  7,8,7,6,5,4,3,2,
  8,7,6,5,4,3,2,1
};

int DiagonalLength_a8h1[64] = {
  8,7,6,5,4,3,2,1,
  7,8,7,6,5,4,3,2,
  6,7,8,7,6,5,4,3,
  5,6,7,8,7,6,5,4,
  4,5,6,7,8,7,6,5,
  3,4,5,6,7,8,7,6,
  2,3,4,5,6,7,8,7,
  1,2,3,4,5,6,7,8 
};

/* These store the number of bits rotated bitboards need to be shifted to extract
 * the required row.  For example, if I wanted the diagonal from a6-c8, which is in the
 * a1h8 sense, I take the correct bitboard, namely the R45 one, and shift it right
 * by the stated amount (3) and then and it with (2^l)-1 where l is the diagonal
 * length. */
int DiagShifts_a1h8[64] = {
   0, 1, 3, 6,10,15,21,28,
   1, 3, 6,10,15,21,28,36,
   3, 6,10,15,21,28,36,43,
   6,10,15,21,28,36,43,49,
  10,15,21,28,36,43,49,54,
  15,21,28,36,43,49,54,58,
  21,28,36,43,49,54,58,61,
  28,36,43,49,54,58,61,63
};

int DiagShifts_a8h1[64] = {
  28,21,15,10, 6, 3, 1, 0,
  36,28,21,15,10, 6, 3, 1,
  43,36,28,21,15,10, 6, 3,
  49,43,36,28,21,15,10, 6,
  54,49,43,36,28,21,15,10,
  58,54,49,43,36,28,21,15,
  61,58,54,49,43,36,28,21,
  63,61,58,54,49,43,36,28
};

/* Values of the pieces times 1 & 100 respectively */
int PieceValue[7]    = {0,1,5,3,3,9,1000};
int PieceValue10[7]    = {0,10,50,30,30,90,0};
int PieceValue100[7] = {0,0,0,0,0,0,0}; /* This one is filled later */

/* Bonus for advancing pawns to high ranks */
int RankBonus[8] = {0,32,16,8,4,2,0,0};
/* Modifier depending on file (/10) */
int FileMod[8] = {10,9,8,7,7,8,9,10};
/* Initial scores per rank for a passed pawn */
int PassedPawnI[8] = {0,64,50,24,16,12,8,0};
/* Score for a PP based on board square */
int PassedPawn[64];

/* Pieces to be promoted for promote flags 1-4 */
int PromotePiece[5] = {0,5,2,3,4};

/* Bonuses for connected passed pawns (per pawn)
 * Scaled by the danger presented by the opposition */
int ConnectedPP[8]  = {0,20,16,12,10,8,6,0};
int ConnectedPP2[8] = {0,38,24,18,12,10,8,0};
int ConnectedPP3[8] = {0,46,32,24,14,12,10,0};

/* Bonus for a pawn pair (by rank of the backmost pawn) */
int PawnPair[8] = {0,25,15,8,4,2,0,0};

/* Bonus/Penalty for having a rook, depending on the number of pawns around */
int RookPawnPenalty[17] = {21,18,15,12,9,6,3,0,-2,-4,-6,-8,-10,-12,-14,-16,-18};

/* Offside Majority Penalty
 * Scaled by the danger presented by the opposition */
int OffsidePenalty[9] = {0,10,22,30,38,53,70,80,90};
int OffsidePenalty2[9] = {0,14,26,34,46,60,75,85,95};
int OffsidePenalty3[9] = {0,18,30,40,54,67,80,90,100};
//int OffsidePenalty3[9] = {0,22,36,46,60,70,80,90,100};

/* Penalty for doubled pawns based on the number of pawns on the board.
 * The first two values are, of course, irrelevant! */
int DoubledPawns[17] = {0,0,20,19,18,17,16,15,14,13,12,11,10,10,10,10,10};

/* Convert from 8*8 position to 12*12 position for attack boards */
int ConvertSq[64] = {
  26, 27, 28, 29, 30, 31, 32, 33,
  38, 39, 40, 41, 42, 43, 44, 45,
  50, 51, 52, 53, 54, 55, 56, 57,
  62, 63, 64, 65, 66, 67, 68, 69,
  74, 75, 76, 77, 78, 79, 80, 81,
  86, 87, 88, 89, 90, 91, 92, 93,
  98, 99,100,101,102,103,104,105,
 110,111,112,113,114,115,116,117
};

/* Corner proximity bonus for kings in the opening and middle game */
int CornerBoard[64] = {
   5,  6, -2, -6, -6, -2,  6,  5,
  -8,-10,-14,-16,-16,-14,-10, -8,
 -10,-15,-18,-20,-20,-18,-15,-10,
 -14,-20,-25,-30,-30,-25,-20,-14,
 -14,-20,-25,-30,-30,-25,-20,-14,
 -10,-15,-18,-20,-20,-18,-15,-10,
  -8,-10,-14,-16,-16,-14,-10, -8,
   5,  6, -2, -6, -6, -2,  6,  5};

/* Pawn position depending on Gamestage */
int PawnPosition[6][64];
/* Undefended pawn penalty depending on Gamestage and position */
int PawnPositionPenalty[6][64];
/* Initial pawn position table - to be modified by Gamestage and 
 * rewritten into PawnPosition[6][64] */
int PawnPositionI[64] = { 
   0,  0,  0,  0,  0,  0,  0,  0,  /* 8 */
  24, 28, 32, 35, 35, 32, 28, 24,  /* 7 */
   5, 10, 16, 25, 25, 16, 10,  5,  /* 6 */
   4,  8, 14, 19, 19, 14,  8,  4,  /* 5 */
   3,  5,  6, 12, 12,  6,  5,  3,  /* 4 */
   2,  4,  3,  7,  7,  3,  4,  2,  /* 3 */
   1,  2,  3, -8, -9,  3,  2,  1,  /* 2 */
   0,  0,  0,  0,  0,  0,  0,  0}; /* 1 */
/* A   B   C   D   E   F   G   H  */

/* Rook positional values */
int RookPosition[64] = {
 0,  1,  2,  4,  4,  2,  1,  0,
 3,  5,  7,  7,  7,  7,  5,  3,
 3,  4,  5,  6,  6,  5,  4,  3,
-2,  0,  4,  5,  5,  4,  0, -2,
-4, -2,  3,  4,  4,  3, -2, -4,
-6, -2,  3,  4,  4,  3, -2, -6,
-6, -2,  4,  4,  4,  4, -2, -6,
-4, -2,  2,  4,  4,  2, -2, -4};

/* Knight positional values */
int KnightPosition[64] = {
-20,-10, -7, -5, -5, -7,-10,-20,
-10, -5,  2,  3,  3,  2, -5,-10,
 -4,  5,  7,  7,  7,  7,  5, -4,
 -5,  6,  8, 10, 10,  8,  6, -5,
 -6,  3,  8, 10, 10,  8,  3, -6,
 -8,  0,  5,  8,  8,  5,  0, -8,
-10, -8,  0,  1,  1,  0, -8,-10,
-20,-10,-10, -8, -8,-10,-10,-20};

/* Bishop positional values */
int BishopPosition[64] = {
  2,  7,  7,  7,  7,  7,  7,  2,
  2,  4,  4,  4,  4,  4,  4,  2,
  4,  6,  7,  7,  7,  7,  6,  4,
  4,  6,  8,  8,  8,  8,  6,  4,
  0,  4,  6,  8,  8,  6,  4,  0,
  0,  4,  5,  6,  6,  5,  4,  0,
  0,  6,  5,  5,  5,  5,  6,  0,
 -8, -8, -6, -6, -6, -6, -8, -8};

/* Queen positional values */
int QueenPosition[64] = {
 0,  0,  0,  0,  0,  0,  0,  0,
 0,  2,  3,  4,  4,  3,  2,  0,
 0,  2,  4,  5,  5,  3,  2,  0,
 0,  2,  4,  8,  8,  4,  2,  0,
-1,  0,  3,  6,  6,  3,  0, -1,
-1,  0,  2,  3,  3,  2,  0, -1,
-2, -1,  0,  0,  0,  0, -1, -2,
-3, -2, -1,  0,  0, -1, -2, -3};

/* Value of knight outposts at various board locations */
int KnightOutpost[64] = {
0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 2, 4, 4, 2, 0, 0,
0, 2, 7,14,14, 7, 2, 0,
0, 0, 6, 9, 9, 6, 0, 0,
0, 0, 1, 5, 5, 1, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0};

/* Penalty for staying near edges if you've got a lone, unprotected king
 * and the opponent has rooks/queens in the endgame (mate threats) */
int EdgePenalty[64] = {
-30,-20,-20,-20,-20,-20,-20,-30,
-20,-10,-10,-10,-10,-10,-10,-20,
-20,-10, -5, -5, -5, -5,-10,-20,
-20,-10, -5,  0,  0, -5,-10,-20,
-20,-10, -5,  0,  0, -5,-10,-20,
-20,-10, -5, -5, -5, -5,-10,-20,
-20,-10,-10,-10,-10,-10,-10,-20,
-30,-20,-20,-20,-20,-20,-20,-30};

#endif /* DEFS_H */
