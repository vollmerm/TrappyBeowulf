/**********************************
 *    board.c                     *
 *    Colin Frayn                 *
 *    March 2001                  *
 **********************************/

/*
  This file contains all the algorithms for messing
  about with bitboards.
*/

#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#include <string.h>

#include "common.h"
#include "board.h"
#include "utils.h"

extern int RotateR90[64],RotateL90[64];
extern int RotateR45[64],RotateL45[64],InvRotateR45[64],InvRotateL45[64];
extern BITBOARD Mask[64],FPMask1,FPMask2,RankMask[8];
extern int PieceValue[7],mvno;
extern MOVE MoveHistory[1000];
extern Undo UndoHistory[1000];
extern BOOL XBoard;

 /* Return a bitboard rotated through 90 degrees anti-clockwise */
BITBOARD RotateBoard_R90(const BITBOARD B) {
  BITBOARD R=0,b=UNIT,i;
  for (i=0;i<64;i++) {
    if (B & Mask[(RotateR90[i])]) R += (b<<i); 
  }
  return R;
}

 /* Return a bitboard rotated through 90 degrees clockwise */
BITBOARD RotateBoard_L90(const BITBOARD B) {
  BITBOARD R=0,b=UNIT,i;
  for (i=0;i<64;i++) {
    if (B & Mask[(RotateL90[i])]) R += (b<<i); 
  }
  return R;
}

 /* Return a bitboard in a1h8 diagonal form */
BITBOARD RotateBoard_R45(const BITBOARD B) {
  BITBOARD R=0,b=UNIT,i;
  for (i=0;i<64;i++) {
    if (B & Mask[(RotateR45[i])]) R += (b<<i); 
  }
  return R;
}

 /* Return a bitboard in a8h1 diagonal form */
BITBOARD RotateBoard_L45(const BITBOARD B) {
  BITBOARD R=0,b=UNIT,i;
  for (i=0;i<64;i++) {
    if (B & Mask[(RotateL45[i])]) R += (b<<i); 
  }
  return R;
}

 /* Transform back a bitboard from a1h8 diagonal form */
BITBOARD InvRotateBoard_R45(const BITBOARD B) {
  BITBOARD R=0,b=UNIT,i;
  for (i=0;i<64;i++) {
    if (B & Mask[(InvRotateR45[i])]) R += (b<<i); 
  }
  return R;
}

 /* Transform back a bitboard from a8h1 diagonal form */
BITBOARD InvRotateBoard_L45(const BITBOARD B) {
  BITBOARD R=0,b=UNIT,i;
  for (i=0;i<64;i++) {
    if (B & Mask[(InvRotateL45[i])]) R += (b<<i); 
  }
  return R;
}

/* A list of the number of bits in numbers from 0-255.  This is used in the
  * bit counting algorithm.  Thanks to Dann Corbit for this one. */
static int inbits[256] = {
  0, 1, 1, 2, 1, 2, 2, 3, 1, 2, 2, 3, 2, 3, 3, 4,
  1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5,
  1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5,
  2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
  1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5,
  2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
  2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
  3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7,
  1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5,
  2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
  2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
  3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7,
  2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
  3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7,
  3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7,
  4, 5, 5, 6, 5, 6, 6, 7, 5, 6, 6, 7, 6, 7, 7, 8,
};

/* This algorithm thanks to Dann Corbit.  Apparently it's faster than
 * the standard one.  */
int Count(const BITBOARD B) {
  return inbits[(unsigned char) B] +
    inbits[(unsigned char) (B >> 8)] +
    inbits[(unsigned char) (B >> 16)] +
    inbits[(unsigned char) (B >> 24)] +
    inbits[(unsigned char) (B >> 32)] +
    inbits[(unsigned char) (B >> 40)] +
    inbits[(unsigned char) (B >> 48)] +
    inbits[(unsigned char) (B >> 56)];
}

static const int lsz64_tbl[64] = {
     0, 31,  4, 33, 60, 15, 12, 34,
    61, 25, 51, 10, 56, 20, 22, 35,
    62, 30,  3, 54, 52, 24, 42, 19,
    57, 29,  2, 44, 47, 28,  1, 36,
    63, 32, 59,  5,  6, 50, 55,  7,
    16, 53, 13, 41,  8, 43, 46, 17,
    26, 58, 49, 14, 11, 40,  9, 45,
    21, 48, 39, 23, 18, 38, 37, 27
};

//______________________________________________________________________________
/* FirstPiece():
  *
  *      Return square number (0 to 63) of the least significant set bit
  *      in bitboard 'bb'
  *
  *      source: Matt Taylor's "de Bruijn method" implementation
  *      Probably best if you don't even TRY to understand this one. I certainly don't...
  */
//______________________________________________________________________________
int FirstPiece(const BITBOARD bb) {
    const BITBOARD lsb = (bb & -bb) - 1;
    const unsigned int foldedLSB = ((unsigned int) lsb) ^ ((unsigned int)(lsb >> 32));
    return lsz64_tbl[foldedLSB * 0x78291ACF >> 26];
}

/* Display the Given Board Position */
void PrintBoard(const Board B) {
  int x,y;
   
  fprintf(stdout,"\n");
   
  for (y=0;y<8;y++) {
    for (x=0;x<8;x++) {
      PrintPiece(B.pieces[(y*8)+x]);
      fprintf(stdout," ");
    }
    fprintf(stdout,"\n");
  }
  fprintf(stdout,"\n");
}

/* Display a Bitboard schematically */
void PrintBitboard(const BITBOARD B) {
  BITBOARD b=UNIT;
  int sq;
  for (sq=0;sq<64;sq++) {
    if (B&(b<<sq)) fprintf(stdout,"X");
    else fprintf(stdout,".");
    if (sq%8 == 7) fprintf(stdout,"\n");
  }
  fprintf(stdout,"\n");
}

/* Set the board specified to the FEN position given */
void SetBoard(Board *B,char *fen,BOOL ResetMovelist) {
  int a=0,x=0,y=0,sq=0,p,backup[64],castlebackup,epbackup,sidebackup;
  char ch;
  BOOL error = FALSE;

  for (sq=0;sq<64;sq++) {backup[sq] = B->pieces[sq]; B->pieces[sq]=0;}

   /* Read in the board layout */
  for (y=0;y<8;y++) {
    x=0;
    do {
      if (x>7) {
        fprintf(stderr,"BAD FEN - too many files!\n");
        for (sq=0;sq<64;sq++) B->pieces[sq] = backup[sq];
        return;
      }
      ch=fen[a++];
      p=0;
      if ((int)ch>60) {
        p = CharToPiece(ch);
        if (ch=='K') B->WhiteKing = (y*8)+x;
        if (ch=='k') B->BlackKing = (y*8)+x;
        B->pieces[(y*8)+x]=p;
        x++;
      }
      else x+=((int)ch-48);
    } while (fen[a]!='/' && fen[a]!=' ');
    if (y==7 && fen[a] != ' ') {
      fprintf(stderr,"BAD FEN - too many ranks!\n");
      for (sq=0;sq<64;sq++) B->pieces[sq] = backup[sq];
      return;
    }
    a++;
  }

   /* Get the side to move */
  ch = fen[a];
  sidebackup = B->side;
  if (toupper(ch)=='W') B->side=WHITE;
  else if (toupper(ch)=='B') B->side=BLACK;
  else {
    fprintf(stderr,"BAD FEN - strange side to move \"%c\"!\n",ch);
    for (sq=0;sq<64;sq++) B->pieces[sq] = backup[sq];
    return;
  }
  a+=2;

   /* Get the castling and EP values */
  castlebackup = B->castle;
  B->castle=0;
  epbackup = B->ep;
  B->ep=-1;
  if (a<(int)strlen(fen)) {
    do {
      switch(fen[a]) {
       case 'K': B->castle += 1; break;
       case 'Q': B->castle += 2; break;
       case 'k': B->castle += 4; break;
       case 'q': B->castle += 8; break;
       case '-': break;
       default : fprintf(stderr,"BAD FEN - strange castling permission \"%c\"!\n",fen[a]);
                 for (sq=0;sq<64;sq++) B->pieces[sq] = backup[sq];
                 B->side = sidebackup; B->castle = castlebackup; B->ep = epbackup;
                 return;
      }
    } while (fen[++a]!=' ' && a<(int)strlen(fen));
    
    if (a+1<(int)strlen(fen) && fen[++a]!='-') {
      B->ep  = (int)(fen[a]-'a');
      B->ep += (8 - (int)(fen[++a]-'0')) * 8;
    }
  }

  if (B->ep > -1) {
    if (fen[a-1]<'a' || fen[a-1]>'h') error = TRUE;
    if (B->side == BLACK && fen[a] != '3') error = TRUE;
    if (B->side == WHITE && fen[a] != '6') error = TRUE;
    if (B->side == BLACK && (B->pieces[B->ep-8] != wpawn || B->pieces[B->ep] != empty || B->pieces[B->ep+8] != empty)) error = TRUE;
    if (B->side == WHITE && (B->pieces[B->ep+8] != bpawn || B->pieces[B->ep] != empty || B->pieces[B->ep-8] != empty)) error = TRUE;
    if (error) {
      fprintf(stderr,"BAD FEN - strange EP square \"%c%c\"!\n",fen[a-1],fen[a]);
      for (sq=0;sq<64;sq++) B->pieces[sq] = backup[sq];
      B->side = sidebackup; B->castle = castlebackup; B->ep = epbackup;
      return;
    }
  }

  B->WPts = Pts(*B,WHITE);
  B->BPts = Pts(*B,BLACK);

  B->WhitePieces = B->BlackPieces = 0;
  B->WhitePawns = B->BlackPawns = 0;
  B->WhiteRooks = B->BlackRooks = 0;
  B->WhiteKnights = B->BlackKnights = 0;
  B->WhiteBishops = B->BlackBishops = 0;
  B->WhiteQueens = B->BlackQueens = 0;

  for (sq=0;sq<64;sq++) {
    p = B->pieces[sq];
    if (p==0) continue;
    switch(p) {
     case (wpawn)   : B->WhitePawns |= Mask[sq]; break;
     case (wrook)   : B->WhiteRooks |= Mask[sq]; break;
     case (wknight) : B->WhiteKnights |= Mask[sq]; break;
     case (wbishop) : B->WhiteBishops |= Mask[sq]; break;
     case (wqueen)  : B->WhiteQueens |= Mask[sq]; break;
     case (bpawn)   : B->BlackPawns |= Mask[sq]; break;
     case (brook)   : B->BlackRooks |= Mask[sq]; break;
     case (bknight) : B->BlackKnights |= Mask[sq]; break;
     case (bbishop) : B->BlackBishops |= Mask[sq]; break;
     case (bqueen)  : B->BlackQueens |= Mask[sq]; break;
     default        : break;
    }
    if (p>0) B->WhitePieces |= Mask[sq];
    if (p<0) B->BlackPieces |= Mask[sq];
  }

  B->All = B->WhitePieces | B->BlackPieces;
  B->R90 = RotateBoard_R90(B->All);
  B->R45 = RotateBoard_R45(B->All);
  B->L45 = RotateBoard_L45(B->All);

   /* Reset the move history */
  if (ResetMovelist) {
    mvno=0;
    for (a=0;a<1000;a++) MoveHistory[a]=NO_MOVE;
  }
}

/* Reset the board to the start position */
void ResetBoard(Board *B) {
  int a=0,sq=0;

  /* Reset a few variables */
  for (sq=a6;sq<=h3;sq++) B->pieces[sq]=0;
  B->castle=15;
  B->ep=-1;
  B->side=WHITE;  
  B->WPts = B->BPts = 39;

  // Set the board itself
  for (sq=a7;sq<=h7;sq++) B->pieces[sq] = bpawn;
  for (sq=a2;sq<=h2;sq++) B->pieces[sq] = wpawn;
  B->pieces[a8] = B->pieces[h8] = brook;
  B->pieces[a1] = B->pieces[h1] = wrook;
  B->pieces[b8] = B->pieces[g8] = bknight;
  B->pieces[b1] = B->pieces[g1] = wknight;
  B->pieces[c8] = B->pieces[f8] = bbishop;
  B->pieces[c1] = B->pieces[f1] = wbishop;
  B->pieces[d8] = bqueen;
  B->pieces[d1] = wqueen;
  B->pieces[e8] = bking;
  B->pieces[e1] = wking;

  // Set the bitboards
  B->WhitePawns = RankMask[Rank2];
  B->BlackPawns = RankMask[Rank7];
  B->WhiteRooks = Mask[a1] | Mask[h1];
  B->BlackRooks = Mask[a8] | Mask[h8];
  B->WhiteKnights = Mask[b1] | Mask[g1];
  B->BlackKnights = Mask[b8] | Mask[g8];
  B->WhiteBishops = Mask[c1] | Mask[f1];
  B->BlackBishops = Mask[c8] | Mask[f8];
  B->WhiteQueens = Mask[d1];
  B->BlackQueens = Mask[d8];
  B->WhiteKing = e1;
  B->BlackKing = e8;
  B->WhitePieces = RankMask[Rank1] | RankMask[Rank2];
  B->BlackPieces = RankMask[Rank7] | RankMask[Rank8];

  B->All = B->WhitePieces | B->BlackPieces;
  B->R90 = RotateBoard_R90(B->All);
  B->R45 = RotateBoard_R45(B->All);
  B->L45 = RotateBoard_L45(B->All);

   /* Reset the move history */
  mvno=0;
  for (a=0;a<1000;a++) MoveHistory[a]=NO_MOVE;
}

/* Return (slowly) the count of all points on the board.  Doesn't trust any
 * of the bitboards to be set up correctly */
int Pts(Board B,const int side) {
  int sq,pts=0,p;
  for (sq=0;sq<64;sq++) {
    p = B.pieces[sq];
    if ((side==WHITE && p<0) || (side==BLACK && p>0)) continue;
    if (PType(p)<king) pts += PieceValue[PType(p)];
  }
  return pts;
}

/* Print off the piece letter for the required piece */
void PrintPiece(int p) {
  switch (p) {
   case (empty)   : fprintf(stdout,"."); break;
   case (wpawn)   : fprintf(stdout,"P"); break;
   case (wrook)   : fprintf(stdout,"R"); break;
   case (wknight) : fprintf(stdout,"N"); break;
   case (wbishop) : fprintf(stdout,"B"); break;
   case (wqueen)  : fprintf(stdout,"Q"); break;
   case (wking)   : fprintf(stdout,"K"); break;
   case (bpawn)   : fprintf(stdout,"p"); break;
   case (brook)   : fprintf(stdout,"r"); break;
   case (bknight) : fprintf(stdout,"n"); break;
   case (bbishop) : fprintf(stdout,"b"); break;
   case (bqueen)  : fprintf(stdout,"q"); break;
   case (bking)   : fprintf(stdout,"k"); break;
   default        : break;
  }
}
