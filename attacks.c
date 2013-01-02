/**********************************
 *    attacks.c                   *
 *    Colin Frayn                 *
 *    March 2001                  *
 **********************************/

/*
  This file contains all the functions for generating
  attack boards and threat stats.
*/

#include <stdlib.h>
#include <stdio.h>
#include <memory.h>

#include "common.h"
#include "attacks.h"
#include "board.h"

//#define DEBUG_SEE
//#define DEBUG_ATTACKS

#ifdef DEBUG_SEE
#include "moves.h"
#endif

extern BITBOARD MovesRank[64][256],MovesFile[64][256],Movesa1h8[64][256],Movesa8h1[64][256];
extern BITBOARD KnightMoves[64],KingMoves[64],RookMask[64],BishopMask[64],QueenMask[64];
extern BITBOARD PawnAttacksBlack[64],PawnAttacksWhite[64];
extern BITBOARD DiagMaska1h8[64],DiagMaska8h1[64],RankMask[8],FileMask[8],Mask[64];
extern BITBOARD DirR[64],DirL[64],DirUR[64],DirUL[64],DirDL[64],DirDR[64];
extern BITBOARD FileUpMask[64],FileDownMask[64];
extern int DiagShifts_a1h8[64],DiagShifts_a8h1[64];
extern int DiagonalMask_a1h8[64],DiagonalMask_a8h1[64];
extern int PieceValue[7],ConvertSq[64];
int PawnCount;

/*  Calculates total defence values for b&w, ignoring pinned pieces
 *  This is realistic as pins are only temporary, but a positional
 *  (dis)advantage can last.  This method is also much faster :) This
 *  routine has been stolen in a familiar state from ColChess. */
void GenerateAttacks(Board *B) {
  int a,x,y,sq,sq2;
  BITBOARD bd = B->All;

   /* Clear the attack board */
  memset(B->BAttacks,0,144*4);
  memset(B->WAttacks,0,144*4);

   /* Count the pawns - used later on in tactics.c */
  PawnCount = Count(B->BlackPawns|B->WhitePawns);

   /* Loop through all pieces on the board */
  while (bd) {
     /* Get the location of the first piece */
    sq = FirstPiece(bd);
    sq2 = ConvertSq[sq];

     /* Let's see what this piece is, and alter the attack squares accordingly */
    switch(B->pieces[sq]) {
      case wpawn:
       B->WAttacks[sq2-13]++;
       B->WAttacks[sq2-11]++;
       break;
      case wrook: 
       x = File(sq);
       y = Rank(sq);
       for (a=1;x+a<8;a++) {B->WAttacks[sq2+a]++; if (B->pieces[sq+a]) break;}
       for (a=1;x-a>=0;a++) {B->WAttacks[sq2-a]++; if (B->pieces[sq-a]) break;}
       for (a=1;y+a<8;a++) {B->WAttacks[sq2+a*12]++; if (B->pieces[sq+a*8]) break;}
       for (a=1;y-a>=0;a++) {B->WAttacks[sq2-a*12]++; if (B->pieces[sq-a*8]) break;}
       break;
      case wknight:
       B->WAttacks[sq2-23]++;
       B->WAttacks[sq2-10]++;
       B->WAttacks[sq2+14]++;
       B->WAttacks[sq2+25]++;
       B->WAttacks[sq2+23]++;
       B->WAttacks[sq2+10]++;
       B->WAttacks[sq2-14]++;
       B->WAttacks[sq2-25]++;
       break;
      case wbishop:
       for (a=sq-9;a>=0 && (a+1)&7;a-=9) {B->WAttacks[ConvertSq[a]]++; if (B->pieces[a]) break;}
       for (a=sq-7;a>=0 && a&7;a-=7) {B->WAttacks[ConvertSq[a]]++; if (B->pieces[a]) break;}
       for (a=sq+9;a<64 && a&7;a+=9) {B->WAttacks[ConvertSq[a]]++; if (B->pieces[a]) break;}
       for (a=sq+7;a<64 && (a+1)&7;a+=7) {B->WAttacks[ConvertSq[a]]++; if (B->pieces[a]) break;}
       break;
      case wqueen: 
       x = File(sq);
       y = Rank(sq);
       for (a=1;x+a<8;a++) {B->WAttacks[sq2+a]++; if (B->pieces[sq+a]) break;}
       for (a=1;x-a>=0;a++) {B->WAttacks[sq2-a]++; if (B->pieces[sq-a]) break;}
       for (a=1;y+a<8;a++) {B->WAttacks[sq2+a*12]++; if (B->pieces[sq+a*8]) break;}
       for (a=1;y-a>=0;a++) {B->WAttacks[sq2-a*12]++; if (B->pieces[sq-a*8]) break;}
       for (a=sq-9;a>=0 && (a+1)&7;a-=9) {B->WAttacks[ConvertSq[a]]++; if (B->pieces[a]) break;}
       for (a=sq-7;a>=0 && a&7;a-=7) {B->WAttacks[ConvertSq[a]]++; if (B->pieces[a]) break;}
       for (a=sq+9;a<64 && a&7;a+=9) {B->WAttacks[ConvertSq[a]]++; if (B->pieces[a]) break;}
       for (a=sq+7;a<64 && (a+1)&7;a+=7) {B->WAttacks[ConvertSq[a]]++; if (B->pieces[a]) break;}
       break;
      case wking: 
       B->WAttacks[sq2-11]++;
       B->WAttacks[sq2+13]++;
       B->WAttacks[sq2-13]++;
       B->WAttacks[sq2+11]++;
       B->WAttacks[sq2+1]++;
       B->WAttacks[sq2-1]++;
       B->WAttacks[sq2-12]++;
       B->WAttacks[sq2+12]++;
       break;
      case bpawn:
       B->BAttacks[sq2+11]++;
       B->BAttacks[sq2+13]++;
       break;
      case brook: 
       x = File(sq);
       y = Rank(sq);
       for (a=1;x+a<8;a++) {B->BAttacks[sq2+a]++; if (B->pieces[sq+a]) break;}
       for (a=1;x-a>=0;a++) {B->BAttacks[sq2-a]++; if (B->pieces[sq-a]) break;}
       for (a=1;y+a<8;a++) {B->BAttacks[sq2+a*12]++; if (B->pieces[sq+a*8]) break;}
       for (a=1;y-a>=0;a++) {B->BAttacks[sq2-a*12]++; if (B->pieces[sq-a*8]) break;}
       break;
      case bknight:
       B->BAttacks[sq2-23]++;
       B->BAttacks[sq2-10]++;
       B->BAttacks[sq2+14]++;
       B->BAttacks[sq2+25]++;
       B->BAttacks[sq2+23]++;
       B->BAttacks[sq2+10]++;
       B->BAttacks[sq2-14]++;
       B->BAttacks[sq2-25]++;
       break;
      case bbishop:
       for (a=sq-9;a>=0 && (a+1)&7;a-=9) {B->BAttacks[ConvertSq[a]]++; if (B->pieces[a]) break;}
       for (a=sq-7;a>=0 && a&7;a-=7) {B->BAttacks[ConvertSq[a]]++; if (B->pieces[a]) break;}
       for (a=sq+9;a<64 && a&7;a+=9) {B->BAttacks[ConvertSq[a]]++; if (B->pieces[a]) break;}
       for (a=sq+7;a<64 && (a+1)&7;a+=7) {B->BAttacks[ConvertSq[a]]++; if (B->pieces[a]) break;}
       break;
      case bqueen: 
       x = File(sq);
       y = Rank(sq);
       for (a=1;x+a<8;a++) {B->BAttacks[sq2+a]++; if (B->pieces[sq+a]) break;}
       for (a=1;x-a>=0;a++) {B->BAttacks[sq2-a]++; if (B->pieces[sq-a]) break;}
       for (a=1;y+a<8;a++) {B->BAttacks[sq2+a*12]++; if (B->pieces[sq+a*8]) break;}
       for (a=1;y-a>=0;a++) {B->BAttacks[sq2-a*12]++; if (B->pieces[sq-a*8]) break;}
       for (a=sq-9;a>=0 && (a+1)&7;a-=9) {B->BAttacks[ConvertSq[a]]++; if (B->pieces[a]) break;}
       for (a=sq-7;a>=0 && a&7;a-=7) {B->BAttacks[ConvertSq[a]]++; if (B->pieces[a]) break;}
       for (a=sq+9;a<64 && a&7;a+=9) {B->BAttacks[ConvertSq[a]]++; if (B->pieces[a]) break;}
       for (a=sq+7;a<64 && (a+1)&7;a+=7) {B->BAttacks[ConvertSq[a]]++; if (B->pieces[a]) break;}
       break;
      case bking: 
       B->BAttacks[sq2-11]++;
       B->BAttacks[sq2+13]++;
       B->BAttacks[sq2-13]++;
       B->BAttacks[sq2+11]++;
       B->BAttacks[sq2+1]++;
       B->BAttacks[sq2-1]++;
       B->BAttacks[sq2-12]++;
       B->BAttacks[sq2+12]++;
       break;
    }
    RemoveFirst(bd);
  }
#ifdef DEBUG_ATTACKS
  for (y=0;y<8;y++) {
    for (x=0;x<8;x++) fprintf(stdout,"%d ",B->WAttacks[ConvertSq[y*8+x]]);
    fprintf(stdout,"\n");
  }
  fprintf(stdout,"\n");
  for (y=0;y<8;y++) {
    for (x=0;x<8;x++) fprintf(stdout,"%d ",B->BAttacks[ConvertSq[y*8 +x]]);
    fprintf(stdout,"\n");
  }
#endif
}

/* Generate a list of all pieces attacking this square.  Include friendly pieces. */
BITBOARD AttacksTo(const Board *B, const int sq) {
  BITBOARD temp=0,rq,bq,mask;
  int x=File(sq),y=Rank(sq);

   /* First the easy pieces */
  temp = (B->BlackPawns & PawnAttacksWhite[sq]);
  temp |= (B->WhitePawns & PawnAttacksBlack[sq]);
  temp |= ((B->BlackKnights | B->WhiteKnights) & KnightMoves[sq]);
  temp |= (Mask[B->BlackKing] | Mask[B->WhiteKing]) & KingMoves[sq];

   /* Now the sliding pieces.  Much more difficult */
  rq = (B->BlackRooks | B->BlackQueens | B->WhiteRooks | B->WhiteQueens); 
  bq = (B->BlackBishops | B->BlackQueens | B->WhiteBishops | B->WhiteQueens); 
  
   /* Check for rooks and queens on this rank */
  if (RankMask[y]&rq) {
    mask = (B->All >> (y*8)) & FullRank;
    temp |= (rq & MovesRank[sq][mask]);
  }
   /* Check for rooks and queens on this file */
  if (FileMask[x]&rq) {
    mask = (B->R90 >> (x*8)) & FullRank;
    temp |= (rq & MovesFile[sq][mask]);
  }   
   /* Check for Bishops and Queens on the a1h8-sense diagonal */
  if (DiagMaska1h8[sq]&bq) {
    mask = ((B->R45 >> DiagShifts_a1h8[sq]) & DiagonalMask_a1h8[sq]);
    temp |= (bq & Movesa1h8[sq][mask]);						 
  }
   /* Check for Bishops and Queens on the a8h1-sense diagonal */
  if (DiagMaska8h1[sq]&bq) {
    mask = ((B->L45 >> DiagShifts_a8h1[sq]) & DiagonalMask_a8h1[sq]);
    temp |= (bq & Movesa8h1[sq][mask]);						 
  }
      
  return temp;
}

/* Check the outcome of exchanges on the square specified.  Algorithm borrowed
 * from Crafty in its simplest form.  There doesn't really seem to be many other
 * ways of doing this to be honest.  Note that this estimation does _not_
 * take into account checks.  Some of the captures might in fact be illegal.
 * This is known as a Static Exchange Evaluation (SEE). */
int SEE(const Board *B, const int from, const int to, const int promote) {
  BITBOARD attacks,tmp;
  int p = PieceValue[(PType(B->pieces[to]))],captured[32];
  int sq,side=WHITE,ncapt=1,sign=-1;

   /* Get the side to move.  This should usually be the B->side, but not always */
  if (B->pieces[from]<0) side = BLACK;
   /* Generate a list of pieces which attack the square 'to'.  Include
    * both friendly and hostile pieces. */
  attacks = AttacksTo(B,to);

#ifdef DEBUG_SEE
  PrintBoard(*B);
  PrintBitboard(attacks);
  fprintf(stdout,"Move Is : ");
  PrintMove((MOVE)(from+(to<<6)),TRUE,stdout);
  fprintf(stdout,"\n");
#endif

   /* Perform the initiating capture */
  captured[0] = p;
   /* Set up score for next piece.  Note that we don't test for check
    * so we have to stop kings moving into check by making them absurdly valuable.
    * King score = 1000.  That should do. */
  p = PieceValue[(PType(B->pieces[from]))];
   /* Test first to see if the attacker is actually counted in the attacks bitboard.
    * This is needed so that we can use the SEE for promotion moves when the 
    * first move is pawn forward 1 square, therefore it wouldn't be counted as an
    * attacker.  If the attacker is counted, then remove it from the attacks list. */ 
  if (attacks&Mask[from]) Remove(attacks,from);
  
   /* Deal with promoting pawns */
  if (promote) p = PieceValue[promote];

  if (QueenMask[to] & Mask[from]) attacks = ReevaluateAttacks(B,attacks,from,to);
  side = Opponent(side);

  
  while (attacks) {
     /* Loop through attackers in approximate order of increasing value */
    if (side==WHITE) {
      if ((tmp = B->WhitePawns & attacks)) sq=FirstPiece(tmp);
      else if ((tmp = B->WhiteKnights & attacks)) sq=FirstPiece(tmp);
      else if ((tmp = B->WhiteBishops & attacks)) sq=FirstPiece(tmp);
      else if ((tmp = B->WhiteRooks & attacks)) sq=FirstPiece(tmp);
      else if ((tmp = B->WhiteQueens & attacks)) sq=FirstPiece(tmp);
      else if (Mask[B->WhiteKing] & attacks) sq=B->WhiteKing;
      else break;       
    }
    else {
      if ((tmp = B->BlackPawns & attacks)) sq=FirstPiece(tmp);
      else if ((tmp = B->BlackKnights & attacks)) sq=FirstPiece(tmp);
      else if ((tmp = B->BlackBishops & attacks)) sq=FirstPiece(tmp);
      else if ((tmp = B->BlackRooks & attacks)) sq=FirstPiece(tmp);
      else if ((tmp = B->BlackQueens & attacks)) sq=FirstPiece(tmp);
      else if (Mask[B->BlackKing] & attacks) sq=B->BlackKing;
      else break;       
    }

     /* We have now got the location of the least valuable attacker of 'to' for
      * the side to move.  Remove it from the attacks BITBOARD and add the value
      * of the current occupant of 'to' onto the 'captured' list.  This list
      * stores the material value balance so far. */
    captured[ncapt] = captured[ncapt-1] + (p*sign);
     /* Set up score for next piece.  Note that we don't test for check
      * so we have to stop kings moving into check by making them absurdly valuable.
      * King score = 1000.  That should do. */
    p = PieceValue[(PType(B->pieces[sq]))];
    Remove(attacks,sq);
    if (QueenMask[to] & Mask[sq]) attacks = ReevaluateAttacks(B,attacks,sq,to);
    side = Opponent(side);
    sign =- sign;
    ncapt++;
#ifdef DEBUG_SEE
fprintf(stdout,"From %d  P=%d {CAPT=%d}\n",sq,p,captured[ncapt-1]);
#endif
  }
   /* Now we simply determine which capture moves will be actually played.
    * if any move results in an overall worsening of the position for that
    * side then it is not played.  We have to start from the back and work
    * forward, only retaining the current score if it improves the score for
    * the side to move. */
  if (ncapt&1) sign=1;
  else sign=-1;
   /* Loop through moves, starting at the end */
  while (--ncapt) {
     /* If Bob Hyatt ever reads this then perhaps he can explain to me why
      * he added in '<=' instead of '<' here. */
    if (sign==-1 && captured[ncapt] < captured[ncapt-1]) captured[ncapt-1]=captured[ncapt];
    else if (sign==1 && captured[ncapt] > captured[ncapt-1]) captured[ncapt-1]=captured[ncapt];
    sign=-sign;
  }
#ifdef DEBUG_SEE
fprintf(stdout,"Exchange = %d\n",captured[0]);
#endif
   /* Return the expectation value for the capture sequence */
  if (promote) return captured[0] + PieceValue[promote] - PieceValue[pawn];
  return captured[0];
}

/* Calculate if moving this piece (on 'from') revealed another
 * attacker (of 'to') behind it.  It is guaranteed to lie on either
 * the same rank/file or the same diagonal as the target piece, as I have
 * removed all knight moves */
BITBOARD ReevaluateAttacks(const Board *B, BITBOARD attacks, const int from, const int to) {
  int fx=File(from),fy=Rank(from),tx=File(to),ty=Rank(to);
  int dx,dy,a,p;

   /* Check if we have a new Xray threat on the same rank */
  if (ty == fy) {
     /* Attack from the right */
    if (tx<fx) {
      for (a=from+1;File(a)>fx;a++) {
        p = PType(B->pieces[a]);
        if (p == rook || p == queen) return (attacks | Mask[a]);
        if (p) return attacks;
      }
    }
     /* Attack from the left */
    if (tx>fx) {
      for (a=from-1;File(a)<fx;a--) {
        p = PType(B->pieces[a]);
        if (p == rook || p == queen) return (attacks | Mask[a]);
        if (p) return attacks;
      }
    }
    return attacks;
  }

   /* Check if we have a new Xray threat on the same file */
  if (fx == tx) {
     /* Attack from above */
    if (fy<ty) {
      for (a=from-8;a>=0;a-=8) {
        p = PType(B->pieces[a]);
        if (p == rook || p == queen) return (attacks | Mask[a]);
        if (p) return attacks;
      }
    }
     /* Attack from below */
    if (fy>ty) {
      for (a=from+8;a<64;a+=8) {
        p = PType(B->pieces[a]);
        if (p == rook || p == queen) return (attacks | Mask[a]);
        if (p) return attacks;
      }
    }
    return attacks;
  }

   /* Check if we have a new Xray threat on the same diagonal */
  dx = tx-fx; dy = ty-fy;
  if (abs(dx) == abs(dy)) {
     /* Up left */
    if (dx>0 && dy>0) {
      for (a=from-9;a>=0 && File(a)<fx;a-=9) {
        p = PType(B->pieces[a]);
        if (p == bishop || p == queen) return (attacks | Mask[a]);
        if (p) return attacks;
      }      
    }
     /* Down left */
    if (dx>0 && dy<0) {
      for (a=from+7;a<64 && File(a)<fx;a+=7) {
        p = PType(B->pieces[a]);
        if (p == bishop || p == queen) return (attacks | Mask[a]);
        if (p) return attacks;
      }      
    }
     /* Up right */
    if (dx<0 && dy>0) {
      for (a=from-7;a>=0 && File(a)>fx;a-=7) {
        p = PType(B->pieces[a]);
        if (p == bishop || p == queen) return (attacks | Mask[a]);
        if (p) return attacks;
      }      
    }
     /* Down right */
    if (dx<0 && dy<0) {
      for (a=from+9;a<64 && File(a)>fx;a+=9) {
        p = PType(B->pieces[a]);
        if (p == bishop || p == queen) return (attacks | Mask[a]);
        if (p) return attacks;
      }      
    }    
    return attacks;
  }
     
   /* No modification */      
  return attacks;
}


/* Evaluate the defence of a given square based on the value of
 * the weakest attacker / defender */
int EvaluateOwnership(Board *B, int sq) {
  BITBOARD attacks;

   /* Generate a list of pieces which attack the square 'to'.  Include
    * both friendly and hostile pieces. */
  attacks = AttacksTo(B,sq);
  
  /* Check what the weakest piece is */
  // It's a pawn
  if (attacks&(B->WhitePawns | B->BlackPawns)) {
  	return Count(attacks & B->WhitePawns) - Count(attacks & B->BlackPawns);
  }

  // It's a minor piece
  if (attacks&(B->WhiteBishops | B->WhiteKnights | B->BlackBishops | B->BlackKnights)) {
  	return Count(attacks & (B->WhiteBishops | B->WhiteKnights)) - Count(attacks & (B->BlackBishops | B->BlackKnights));
  }

  // It's a rook
  if (attacks&(B->WhiteRooks | B->BlackRooks)) {
  	return Count(attacks & B->WhiteRooks) - Count(attacks & B->BlackRooks);
  }

  // It's a queen
  if (attacks&(B->WhiteQueens | B->BlackQueens)) {
  	return Count(attacks & B->WhiteQueens) - Count(attacks & B->BlackQueens);
  }

  return NEUTRAL;
}
