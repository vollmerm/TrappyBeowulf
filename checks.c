/**********************************
 *    checks.c                    *
 *    Colin Frayn                 *
 *    March 2001                  *
 **********************************/

/*
  This file contains all the functions for testing for checks,
  checkmates and threats.
*/

#include <stdlib.h>
#include <stdio.h>

#include "common.h"
#include "checks.h"
#include "board.h"
#include "moves.h"

extern BITBOARD PawnAttacksBlack[64],PawnAttacksWhite[64],KnightMoves[64],KingMoves[64];
extern BITBOARD DiagMaska1h8[64],DiagMaska8h1[64],Mask[64];
extern BITBOARD RankMask[8], FileMask[8],RookMask[64],BishopMask[64],QueenMask[64];
extern BITBOARD MovesRank[64][256],MovesFile[64][256];
extern int DiagShifts_a1h8[64],DiagShifts_a8h1[64];
extern int DiagonalMask_a1h8[64],DiagonalMask_a8h1[64];
extern BITBOARD Movesa1h8[64][256],Movesa8h1[64][256];

/* Tests to see if the side specified is in check.  */
int InCheck(const Board *B,const int side) {
  int kpos,kx,ky;
  BITBOARD rq,bq,r,b,q,mask,tsq;

   /* We don't have any precalculated attack information
    * so we'll have to do this the hard way */
 
   /* (1) Check for king attacks.  Note that if the white king
    * can attack the black one then vice versa too. */
  if (KingMoves[B->WhiteKing]&Mask[B->BlackKing]) return king;

   /* It's not a king attack so let's set up some more variables. */
  if (side==WHITE) kpos = B->WhiteKing;
  else             kpos = B->BlackKing;
  kx = File(kpos);
  ky = Rank(kpos);
   
   /* (2) Check for pawn and knight attacks */
  if (side==WHITE) {
    if (ky>1 && (PawnAttacksWhite[kpos] & B->BlackPawns)) return pawn;
    if (B->BlackKnights & KnightMoves[kpos]) return knight;
  }
  if (side==BLACK) {
    if (ky<6 && (PawnAttacksBlack[kpos] & B->WhitePawns)) return pawn;
    if (B->WhiteKnights & KnightMoves[kpos]) return knight;
  }

   /* (3) *sigh* We'll have to check for sliding pieces then.  This isn't actually
    * all that painful.  First of all set up bitboards holding the relevant enemy
    * piece locations. r=Rooks, b=Bishops, q=Queens */
  if (side==WHITE) {
    r = B->BlackRooks;
    q = B->BlackQueens;
    b = B->BlackBishops;
  }
  else {
    r = B->WhiteRooks;
    q = B->WhiteQueens;
    b = B->WhiteBishops;
  }
   /* Combined bitboard representing the locations of enemy rooks & queens */
  rq = r|q;
   
   /* (3a) Horizontal/Vertical sliders : Rooks and queens first */
   /* Solve this by inverting the problem - i.e. imagine the target king as
    * a rook/bishop/queen.  See where it can move, and then & these squares
    * with the board positions of the required piece.  If this is !=0 then
    * king is in check */
   
   /* Check for rooks/queens on this rank. */
  if (RankMask[ky] & rq) {
    mask = (B->All >> (ky*8)) & FullRank;
    tsq  = MovesRank[kpos][mask];
    if (tsq & r) return rook;
    if (tsq & q) return queen;
  }     
   /* Check for rooks/queens on this file */
  if (FileMask[kx] & rq) {
    mask = (B->R90 >> (kx*8)) & FullRank;
    tsq  = MovesFile[kpos][mask];
    if (tsq & r) return rook;
    if (tsq & q) return queen;
  }
     
   /* Combined bitboard representing the locations of enemy bishops & queens */
  bq = b|q;

   /* (3b) Diagonal sliders : Bishops and queens last */
   
   /* Bishop or Queen on a1h8 Diagonal */
  if (DiagMaska1h8[kpos] & bq) { 
    mask = ((B->R45 >> DiagShifts_a1h8[kpos]) & DiagonalMask_a1h8[kpos]);
    tsq  = Movesa1h8[kpos][mask];
    if (tsq & b) return bishop;
    if (tsq & q) return queen;						
  }
   /* Bishop or Queen on a8h1 Diagonal */
  if (DiagMaska8h1[kpos] & bq) {
    mask = ((B->L45 >> DiagShifts_a8h1[kpos]) & DiagonalMask_a8h1[kpos]);
    tsq  = Movesa8h1[kpos][mask];
    if (tsq & b) return bishop;
    if (tsq & q) return queen;						
  }
  return FALSE;
}

/* Tests to see if the side specified is in checkmate.
 * Assumes that it is in check.  'p' is either 0, or the ID
 * of (the/a) checking piece (if known) */
BOOL InCM(Board *B, const int p) {
  MOVE *lastmove,*m,move,movelist[MAX_MOVES];
  int pid,to,kpos,inchk,side = B->side;
  BITBOARD km;
  Undo U;

   /* Setup the king position */
  if (side==WHITE) kpos = B->WhiteKing;
  else             kpos = B->BlackKing;

   /* Check if king can move away to an empty square */
  km = KingMoves[kpos] & ~B->All;
  while (km) {
    to = FirstPiece(km);
    move = kpos + (to<<6);
    U=DoMove(B,move);
    inchk = InCheck(B,Opponent(B->side));
    UndoMove(B,move,U);
    if (!inchk) return FALSE;
    RemoveFirst(km);
  }

   /* See if we can capture the offending piece */
  lastmove = GenerateCaptures(B,side,movelist);
  m = movelist;
  while (m<lastmove) {
    U=DoMove(B,*m);
    inchk = InCheck(B,Opponent(B->side));
    UndoMove(B,*m,U);
    if (!inchk) return FALSE;
    m++;
  }
   
   /* If there is no defence so far and the piece is known to be a pawn,
    * king or knight then this is CM.  Yeah, I know it shouldn't ever be a king,
    * but these things might happen so it's best to be safe. Also, it might be a
    * double check with another piece also attacking.  Either way, it's CM. */
  pid = PType(p);

  if ((pid==knight) | (pid==pawn) | (pid==king)) return TRUE;

   /* Otherwise get all possible blocking moves */
  lastmove=GenerateBlockingMoves(B,side,kpos,movelist);
  m = movelist;
  while (m<lastmove) {
    U=DoMove(B,*m);
    inchk = InCheck(B,Opponent(B->side));
    UndoMove(B,*m,U);
    if (!inchk) return FALSE;
    m++;
  }

   /* No way out (worst case) so return CM */
  return TRUE;
}

/* Tests to see if a certain move gives check.  This is slightly faster than
 * searching for check generally and so saves a bit of time.  Tests for direct
 * checks first, and then revealed checks. */
int GivesCheck(const Board *B, const MOVE m) {
  int from = MFrom(m), to = MTo(m);
  int kpos=0,ky,kx,x,y,p=PType(B->pieces[to]);
  BITBOARD kingmask,mask,MF,b,q,r,rq,bq,tsq;

   /* Castling and EP-captures are a bit more tricky */
  if (IsCastle(m) || IsEP(m)) return InCheck(B,B->side);

   /* B->side is the side which *might* be in check (the other side just moved) */
  if (B->side==WHITE) kpos = B->WhiteKing;
  else                kpos = B->BlackKing;
  kingmask = Mask[kpos];

   /* Coordinates for the 'to' square */
  x = File(to); y = Rank(to);
   
  switch (p) {
   case (pawn): if (B->side==WHITE && (PawnAttacksBlack[to] & kingmask)) return pawn;
                if (B->side==BLACK && (PawnAttacksWhite[to] & kingmask)) return pawn;
                break;
   case (knight): if (KnightMoves[to] & kingmask) return knight; break;     
   case (rook): mask = (B->All >> (y*8)) & FullRank;
                if (MovesRank[to][mask] & kingmask) return rook;                
                mask = (B->R90 >> (x*8)) & FullRank;
                if (MovesFile[to][mask] & kingmask) return rook;                
                break;
   case (queen): mask = (B->All >> (y*8)) & FullRank;
                 if (MovesRank[to][mask] & kingmask) return queen;
                 mask = (B->R90 >> (x*8)) & FullRank;
                 if (MovesFile[to][mask] & kingmask) return queen;                
                 mask = ((B->R45 >> DiagShifts_a1h8[to]) & DiagonalMask_a1h8[to]);
                 if (Movesa1h8[to][mask] & kingmask) return queen;
                 mask = ((B->L45 >> DiagShifts_a8h1[to]) & DiagonalMask_a8h1[to]);
                 if (Movesa8h1[to][mask] & kingmask) return queen;
                 break;
   case (bishop): mask = ((B->R45 >> DiagShifts_a1h8[to]) & DiagonalMask_a1h8[to]);
                  if (Movesa1h8[to][mask] & kingmask) return bishop;
                  mask = ((B->L45 >> DiagShifts_a8h1[to]) & DiagonalMask_a8h1[to]);
                  if (Movesa8h1[to][mask] & kingmask) return bishop;
                  break;
  }

  MF = Mask[from];
   /* See if it's impossible to have caused a revealed check - if so then we can quit */
  if ((QueenMask[kpos]&MF) == 0) return 0;

   /* Get locations of enemy sliding pieces */
  if (B->side==WHITE) {
    r = B->BlackRooks;
    q = B->BlackQueens;
    b = B->BlackBishops;
  }
  else {
    r = B->WhiteRooks;
    q = B->WhiteQueens;
    b = B->WhiteBishops;
  }
   /* Combined bitboard representing the locations of enemy rooks & queens */
  rq = r|q;
   /* Coordinates for the 'from' square and king square */
  x = File(from); y = Rank(from);
  kx = File(kpos); ky = Rank(kpos);
      
   /* Test for revealed attacks by rooks/queens */
  if (RookMask[kpos] & MF) {
    if ((rq&RookMask[kpos]) == 0) return 0;
     /* Same Rank */
    if (ky==y) {
      if ((RankMask[y]&rq) == 0) return 0;
      mask = (B->All >> (ky*8)) & FullRank;
      tsq  = MovesRank[kpos][mask];
      if (tsq & r) return rook;
      if (tsq & q) return queen;       
      return 0;
    }
     /* Same File */
    mask = (B->R90 >> (kx*8)) & FullRank;
    tsq  = MovesFile[kpos][mask];
    if (tsq & r) return rook;
    if (tsq & q) return queen;     
    return 0;
  }
   
   /* Combined bitboard representing the locations of enemy bishops & queens */
  bq = b|q;
  if ((bq&BishopMask[kpos]) == 0) return 0;
   
   /* Test for revealed attacks by bishops/queens */
   /* Bishop or Queen on a1h8 Diagonal */
  if (DiagMaska1h8[kpos] & bq) {
    mask = ((B->R45 >> DiagShifts_a1h8[kpos]) & DiagonalMask_a1h8[kpos]);
    tsq  = Movesa1h8[kpos][mask];
    if (tsq & b) return bishop;
    if (tsq & q) return queen;
  }
   /* Bishop or Queen on a8h1 Diagonal */
  if (DiagMaska8h1[kpos] & bq) {
    mask = ((B->L45 >> DiagShifts_a8h1[kpos]) & DiagonalMask_a8h1[kpos]);
    tsq  = Movesa8h1[kpos][mask];
    if (tsq & b) return bishop;
    if (tsq & q) return queen;
  }
  return 0;  
}



