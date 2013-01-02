/**********************************
 *    tactics.c                   *
 *    Colin Frayn                 *
 *    March 2001                  *
 **********************************/

/*
  This file contains all the functions for evaluating
  the value of different pieces in different board states.
*/

#include <stdlib.h>
#include <stdio.h>

#include "common.h"
#include "tactics.h"
#include "params.h"
#include "attacks.h"
#include "board.h"
#include "moves.h"

#define USE_KING_ATTACK
#define BISHOP_MOBILITY
#define SIMPLE_PINS
#define TRAPPED_ROOK
#define TRAPPED_KNIGHT
#define DEFENDED_QUEEN_ATTACKS
//#define ADJACENT_PAWNS
//#define KING_BOXED_IN
//#define COUNT_CHECKS
//#define DEBUG_TACTICS


extern int DoubledPawns[17],ConvertSq[64],Flip[64],KnightOutpost[64],Distance[64][64];
extern int RankBonus[8],PassedPawn[64],EdgePenalty[64], RookPawnPenalty[17], KingSafetyPP[17][7];
extern int KingArea[64][25], SideAttack[6], PawnPair[8], PawnCount, RookTrapped[6];
extern int Backward[7][8], PieceValue10[7],PawnPosition[6][64],PawnPositionPenalty[6][64];
extern BITBOARD Mask[64],PawnAttacksWhite[64],PawnAttacksBlack[64],FileMask[8],RankMask[8],InvMask[64];
extern BITBOARD FileUpMask[64],FileDownMask[64],MovesRank[64][256], KnightMoves[64];
extern BITBOARD DiagMaska1h8[64],DiagMaska8h1[64], RookMask[64], BishopMask[64];
extern BITBOARD DoubleKnightMove[64], KingMoves[64], FrontMask[64], BackMask[64];
extern BITBOARD WhiteMask,BlackMask,PassedMask,HiddenMask,MovesFile[64][256];
extern BITBOARD AboveMask[8],BelowMask[8], KingMoves[64], PPKSMask, PairMask[64];
extern BITBOARD WhiteKingSide, WhiteQueenSide, BlackKingSide, BlackQueenSide;
extern BITBOARD LLCorner, LRCorner, ULCorner, URCorner, PairMaskUp[64], PairMaskDown[64];
extern BITBOARD RankLeftMask[64], RankRightMask[64], FlankMaskDown[64],FlankMaskUp[64];
extern BITBOARD SideMask[64],KingSafetyMask[64], PawnShieldMaskWhite[64], PawnShieldMaskBlack[64];
extern BITBOARD KnightAttackPromotionW[64], KnightAttackPromotionB[64];
#ifdef BISHOP_MOBILITY
extern int DiagShifts_a1h8[64],DiagShifts_a8h1[64];
extern int DiagonalMask_a1h8[64],DiagonalMask_a8h1[64];
extern BITBOARD Movesa1h8[64][256],Movesa8h1[64][256];
#endif

/* Evaluate the tactics for a pawn */
int TactPawn(const int sq,const Board *B,const int side) {
  int x=File(sq),def=0,sq2=ConvertSq[sq];
  int Doubled, score = 0, backward = 0, ktrop;
  BOOL iso=FALSE, pass=FALSE, block=FALSE;
#ifdef DEBUG_TACTICS
  int oldscore = 0;
#endif

  /* -- Test a white pawn -- */
  if (side==WHITE) {
    def = B->WAttacks[sq2] - B->BAttacks[sq2];
     /* Check if the pawn is isolated (no pawns either side) */
    if (!(Mask[sq]&PassedMask) &&
        (x==FileA || !(B->WhitePawns & FileMask[x-1])) &&
        (x==FileH || !(B->WhitePawns & FileMask[x+1]))) {
      iso=TRUE; score -= ISOLATED_PAWN;
#ifdef DEBUG_TACTICS
      fprintf(stdout,"Iso %d, ",-ISOLATED_PAWN);
      oldscore = score;
#endif
    }
     /* Reward advanced pawn pairs */
    else {
      if (PairMaskUp[sq]&B->WhitePawns) score += PawnPair[Rank(sq)];
#ifdef DEBUG_TACTICS
      if (score!=oldscore) fprintf(stdout,"Pair %d, ",score-oldscore);
      oldscore = score;
#endif
    }
    /* Penalise Doubled Pawns */
    if (B->WhitePawns & FileUpMask[sq]) {
      Doubled = DoubledPawns[PawnCount];
       /* Possibility of un-doubling.  Therefore not so bad ;) */
      if (x>0 && (FileUpMask[sq-1]&B->BlackPawns)) Doubled >>= 1;
      if (x<7 && (FileUpMask[sq+1]&B->BlackPawns)) Doubled >>= 1;
      score -= Doubled;
      /* Doubled isolated pawns are even worse */
      if (iso) score -= DOUBLED_PAWNS_ISO;
#ifdef DEBUG_TACTICS
      fprintf(stdout,"Doubled %d, ",-Doubled);
      oldscore = score;
#endif
       /* Doubled blocked pawns are worse still */
      if (B->BlackPawns & FileUpMask[sq]) score -= DOUBLED_PAWNS;
#ifdef DEBUG_TACTICS
      if (B->BlackPawns & FileUpMask[sq]) fprintf(stdout,"D&Block, ");
      oldscore = score;
#endif
    }
     /* Check for passed pawns */
    if (Mask[sq]&PassedMask) {
#ifdef DEBUG_TACTICS
      oldscore = score;
#endif
       /* Endgame King tropism */
      if (B->WAttacks[sq2]) ktrop=1;
      else ktrop = KING_TROPISM;
      if (B->Gamestage>=LateMid)
        score += (Distance[sq][B->BlackKing] - Distance[sq][B->WhiteKing]) * ktrop;
       /* Further bonus if there are no enemy pieces badly threatening the pawn */
      if (!(B->BlackQueens | B->BlackRooks))
        score += (Distance[sq][B->BlackKing] - Distance[sq][B->WhiteKing]) * ktrop;
       /* Penalise blockaded/attacked pawns */
      if (FileMask[x] & (B->BlackRooks | B->BlackQueens)) {score -= HOSTILE_BLOCKADE;block=TRUE;}
      else if (FileUpMask[sq] & B->BlackPieces) {score -= PIECE_BLOCKADE;block=TRUE;}

#ifdef DEBUG_TACTICS
      fprintf(stdout,"Passed %d, ",score-oldscore);
      oldscore = score;
#endif
      pass=TRUE;
    }
     /* Otherwise, penalise non-passed pawns for being undefended by pawns */
    else if (sq<a2 && def<2) {
      switch (def) {
        case 1 : if (!(PawnAttacksBlack[sq]&B->WhitePawns))
                   score -= PawnPositionPenalty[B->Gamestage][sq]; break;
        default: if (!(PawnAttacksBlack[sq]&B->WhitePawns))
                   score -= PawnPosition[B->Gamestage][sq]; break;
      }
#ifdef DEBUG_TACTICS
      if (score != oldscore) fprintf(stdout,"Undef %d, ",score-oldscore);
      oldscore = score;
#endif
    }
     /* Check for blocked pawns */
    if (B->pieces[sq-8]==bpawn ||
      Count(PawnAttacksWhite[sq-8]&B->BlackPawns) > Count(PawnAttacksBlack[sq-8]&B->WhitePawns)) {
      block = TRUE;
      score -= PIECE_BLOCKADE;
    }
  }

   /* -- Test a black pawn -- */
  else {
    def = B->BAttacks[sq2] - B->WAttacks[sq2];
     /* Check if the pawn is isolated (no pawns either side) */
    if (!(Mask[sq]&PassedMask) &&
        (x==FileA || !(B->BlackPawns & FileMask[x-1])) &&
        (x==FileH || !(B->BlackPawns & FileMask[x+1]))) {
      iso=TRUE;score -= ISOLATED_PAWN;
#ifdef DEBUG_TACTICS
      fprintf(stdout,"Iso %d, ",-ISOLATED_PAWN);
      oldscore = score;
#endif
    }
     /* Reward advanced pawn pairs */
    else {
      if (PairMaskDown[sq]&B->BlackPawns) score += PawnPair[7-Rank(sq)];
#ifdef DEBUG_TACTICS
      if (score!=oldscore) fprintf(stdout,"Pair %d, ",score-oldscore);
      oldscore = score;
#endif
    }
    /* Penalise Doubled Pawns */
    if (B->BlackPawns & FileDownMask[sq]) {
      Doubled = DoubledPawns[PawnCount];
       /* Possibility of un-doubling.  Therefore not so bad ;) */
      if (x>0 && (FileDownMask[sq-1]&B->WhitePawns)) Doubled >>= 1;
      if (x<7 && (FileDownMask[sq+1]&B->WhitePawns)) Doubled >>= 1;
      score -= Doubled;
      /* Doubled isolated pawns are even worse */
      if (iso) score -= DOUBLED_PAWNS_ISO;
#ifdef DEBUG_TACTICS
      fprintf(stdout,"Doubled %d, ",-Doubled);
      oldscore = score;
#endif
       /* Doubled blocked pawns */
      if (B->WhitePawns & FileDownMask[sq]) score -= DOUBLED_PAWNS;
#ifdef DEBUG_TACTICS
      if (B->WhitePawns & FileDownMask[sq]) fprintf(stdout,"D&Block, ");
      oldscore = score;
#endif
    }
     /* Check for passed pawns */
    if (Mask[sq]&PassedMask) {
#ifdef DEBUG_TACTICS
      oldscore = score;
#endif
       /* Endgame King tropism */
      if (B->BAttacks[sq2]) ktrop=1;
      else ktrop = KING_TROPISM;
      if (B->Gamestage>=LateMid)
        score += (Distance[sq][B->WhiteKing] - Distance[sq][B->BlackKing]) * ktrop;
       /* Further bonus if there are no enemy pieces badly threatening the pawn */
      if (!(B->WhiteQueens | B->WhiteRooks))
        score += (Distance[sq][B->WhiteKing] - Distance[sq][B->BlackKing]) * ktrop;
       /* Penalise blockaded/attacked pawns */
      if (FileMask[x] & (B->WhiteRooks | B->WhiteQueens)) {score -= HOSTILE_BLOCKADE;block=TRUE;}
      else if (FileDownMask[sq] & B->WhitePieces) {score -= PIECE_BLOCKADE;block=TRUE;}

#ifdef DEBUG_TACTICS
      fprintf(stdout,"Passed %d, ",score-oldscore);
      oldscore = score;
#endif
      pass=TRUE;
    }
     /* Otherwise, penalise non-passed pawns for being undefended by pawns */
    else if (sq>h7 && def<2) {
      switch (def) {
        case 1 : if (!(PawnAttacksWhite[sq]&B->BlackPawns))
                   score -= PawnPositionPenalty[B->Gamestage][Flip[sq]]; break;
        default: if (!(PawnAttacksWhite[sq]&B->BlackPawns))
                   score -= PawnPosition[B->Gamestage][Flip[sq]]; break;
      }
#ifdef DEBUG_TACTICS
      if (score != oldscore) fprintf(stdout,"Undef %d, ",score-oldscore);
      oldscore = score;
#endif
    }
     /* Check for blocked pawns */
    if (B->pieces[sq+8]==wpawn ||
      Count(PawnAttacksBlack[sq+8]&B->WhitePawns) > Count(PawnAttacksWhite[sq+8]&B->BlackPawns)) {
      block = TRUE;
      score -= PIECE_BLOCKADE;
    }
  }

   /* Check for backward pawns */
  if (!iso && !pass && (backward = BackwardsPawn(sq,B,side,block))) {
    score -= backward;
  }
#ifdef DEBUG_TACTICS
  if (def>2) fprintf(stdout,"DEF=%d(2), ",def);
  else fprintf(stdout,"DEF=%d, ",def);
  if (backward) fprintf(stdout,"Backwards %d, ",-backward);
  oldscore = score;
#endif  

   /* Defence score */
  score += DEFENDED_PAWN*min(def,2);

#ifdef DEBUG_TACTICS
  fprintf(stdout," [%ld]", score);
#endif
  return score;
}

/* Evaluate the tactics for a rook */
int TactRook(const int sq,const Board *B,const int side) {
  int efsq = sq,x=File(sq),y=Rank(sq),nm, score=0;
  BITBOARD mask,xmoves,ymoves;
#ifdef SIMPLE_PINS
  BITBOARD Moves;
#endif
#ifdef DEBUG_TACTICS
  int oldscore;
#endif  
#ifdef TRAPPED_ROOK
# ifndef SIMPLE_PINS
  BITBOARD Moves;
# endif
  BITBOARD AllMoves;
  int tsq;
#endif


  /* efsq holds the square which this piece is on (for white), or the effective
   * square when the board is flipped in y, (for black) */
  if (side==BLACK) efsq = Flip[sq];
  
  /* Bonus for 7th Rank Attacks */
  if (Rank(efsq)==Rank7 && B->Gamestage<LateEnd) score += ROOK_7TH_RANK;
#ifdef DEBUG_TACTICS
  if (Rank(efsq)==Rank7 && B->Gamestage<LateEnd) fprintf(stdout,"7th Rank (%d), ",ROOK_7TH_RANK);
  oldscore = score;
#endif
  
  /* Worth less if there are lots of pawns around */
  score += RookPawnPenalty[PawnCount];
#ifdef DEBUG_TACTICS
  if (oldscore != score) fprintf(stdout,"Restricted %d, ",score-oldscore);
  oldscore = score;
#endif

  /* Calculate Horizontal moves */
  mask = (B->All >> (y*8)) & FullRank;
  xmoves = MovesRank[sq][mask];
  /* Calculate vertical moves */
  mask = (B->R90 >> (x*8)) & FullRank;
  ymoves = MovesFile[sq][mask];

  /* Side specific tactics */
  if (side==WHITE) {
    /* Defending Passed Pawn */
    if ((B->WhitePawns & PassedMask) & FileUpMask[sq] & ymoves) {
      score += ROOK_BEHIND_PP;
#ifdef DEBUG_TACTICS
      fprintf(stdout,"DefPP %d, ",score-oldscore);
      oldscore = score;
#endif
    }
    /* Freefile Bonus */
    else if (!(B->WhitePawns & FileUpMask[sq])) {
      score += HALF_OPEN_FILE;
      if (!(FileMask[x] & B->BlackPieces)) score += OPEN_FILE;
      else if (!(FileMask[x] & (B->BlackQueens | B->BlackRooks))) score += HALF_OPEN_FILE;
#ifdef USE_KING_ATTACK
      if (x == File(B->BlackKing)) score += ROOK_ATTACK;
      if (abs(x - File(B->BlackKing)) < 2) score += ROOK_ATTACK;
#endif
#ifdef DEBUG_TACTICS
      fprintf(stdout,"OpenFile %d, ",score-oldscore);
      oldscore = score;
#endif
    }
     /* Rook blocked behind first rank pawn */
    else {
      if (y == Rank1 && B->pieces[sq-8]==wpawn) score -= ROOK_BLOCKED;
    }
#ifdef DEBUG_TACTICS
    if (oldscore != score) fprintf(stdout,"Blocked %d, ",score-oldscore);
    oldscore = score;
#endif
    /* Check if boxed in by white's own king */
    if (B->Gamestage<=LateMid && y==Rank1 && B->WhiteKing>=a1) {
      if ((x>FileF && B->WhiteKing>e1 && File(B->WhiteKing)<x) ||
          (x<FileC && B->WhiteKing<d1 && File(B->WhiteKing)>x)) {
        score -= ROOK_BOXED_IN;
        if (B->pieces[sq-8] != empty) score -= ROOK_BOXED_IN;
      }
    }
#ifdef DEBUG_TACTICS
    if (oldscore != score) fprintf(stdout,"KBlock %d, ",score-oldscore);
    oldscore = score;
#endif
  }
  /* Repeat for black */
  else {
    /* Defending Passed Pawn */
    if ((B->BlackPawns & PassedMask) & FileDownMask[sq] & ymoves) {
      score += ROOK_BEHIND_PP;
#ifdef DEBUG_TACTICS
      fprintf(stdout,"DefPP %d, ",score-oldscore);
      oldscore = score;
#endif
    }
    /* Freefile Bonus */
    else if (!(B->BlackPawns & FileDownMask[sq])) {
      score += HALF_OPEN_FILE;
      if (!(FileMask[x] & B->WhitePieces)) score += OPEN_FILE;
      else if (!(FileMask[x] & (B->WhiteQueens | B->WhiteRooks))) score += HALF_OPEN_FILE;
#ifdef USE_KING_ATTACK
      if (x == File(B->WhiteKing)) score += ROOK_ATTACK;
      if (abs(x - File(B->WhiteKing)) < 2) score += ROOK_ATTACK;
#endif
#ifdef DEBUG_TACTICS
      fprintf(stdout,"OpenFile %d, ",score-oldscore);
      oldscore = score;
#endif
    }
     /* Rook blocked behind first rank pawn */
    else {
      if (y == Rank8 && B->pieces[sq+8] == bpawn) score -= ROOK_BLOCKED;
    }
#ifdef DEBUG_TACTICS
    if (score != oldscore) fprintf(stdout,"Blocked %d, ",score-oldscore);
    oldscore = score;
#endif
    /* Check if boxed in by black's own king */
    if (B->Gamestage<=LateMid && y==Rank8 && B->BlackKing<=h8) { 
      if ((x>FileF && B->BlackKing>e8 && File(B->BlackKing)<x) ||
          (x<FileC && B->BlackKing<d8 && File(B->BlackKing)>x)) {
        score -= ROOK_BOXED_IN;
        if (B->pieces[sq+8] != empty) score -= ROOK_BOXED_IN;
      }
    }
#ifdef DEBUG_TACTICS
    if (score != oldscore) fprintf(stdout,"KBlock %d, ",score-oldscore);
    oldscore = score;
#endif
  }
  
   /* Reward Horizontal Freedom */
  nm = Count(xmoves & (InvMask[B->WhiteKing] & InvMask[B->BlackKing]));
  score += nm;
  /* Penalise if Rook is Blocked Horizontally */
  if (nm<3) score -= ROOK_BLOCKED;
#ifdef DEBUG_TACTICS
  if (nm<3) fprintf(stdout,"HBlock %d, ",nm);
  else fprintf(stdout,"Horiz. %d, ",nm);
  oldscore = score;
#endif

  /* Reward for connected Rooks */
  if (side==WHITE && (xmoves & B->WhiteRooks)) score += CONNECTED_ROOKS;
  if (side==BLACK && (xmoves & B->BlackRooks)) score += CONNECTED_ROOKS;
#ifdef DEBUG_TACTICS
  if (oldscore != score) fprintf(stdout,"Connected (%d), ",score-oldscore);
  oldscore = score;
#endif

  /* Reward for doubled Rooks (on one file) */
  if (side==WHITE && (ymoves & B->WhiteRooks)) {
    score += DOUBLED_ROOKS;
    // Reward more if file is empty of opposing pawns
    if (!(FileUpMask[sq] & B->BlackPawns)) score += DOUBLED_ROOKS;
  }
  if (side==BLACK && (ymoves & B->BlackRooks)) {
    score += DOUBLED_ROOKS;
    // Reward more if file is empty of opposing pawns
    if (!(FileDownMask[sq] & B->WhitePawns)) score += DOUBLED_ROOKS;
  }
#ifdef DEBUG_TACTICS
  if (oldscore != score) fprintf(stdout,"Doubled (%d), ",score-oldscore);
  oldscore = score;
#endif

  /* Test to see if it's pinned horiz or vert or diagonally */
#ifdef SIMPLE_PINS
  Moves = BishopMoves(B,sq);
  if (side==WHITE) {
    if ((xmoves&(Mask[B->WhiteKing]|B->WhiteQueens)) && (xmoves&B->BlackRooks)) score -= ROOK_PINNED;
    if ((ymoves&(Mask[B->WhiteKing]|B->WhiteQueens)) && (ymoves&B->BlackRooks)) score -= ROOK_PINNED;
    if ((Moves&DiagMaska1h8[sq]&(Mask[B->WhiteKing]|B->WhiteQueens)) && (Moves&DiagMaska1h8[sq]&(B->BlackBishops|B->BlackQueens))) score -= ROOK_PINNED;
    if ((Moves&DiagMaska8h1[sq]&(Mask[B->WhiteKing]|B->WhiteQueens)) && (Moves&DiagMaska8h1[sq]&(B->BlackBishops|B->BlackQueens))) score -= ROOK_PINNED;
  }
  else {
    if ((xmoves&(Mask[B->BlackKing]|B->BlackQueens)) && (xmoves&B->WhiteRooks)) score -= ROOK_PINNED;
    if ((ymoves&(Mask[B->BlackKing]|B->BlackQueens)) && (ymoves&B->WhiteRooks)) score -= ROOK_PINNED;
    if ((Moves&DiagMaska1h8[sq]&(Mask[B->BlackKing]|B->BlackQueens)) && (Moves&DiagMaska1h8[sq]&(B->WhiteBishops|B->WhiteQueens))) score -= ROOK_PINNED;
    if ((Moves&DiagMaska8h1[sq]&(Mask[B->BlackKing]|B->BlackQueens)) && (Moves&DiagMaska8h1[sq]&(B->WhiteBishops|B->WhiteQueens))) score -= ROOK_PINNED;
  }
# ifdef DEBUG_TACTICS
  if (oldscore != score) fprintf(stdout,"Pinned %d, ",score-oldscore);
  oldscore = score;
# endif
#endif

  /* Penalise if pawns can drive it away */
  score -= DriveAway(sq,B,side);
  
#ifdef DEBUG_TACTICS
  oldscore = score;
#endif
  /* Penalty for bringing rooks out too early */
  if (B->Gamestage == Opening && Rank(efsq)!=Rank1) {
    score -= (Rank1-Rank(efsq)) * EARLY_ROOK_PENALTY;
#ifdef DEBUG_TACTICS
    fprintf(stdout,"Early %d, ",-(Rank1-Rank(efsq)) * EARLY_ROOK_PENALTY);
    oldscore = score;
#endif
  }

#ifdef TRAPPED_ROOK
  // Complicated trap detection. Very slow.
  AllMoves = Moves = xmoves | ymoves;

  /* Parse through all squares to which we can move.  Look for a safe one. */
  if (side==WHITE) {
    // If there is a friendly piece (not a pawn) 
    // in the way then assume we're not trapped
    if (!(Moves&B->WhitePieces&(~(B->WhitePawns)))) { 
      Moves &= ~(B->WhitePieces);
      while (Moves) {
        tsq = FirstPiece(Moves);
        // See if this square is unattacked or suitably defended
        if (B->BAttacks[ConvertSq[tsq]]==0 || SEE(B,sq,tsq,0)>=0) {
          // See if this is not just a dead end
          if (RookMask[tsq]&KingMoves[tsq]&(~(B->WhitePawns))&(~AllMoves)&InvMask[sq]) break;
        }
        RemoveFirst(Moves);
      }
    }
  }
  else {
    // If there is a friendly piece (not a pawn) 
    // in the way then assume we're not trapped
    if (!(Moves&B->BlackPieces&(~(B->BlackPawns)))) { 
      Moves &= ~(B->BlackPieces);
      while (Moves) {
        tsq = FirstPiece(Moves);
        // See if this square is unattacked or suitably defended
        if (B->WAttacks[ConvertSq[tsq]]==0 || SEE(B,sq,tsq,0)>=0) {
          // See if this is not just a dead end
          if (RookMask[tsq]&KingMoves[tsq]&(~(B->BlackPawns))&(~AllMoves)&InvMask[sq]) break;
        }
        RemoveFirst(Moves);
      }
    }
  }
  if (!Moves) score -= RookTrapped[B->Gamestage];
# ifdef DEBUG_TACTICS
  if (oldscore != score) fprintf(stdout,"Trapped (%d), ",score-oldscore);
# endif
#endif
  
#ifdef DEBUG_TACTICS
  fprintf(stdout,"[%d]",score);
#endif
  return score;
}

/* Evaluate the tactics for a knight */
int TactKnight(const int sq,const Board *B,const int side) {
  int efsq=sq,x = File(sq),y=Rank(sq),drive_away,pdef=0,outpost=0,tpts, score=0;
  BITBOARD Moves;
#ifdef DEBUG_TACTICS
  int oldscore;
#endif
#ifdef TRAPPED_KNIGHT
  int tsq;
#endif
  BOOL safe = FALSE;

  /* efsq holds the square which this piece is on (for white), or the effective
   * square when the board is flipped in y, (for black) */
  if (side==BLACK) efsq = Flip[sq];
  
  /* Knights worth less in the open endgame */
  if ((tpts = B->BPts + B->WPts) < 30) score -= (30 - tpts);

  /* Side specific stuff */
  if (side==WHITE) {
#ifdef USE_KING_ATTACK
    /* Opponent king tropism */
    score -= Distance[sq][B->BlackKing];
#endif
    /* Don't block in central pawns */
    if ((sq==d3 || sq==e3) && B->pieces[sq+8]==wpawn) score -= BLOCK_PAWNS;
#ifdef DEBUG_TACTICS
    fprintf(stdout,"Opp.KTrop. -%d, ",Distance[sq][B->BlackKing]);
    if ((sq==d3 || sq==e3) && B->pieces[sq+8]==wpawn) fprintf(stdout,"BlockP, ");
    oldscore = score;
#endif
    if (B->Gamestage <= Middle) {
      /* Penalise them for not developing */
      if (y == Rank1) score -= UNDEVELOPED;
      /* Penalise for moving too far forward in the opening */
      if (y < Rank5) score -= (OVERSTRETCHED * (Middle + 2 - B->Gamestage)) / 2;
#ifdef DEBUG_TACTICS
      if (oldscore!=score) fprintf(stdout,"Bad Develop. %d, ",score-oldscore);
      oldscore = score;
#endif
    }
     /* Test to see if it's pinned to a more valuable piece. */
#ifdef SIMPLE_PINS
    Moves = QueenMoves(B,sq);
    if ((Moves&RankMask[y]&(Mask[B->WhiteKing]|B->WhiteQueens)) && (Moves&RankMask[y]&(B->BlackRooks|B->BlackQueens))) score -= KNIGHT_PINNED;
    if ((Moves&FileMask[x]&(Mask[B->WhiteKing]|B->WhiteQueens)) && (Moves&FileMask[x]&(B->BlackRooks|B->BlackQueens))) score -= KNIGHT_PINNED;
    if ((Moves&DiagMaska1h8[sq]&(Mask[B->WhiteKing]|B->WhiteQueens)) && (Moves&DiagMaska1h8[sq]&(B->BlackBishops|B->BlackQueens))) score -= KNIGHT_PINNED;
    if ((Moves&DiagMaska8h1[sq]&(Mask[B->WhiteKing]|B->WhiteQueens)) && (Moves&DiagMaska8h1[sq]&(B->BlackBishops|B->BlackQueens))) score -= KNIGHT_PINNED;
#ifdef DEBUG_TACTICS
    if (oldscore != score) fprintf(stdout,"Pinned %d, ",score-oldscore);
    oldscore = score;
#endif
#endif
#ifdef TRAPPED_KNIGHT
     /* Get the possible moves to see if it's trapped. */
    Moves = KnightMoves[sq]&(~(B->WhitePieces));
    /* Parse through all squares to which we can move.  Look for a safe one. */
    while (Moves) {
      tsq = FirstPiece(Moves);
      if (B->BAttacks[ConvertSq[tsq]]==0 || SEE(B,sq,tsq,0)>=0) break;
      RemoveFirst(Moves);
    }
    if (!Moves) score -= KNIGHT_TRAPPED;
#endif
  }
  else {
#ifdef USE_KING_ATTACK
    /* Opponent king tropism */
    score -= Distance[sq][B->WhiteKing];
#endif
    /* Don't block in central pawns */
    if ((sq==d6 || sq==e6) && B->pieces[sq-8]==bpawn) score -= BLOCK_PAWNS;
#ifdef DEBUG_TACTICS
    fprintf(stdout,"Opp.KTrop. -%d, ",Distance[sq][B->WhiteKing]);
    if ((sq==d6 || sq==e6) && B->pieces[sq-8]==bpawn) fprintf(stdout,"BlockP, ");
    oldscore = score;
#endif
    if (B->Gamestage <= Middle) {
      /* Penalise them for not developing */
      if (y == Rank8) score -= UNDEVELOPED;
      /* Penalise for moving too far forward in the opening */
      if (y > Rank4) score -= (OVERSTRETCHED * (Middle + 2 - B->Gamestage)) / 2;
#ifdef DEBUG_TACTICS
      if (oldscore!=score) fprintf(stdout,"Bad Develop. %d, ",score-oldscore);
      oldscore = score;
#endif
    }
     /* Test to see if it's pinned to a more valuable piece. */
#ifdef SIMPLE_PINS
    Moves = QueenMoves(B,sq);
    if ((Moves&RankMask[y]&(Mask[B->BlackKing]|B->BlackQueens)) && (Moves&RankMask[y]&(B->WhiteRooks|B->WhiteQueens))) score -= KNIGHT_PINNED;
    if ((Moves&FileMask[x]&(Mask[B->BlackKing]|B->BlackQueens)) && (Moves&FileMask[x]&(B->WhiteRooks|B->WhiteQueens))) score -= KNIGHT_PINNED;
    if ((Moves&DiagMaska1h8[sq]&(Mask[B->BlackKing]|B->BlackQueens)) && (Moves&DiagMaska1h8[sq]&(B->WhiteBishops|B->WhiteQueens))) score -= KNIGHT_PINNED;
    if ((Moves&DiagMaska8h1[sq]&(Mask[B->BlackKing]|B->BlackQueens)) && (Moves&DiagMaska8h1[sq]&(B->WhiteBishops|B->WhiteQueens))) score -= KNIGHT_PINNED;
#ifdef DEBUG_TACTICS
    if (oldscore != score) fprintf(stdout,"Pinned %d, ",score-oldscore);
    oldscore = score;
#endif
#endif
#ifdef TRAPPED_KNIGHT
     /* Get the possible moves to see if it's trapped. */
    Moves = KnightMoves[sq]&(~(B->BlackPieces));
    /* Parse through all squares to which we can move.  Look for a safe one. */
    while (Moves) {
      tsq = FirstPiece(Moves);
      if (B->WAttacks[ConvertSq[tsq]]==0 || SEE(B,sq,tsq,0)>=0) break;
      RemoveFirst(Moves);
    }
    if (!Moves) score -= KNIGHT_TRAPPED;
#endif
  }
#ifdef DEBUG_TACTICS
  if (oldscore!=score) fprintf(stdout,"Trapped %d, ",score-oldscore);
  oldscore = score;
#endif
  
  /* Penalise if pawns can drive it away */
  drive_away = DriveAway(sq,B,side);
  score -= drive_away;

  /* Test for knight outposts */
  if (drive_away==0) {
    /* Outpost for a white knight? */
    if (side==WHITE) {
      if (!(PawnAttacksWhite[sq]&B->BlackPawns)) {
        pdef = Count(B->WhitePawns & PawnAttacksBlack[sq]);
        if (pdef>0 && !B->BlackKnights) {
          if (IsWhite(sq) && !(B->BlackBishops&WhiteMask)) safe=TRUE;
          if (!IsWhite(sq) && !(B->BlackBishops&BlackMask)) safe=TRUE;
        }
      }
    }
    /* Outpost for a black knight? */
    else {
      if (!(PawnAttacksBlack[sq]&B->WhitePawns))
        pdef = Count(B->BlackPawns & PawnAttacksWhite[sq]);
      if (pdef>0 && !B->WhiteKnights) {
        if (IsWhite(sq) && !(B->WhiteBishops&WhiteMask)) safe=TRUE;
        if (!IsWhite(sq) && !(B->WhiteBishops&BlackMask)) safe=TRUE;
      }
    }
     /* This square is not attacked by any enemy pawns.
      * It is also defended by at least one friendly pawn. */
    if (pdef>0)  {
      outpost = KnightOutpost[efsq];
      if (safe)    outpost += KnightOutpost[efsq] >> 1;
      if (pdef==2) outpost += KnightOutpost[efsq] >> 1;
      score += outpost;
#ifdef DEBUG_TACTICS
      if (outpost!=0) fprintf(stdout,"Outpost %d, ",outpost);
#endif
    }
  }
  
#ifdef DEBUG_TACTICS
  fprintf(stdout,"[%d]",score);
#endif
  return score;
}

/* Evaluate the tactics for a bishop */
int TactBishop(const int sq,const Board *B,const int side) {
  int tpts,y = Rank(sq),x = File(sq), score=0;
#ifdef BISHOP_MOBILITY
  int nmoves;
  BITBOARD mask;
#ifdef DEBUG_TACTICS
  int oldscore;
#endif
#endif
#ifdef SIMPLE_PINS
  BITBOARD Moves;
#endif
  
  /* Bishops worth more in the open endgame */
  if ((tpts = B->BPts + B->WPts) < 30) score += (30 - tpts);

  /* Side specific stuff */
  if (side==WHITE) {
    /* Don't block in central pawns */
    if ((sq==d3 || sq==e3) && B->pieces[sq+8]==wpawn) score -= BLOCK_PAWNS;
#ifdef DEBUG_TACTICS
    if ((sq==d3 || sq==e3) && B->pieces[sq+8]==wpawn) fprintf(stdout,"BlockP, ");
    oldscore = score;
#endif
    if (B->Gamestage <= Middle) {
      /* Penalise for not developing */
      if (y == Rank1) score -= UNDEVELOPED;
      /* Penalise for moving too far forward in the opening */
      if (y < Rank5) score -= (OVERSTRETCHED * (2 + Middle - B->Gamestage)) / 2;
#ifdef DEBUG_TACTICS
      if (oldscore!=score) fprintf(stdout,"Bad Develop. %d, ",score-oldscore);
      oldscore = score;
#endif
    }
#ifdef DEBUG_TACTICS
      if (oldscore!=score) fprintf(stdout,"Trapped %d, ",score-oldscore);
      oldscore = score;
#endif
     /* Test to see if it's pinned to a more valuable piece. */
#ifdef SIMPLE_PINS
    Moves = QueenMoves(B,sq);
    if ((Moves&RankMask[y]&(Mask[B->WhiteKing]|B->WhiteQueens)) && (Moves&RankMask[y]&(B->BlackRooks|B->BlackQueens))) score -= BISHOP_PINNED;
    if ((Moves&FileMask[x]&(Mask[B->WhiteKing]|B->WhiteQueens)) && (Moves&FileMask[x]&(B->BlackRooks|B->BlackQueens))) score -= BISHOP_PINNED;
    if ((Moves&DiagMaska1h8[sq]&(Mask[B->WhiteKing]|B->WhiteQueens)) && (Moves&DiagMaska1h8[sq]&B->BlackBishops)) score -= BISHOP_PINNED;
    if ((Moves&DiagMaska8h1[sq]&(Mask[B->WhiteKing]|B->WhiteQueens)) && (Moves&DiagMaska8h1[sq]&B->BlackBishops)) score -= BISHOP_PINNED;
#ifdef DEBUG_TACTICS
    if (oldscore != score) fprintf(stdout,"Pinned %d, ",score-oldscore);
#endif
#endif
  }
  else {
    /* Don't block in central pawns */
    if ((sq==d6 || sq==e6) && B->pieces[sq-8]==bpawn) score -= BLOCK_PAWNS;
#ifdef DEBUG_TACTICS
    if ((sq==d6 || sq==e6) && B->pieces[sq-8]==bpawn) fprintf(stdout,"BlockP, ");
    oldscore = score;
#endif
    if (B->Gamestage <= Middle) {
      /* Penalise for not developing */
      if (y == Rank8) score -= UNDEVELOPED;
      /* Penalise for moving too far forward in the opening */
      if (y > Rank4) score -= (OVERSTRETCHED * (2 + Middle - B->Gamestage)) / 2;
#ifdef DEBUG_TACTICS
      if (oldscore!=score) fprintf(stdout,"Bad Develop. %d, ",score-oldscore);
      oldscore = score;
#endif
    }
#ifdef DEBUG_TACTICS
      if (oldscore!=score) fprintf(stdout,"Trapped %d, ",score-oldscore);
      oldscore = score;
#endif
     /* Test to see if it's pinned to a more valuable piece. */
#ifdef SIMPLE_PINS
    Moves = QueenMoves(B,sq);
    if ((Moves&RankMask[y]&(Mask[B->BlackKing]|B->BlackQueens)) && (Moves&RankMask[y]&(B->WhiteRooks|B->WhiteQueens))) score -= BISHOP_PINNED;
    if ((Moves&FileMask[x]&(Mask[B->BlackKing]|B->BlackQueens)) && (Moves&FileMask[x]&(B->WhiteRooks|B->WhiteQueens))) score -= BISHOP_PINNED;
    if ((Moves&DiagMaska1h8[sq]&(Mask[B->BlackKing]|B->BlackQueens)) && (Moves&DiagMaska1h8[sq]&B->WhiteBishops)) score -= BISHOP_PINNED;
    if ((Moves&DiagMaska8h1[sq]&(Mask[B->BlackKing]|B->BlackQueens)) && (Moves&DiagMaska8h1[sq]&B->WhiteBishops)) score -= BISHOP_PINNED;
#ifdef DEBUG_TACTICS
    if (oldscore != score) fprintf(stdout,"Pinned %d, ",score-oldscore);
#endif
#endif
  }

#ifdef BISHOP_MOBILITY
#ifdef DEBUG_TACTICS
  oldscore = score;
#endif

  /* Mobility Bonus/Penalty */
  mask = BishopMoves(B,sq);

  switch(side) {
    case WHITE: mask &= ~(B->WhitePieces); break;
    case BLACK: mask &= ~(B->BlackPieces); break;
  }

  switch((nmoves = Count(mask))) {
    case 0 : score -= BISHOP_TIGHT*3; break;
    case 1 : score -= BISHOP_TIGHT*2; break;
    case 2 : score -= BISHOP_TIGHT; break;
	default : break;
//    default: score += nmoves; break;
  }
#ifdef DEBUG_TACTICS
  fprintf(stdout,"Mobility %d, ",score-oldscore);
#endif
#endif

#ifdef DEBUG_TACTICS
  fprintf(stdout,"[%d]",score);
#endif
  return score;
}

/* Evaluate the tactics for a queen */
int TactQueen(const int sq,const Board *B,const int side) {
  int efsq=sq,x = File(sq),y=Rank(sq),nmoves,nquart;
  int to, tpts = B->BPts + B->WPts, score=0;
  BITBOARD Moves,Targets;
#ifdef DEBUG_TACTICS
  int oldscore;
#endif

  /* efsq holds the square which this piece is on (for white), or the effective
   * square when the board is flipped in y, (for black) */
  if (side==BLACK) efsq = Flip[sq];

   /* Check for mobility */
  Moves = QueenMoves(B,sq);

  /* Bonus for 7th Rank Attacks */
  if (Rank(efsq)==Rank7 && B->Gamestage<LateEnd) score += QUEEN_7TH_RANK;
#ifdef DEBUG_TACTICS
  if (Rank(efsq)==Rank7 && B->Gamestage<LateEnd) fprintf(stdout,"7th Rank (%d), ",QUEEN_7TH_RANK);
  oldscore = score;
#endif
  
  if (side==WHITE) {
    /* Defending Passed Pawn */
    if ((B->WhitePawns & PassedMask) & FileUpMask[sq] & Moves) {
      score += ROOK_BEHIND_PP;
#ifdef DEBUG_TACTICS
      fprintf(stdout,"DefPP %d, ",score-oldscore);
      oldscore = score;
#endif
    }
    /* Freefile Bonus */
    else if (!(B->BlackPawns & FileUpMask[sq])) {
      score += HALF_OPEN_FILE_Q;
      if (!(FileMask[x] & (B->WhitePawns | B->BlackQueens | B->BlackRooks)))
        score += OPEN_FILE_Q;
#ifdef DEBUG_TACTICS
      fprintf(stdout,"OpenFile %d, ",score-oldscore);
      oldscore = score;
#endif
    }
    /* Don't block in central pawns */
    if ((sq==d3 || sq==e3) && B->pieces[sq+8]==wpawn) score -= BLOCK_PAWNS;
    /* Bonus if no opponent queen */
    if (!B->BlackQueens) score += LONE_QUEEN;
#ifdef DEBUG_TACTICS
    if ((sq==d3 || sq==e3) && B->pieces[sq+8]==wpawn) fprintf(stdout,"BlockP, ");
    if (!B->BlackQueens) fprintf(stdout,"Lone, ");
    oldscore = score;
#endif

#ifdef USE_KING_ATTACK
    /* Bonus for attacking opponent king (especially if defended) */
    if (abs(x - File(B->BlackKing)) < 2) {
      score += QUEEN_ATTACK;
      if (Moves&FileMask[x]&B->WhiteRooks) {
        score += QUEEN_ATTACK_DEF;
        if (Moves&KingMoves[B->BlackKing]) score += QUEEN_ATTACK_DEF;
      }
    }
    if (abs(y - Rank(B->BlackKing)) < 2) {
      score += QUEEN_ATTACK;
      if (Moves&RankMask[y]&B->WhiteRooks) {
        score += QUEEN_ATTACK_DEF;
        if (Moves&KingMoves[B->BlackKing]) score += QUEEN_ATTACK_DEF;
      }
    }
    if (abs(x-File(B->BlackKing)) == abs(y-Rank(B->BlackKing))) {
      score += QUEEN_ATTACK;
      if (Moves&BishopMask[B->BlackKing]&BishopMask[sq]&B->WhiteBishops) {
        score += QUEEN_ATTACK_DEF;
        if (Moves&KingMoves[B->BlackKing]) score += QUEEN_ATTACK_DEF;
      }
    }
# ifdef DEBUG_TACTICS
    if (score!=oldscore) fprintf(stdout,"KAtt %d, ",score-oldscore);
    oldscore = score;
# endif
#endif
  }
   /* Now for Black */
  else {
    /* Defending Passed Pawn */
    if ((B->BlackPawns & PassedMask) & FileDownMask[sq] & Moves) {
      score += ROOK_BEHIND_PP;
#ifdef DEBUG_TACTICS
      fprintf(stdout,"DefPP %d, ",score-oldscore);
      oldscore = score;
#endif
    }
    /* Freefile Bonus */
    else if (!(B->WhitePawns & FileDownMask[sq])) {
      score += HALF_OPEN_FILE_Q;
      if (!(FileMask[x] & (B->BlackPawns | B->WhiteQueens | B->WhiteRooks)))
        score += OPEN_FILE_Q;
#ifdef DEBUG_TACTICS
      fprintf(stdout,"OpenFile %d, ",score-oldscore);
      oldscore = score;
#endif
    }
    /* Don't block in central pawns */
    if ((sq==d6 || sq==e6) && B->pieces[sq-8]==bpawn) score -= BLOCK_PAWNS;
    /* Bonus if no opponent queen */
    if (!B->WhiteQueens) score += LONE_QUEEN;
#ifdef DEBUG_TACTICS
    if ((sq==d6 || sq==e6) && B->pieces[sq-8]==bpawn) fprintf(stdout,"BlockP, ");
    if (!B->WhiteQueens) fprintf(stdout,"Lone, ");
    oldscore = score;
#endif
#ifdef USE_KING_ATTACK
    /* Bonus for attacking opponent king (especially if defended) */
    if (abs(x - File(B->WhiteKing)) < 2) {
      score += QUEEN_ATTACK;
      if (Moves&FileMask[x]&B->BlackRooks) {
        score += QUEEN_ATTACK_DEF;
        if (Moves&KingMoves[B->WhiteKing]) score += QUEEN_ATTACK_DEF;
      }
    }
    if (abs(y - Rank(B->WhiteKing)) < 2) {
      score += QUEEN_ATTACK;
      if (Moves&RankMask[y]&B->BlackRooks) {
        score += QUEEN_ATTACK_DEF;
        if (Moves&KingMoves[B->WhiteKing]) score += QUEEN_ATTACK_DEF;
      }
    }
    if (abs(x-File(B->WhiteKing)) == abs(y-Rank(B->WhiteKing))) {
      score += QUEEN_ATTACK;
      if (Moves&BishopMask[B->WhiteKing]&BishopMask[sq]&B->BlackBishops) {
        score += QUEEN_ATTACK_DEF;
        if (Moves&KingMoves[B->WhiteKing]) score += QUEEN_ATTACK_DEF;
      }
    }
# ifdef DEBUG_TACTICS
    if (score != oldscore) fprintf(stdout,"KAtt %d, ",score-oldscore);
    oldscore = score;
# endif
#endif
  }
  
  /* Penalise slightly if pawns can drive it away */
  score -= DriveAway(sq,B,side) >> 1;
#ifdef DEBUG_TACTICS
  if (oldscore != score) fprintf(stdout,"[%d], ", score - oldscore);
  oldscore = score;
#endif
  
   /* Penalty for bringing the queen out too far too early */
  if (tpts>74 && Rank(efsq)!=Rank1) score -= (min(Rank1-Rank(efsq),2) * EARLY_QUEEN_PENALTY * (tpts-74));
#ifdef DEBUG_TACTICS
  if (oldscore != score) fprintf(stdout,"Early Adv. %d, ",score - oldscore);
#endif
  
  /* Test to see if it's pinned to the king.  This one is 
   * a little bit scary ;) */
#ifdef SIMPLE_PINS
  if (side==WHITE) {
    // Pinned by a lesser piece
    if ((Moves&RankMask[y]&Mask[B->WhiteKing]) && (Moves&RankMask[y]&B->BlackRooks)) score -= QUEEN_PINNED;
    if ((Moves&FileMask[x]&Mask[B->WhiteKing]) && (Moves&FileMask[x]&B->BlackRooks)) score -= QUEEN_PINNED;
    if ((Moves&DiagMaska1h8[sq]&Mask[B->WhiteKing]) && (Moves&DiagMaska1h8[sq]&B->BlackBishops)) score -= QUEEN_PINNED;
    if ((Moves&DiagMaska8h1[sq]&Mask[B->WhiteKing]) && (Moves&DiagMaska8h1[sq]&B->BlackBishops)) score -= QUEEN_PINNED;
    // Pinned by another queen
    if ((Moves&RankMask[y]&Mask[B->WhiteKing]) && (Moves&RankMask[y]&B->BlackQueens)) score -= QUEEN_PINNED>>1;
    if ((Moves&FileMask[x]&Mask[B->WhiteKing]) && (Moves&FileMask[x]&B->BlackQueens)) score -= QUEEN_PINNED>>1;
    if ((Moves&DiagMaska1h8[sq]&Mask[B->WhiteKing]) && (Moves&DiagMaska1h8[sq]&B->BlackQueens)) score -= QUEEN_PINNED>>1;
    if ((Moves&DiagMaska8h1[sq]&Mask[B->WhiteKing]) && (Moves&DiagMaska8h1[sq]&B->BlackQueens)) score -= QUEEN_PINNED>>1;
  }
  else {
    // Pinned by a lesser piece
    if ((Moves&RankMask[y]&Mask[B->BlackKing]) && (Moves&RankMask[y]&B->WhiteRooks)) score -= QUEEN_PINNED;
    if ((Moves&FileMask[x]&Mask[B->BlackKing]) && (Moves&FileMask[x]&B->WhiteRooks)) score -= QUEEN_PINNED;
    if ((Moves&DiagMaska1h8[sq]&Mask[B->BlackKing]) && (Moves&DiagMaska1h8[sq]&B->WhiteBishops)) score -= QUEEN_PINNED;
    if ((Moves&DiagMaska8h1[sq]&Mask[B->BlackKing]) && (Moves&DiagMaska8h1[sq]&B->WhiteBishops)) score -= QUEEN_PINNED;
    // Pinned by another queen
    if ((Moves&RankMask[y]&Mask[B->BlackKing]) && (Moves&RankMask[y]&B->WhiteQueens)) score -= QUEEN_PINNED>>1;
    if ((Moves&FileMask[x]&Mask[B->BlackKing]) && (Moves&FileMask[x]&B->WhiteQueens)) score -= QUEEN_PINNED>>1;
    if ((Moves&DiagMaska1h8[sq]&Mask[B->BlackKing]) && (Moves&DiagMaska1h8[sq]&B->WhiteQueens)) score -= QUEEN_PINNED>>1;
    if ((Moves&DiagMaska8h1[sq]&Mask[B->BlackKing]) && (Moves&DiagMaska8h1[sq]&B->WhiteQueens)) score -= QUEEN_PINNED>>1;
  }
#ifdef DEBUG_TACTICS
  if (oldscore != score) fprintf(stdout,"Pinned %d, ",score-oldscore);
  oldscore = score;
#endif
#endif

   /* Count only moves into empty spaces or attacks */
  if (side==WHITE) Moves &= ~B->WhitePieces;
  else             Moves &= ~B->BlackPieces;

  Targets = Moves;
  nmoves=0;

  while (Targets) {
    to = FirstPiece(Targets);
     // Check that this square is unattacked or at least defended sufficiently
    if (side == WHITE && B->BAttacks[ConvertSq[to]]==0) {
      nmoves++;
      if (B->WAttacks[ConvertSq[to]]>1) nmoves++;
    }
    if (side == BLACK && B->WAttacks[ConvertSq[to]]==0) {
      nmoves++;
      if (B->BAttacks[ConvertSq[to]]>1) nmoves++;
    }
     // Break out - we're safe already
    if (nmoves==5) break;
    RemoveFirst(Targets);
  }

   /* Reward the queen if it is defended */
  if (side == WHITE && B->WAttacks[ConvertSq[sq]]>0) nmoves++;
  if (side == BLACK && B->BAttacks[ConvertSq[sq]]>0) nmoves++;

   /* We have very few safe moves for this queen - penalise it!
    * (Especially if this is a late stage in the game) */
  if (nmoves<5) {
    switch (B->Gamestage) {
    case Opening  : score -= QUEEN_TRAPPED * (5-nmoves) / 2; break;
    case EarlyMid : score -= QUEEN_TRAPPED * (5-nmoves); break;
    default       : score -= QUEEN_TRAPPED * (5-nmoves) * 2; break;
    }
#ifdef DEBUG_TACTICS
    fprintf(stdout,"Trapped %d, ", score-oldscore);
    oldscore = score;
#endif
  }

  /* Check that the queen can attack into each quartile of the board */
  nquart=0;
  if (Moves&BlackKingSide) nquart++;
  if (Moves&BlackQueenSide) nquart++;
  if (Moves&WhiteKingSide) nquart++;
  if (Moves&WhiteQueenSide) nquart++;
  switch(nquart) {
    case (0) : score -= QUEEN_IMMOBILE;
    case (1) : score -= QUEEN_IMMOBILE;
    case (2) : score -= QUEEN_IMMOBILE; break;
    case (4) : score += QUEEN_MOBILE; break;
  }

#ifdef DEBUG_TACTICS
  fprintf(stdout,"Quarts %d (%d), ",nquart,score-oldscore);
  fprintf(stdout,"[%d]",score);
#endif
  return score;
}

/* Evaluate the tactics for a king */
int TactKing(const int sq,const Board *B,const int side) {
#ifdef KING_BOXED_IN
  int y = Rank(sq);
  int x = File(sq);
#endif
  int score=0;
#ifdef DEBUG_TACTICS
  int oldscore=0;
#endif

  /* Side specific stuff for white */
  if (side==WHITE) {
    /* If the opponent has few points left */
    if (B->BPts<5) {
      /* Reward for keeping the kings close together in this case */
      score = 16 - (4 * Distance[B->BlackKing][B->WhiteKing]);
      if (B->BPts==0) score *= 2;
#ifdef DEBUG_TACTICS
      fprintf(stdout,"Prox, ",score);
#endif
    }

     /* If we have few pieces left and the opp. has a rook or a queen then
      * avoid the edges */
    if (B->WPts<5 && B->bmaj>0) {
#ifdef DEBUG_TACTICS
      oldscore = score;
#endif
      score += EdgePenalty[sq];
      if (B->WPts<3)  score += EdgePenalty[sq];
      if (B->WPts==0) score += EdgePenalty[sq];
#ifdef DEBUG_TACTICS
      fprintf(stdout,"Edge %d, ",score-oldscore);
      oldscore = score;
#endif
    }

#ifdef KING_BOXED_IN
     /*  Check to see if boxed in on back rank (vulnerable to back rank mates) */   
    if (B->Gamestage>=Middle && y == Rank1 && B->bmaj && (Mask[sq-8]&B->WhitePawns) &&
       (x==0 || (Mask[sq-9]&B->WhitePawns)) && 
       (x==7 || (Mask[sq-7]&B->WhitePawns)) &&
       !(RankMask[Rank1]&rq)) {
      score -= BACK_RANK_UNSAFE;
#ifdef DEBUG_TACTICS
      fprintf(stdout,"Boxed in, ");
      oldscore = score;
#endif
    }
#endif
  }
  /* And now for black */
  else {
    /* If the opponent has few points left */
    if (B->WPts<5) {
      /* Reward for keeping the kings close together in this case */
      score = 16 - (4 * Distance[B->BlackKing][B->WhiteKing]);
      if (B->WPts==0) score *= 2;
#ifdef DEBUG_TACTICS
      fprintf(stdout,"Prox., ",score);
#endif
    }

    /* If we have few pieces left and the opp. has a rook or a queen then
     * avoid the edges */
    if (B->BPts<5 && B->wmaj) {
#ifdef DEBUG_TACTICS
      oldscore = score;
#endif
      score += EdgePenalty[sq];
      if (B->BPts<3)  score += EdgePenalty[sq];
      if (B->BPts==0) score += EdgePenalty[sq];
#ifdef DEBUG_TACTICS
      fprintf(stdout,"Edge %d, ",score-oldscore);
      oldscore = score;
#endif
    }
#ifdef KING_BOXED_IN
  /*  Check to see if boxed in on back rank (vulnerable to back rank mates) */   
  if (B->Gamestage>=Middle && y == Rank8 && B->wmaj && (Mask[sq+8]&B->BlackPawns) &&
            (x==0 || (Mask[sq+7]&B->BlackPawns)) && 
            (x==7 || (Mask[sq+9]&B->BlackPawns)) && 
            !(RankMask[Rank8]&rq)) {
      score -= BACK_RANK_UNSAFE;
#ifdef DEBUG_TACTICS
      fprintf(stdout,"Boxed in, ");
      oldscore = score;
#endif
    }
#endif
  }
  
#ifdef DEBUG_TACTICS
  fprintf(stdout,"[%d]",score);
#endif
  return score;
}



/*     ---------------====    OTHER ROUTINES     ====-------------- */




/* Check to see if a pawn is backward */
int BackwardsPawn(const int sq,const Board *B,const int side, const BOOL block) {
  int score = 6;
  BOOL sidep = FALSE;
  
  if (side==WHITE) {
    if (sq < a4) return 0;
    if (PawnAttacksBlack[sq]&B->WhitePawns) return 0;
    if (!(PawnAttacksBlack[sq]&B->All) && ((PawnAttacksBlack[sq]<<8)&B->WhitePawns)) score -= 4;
    if (FileUpMask[sq]&B->BlackPawns) score -= 2;
    if (SideMask[sq]&B->WhitePawns) {score--;sidep=TRUE;}
    if (!block) {
      if (sidep) return 0;
      score -= 4;
    }
    if (sq>=a2 && PType(B->pieces[sq-8]) != pawn && PType(B->pieces[sq-16]) != pawn
        && PawnAttacksWhite[sq]&B->WhitePawns) score -= 3;
  }
  else {
    if (sq > h5) return 0;
    if (PawnAttacksWhite[sq]&B->BlackPawns) return 0;
    if (!(PawnAttacksWhite[sq]&B->All) && ((PawnAttacksWhite[sq]>>8)&B->BlackPawns)) score -= 4;
    if (FileDownMask[sq]&B->WhitePawns) score -= 2;
    if (SideMask[sq]&B->BlackPawns) {score--;sidep=TRUE;}
    if (!block) {
      if (sidep) return 0;
      score -= 4;
    }
    if (sq<=h7 && PType(B->pieces[sq+8]) != pawn && PType(B->pieces[sq+16]) != pawn
        && PawnAttacksBlack[sq]&B->BlackPawns) score -= 3;
  }
  if (score<1) return 0;
  return Backward[score][File(sq)];
}

/* Overall positional bonuses for a few things. Scored for white.
   Includes castling / spoiling castling and retention of centre
   pawns / control of the centre. */
int TacticsPositional(const Board *B) {
  int wk,bk,wkx,bkx, score=0;
  BOOL castledw=TRUE,castledb=TRUE,cp=FALSE;
#ifdef DEBUG_TACTICS
  int oldscore = 0;
#endif

  wk = B->WhiteKing;
  bk = B->BlackKing;

  /* Test NOT on the actual act of having castled, because this is 
   * dumb after the opening.  Instead test for the problems caused by
   * not having castled, such as underdeveloped rooks. */
  wkx = File(wk);
  if (wk>=a1) { /* King on back row */
    if (B->Gamestage <= EarlyMid && wkx>FileC && wkx<FileF) castledw = FALSE;
    else if ((wkx==FileC || wkx==FileD || wkx==FileE) &&
	     (LLCorner&B->WhiteRooks)) castledw = FALSE;
    else if ((wkx==FileE || wkx==FileF) &&
	     (LRCorner&B->WhiteRooks)) castledw = FALSE;
  }
  if (wkx>FileD && (RankRightMask[wk]|RankRightMask[wk-8])&B->WhiteRooks) castledw = FALSE;
  if (wkx<FileE && (RankLeftMask[wk]|RankLeftMask[wk-8])&B->WhiteRooks) castledw = FALSE;
  if (wkx>FileC && wkx<FileG) castledw = FALSE;
   /* And now for black */
  bkx = File(bk);
  if (bk<=h8) { /* King on back row */
    if (B->Gamestage <= EarlyMid && bkx>FileC && bkx<FileF) castledb=FALSE;
    else if ((bkx==FileC || bkx==FileD || bkx==FileE) &&
	     (ULCorner&B->BlackRooks)) castledb=FALSE;
    else if ((bkx==FileE || bkx==FileF) &&
	     (URCorner&B->BlackRooks)) castledb=FALSE;
  }
  if (bkx>FileD && (RankRightMask[bk]|RankRightMask[bk+8])&B->BlackRooks) castledb = FALSE;
  if (bkx<FileE && (RankLeftMask[bk]|RankLeftMask[bk+8])&B->BlackRooks) castledb = FALSE;
  if (bkx>FileC && bkx<FileG) castledb = FALSE;

#ifdef DEBUG_TACTICS
  fprintf(stdout,"Effective Castling : ");
  if (castledw) fprintf(stdout,"WHITE ");
  if (castledb) fprintf(stdout,"BLACK");
  if (!castledw && !castledb) fprintf(stdout,"NONE");
  fprintf(stdout,"\n");
  oldscore = score;
#endif  

   /* If we haven't castled then penalise.  Penalise further if
    * we've messed up our chances of castling in the future. */
  if (!castledw) {
    score -= REWARD_CASTLE;
    if ((B->castle&3)<3) {
      score -= SPOILT_CASTLE_1;
      if ((B->castle&3)==0) score -= SPOILT_CASTLE_2;
    }
#ifdef DEBUG_TACTICS
    fprintf(stdout,"White Not Castled %d\n",score - oldscore);
    oldscore = score;
#endif
  }
  if (!castledb) {
    score += REWARD_CASTLE;
    if (B->castle<12) {
      score += SPOILT_CASTLE_1;
      if (B->castle<4) score += SPOILT_CASTLE_2;
    }
#ifdef DEBUG_TACTICS
    fprintf(stdout,"Black Not Castled %d\n",score - oldscore);
    oldscore = score;
#endif
  }
  
   /* Encourage retention of centre-file pawns until the end of the middlegame */
  cp=FALSE;
  if (B->WhitePawns & FileMask[FileD]) {score += CENTRE_PAWN_BONUS;cp=TRUE;}
  if (B->WhitePawns & FileMask[FileE]) {score += CENTRE_PAWN_BONUS;cp=TRUE;}
  if (!cp) score -= NO_CENTRE_PAWNS;
#ifdef DEBUG_TACTICS
  if (score!=oldscore) fprintf(stdout,"White centre pawns : %d\n",score-oldscore);
  oldscore = score;
#endif
  cp=FALSE;
  if (B->BlackPawns & FileMask[FileD]) {score -= CENTRE_PAWN_BONUS;cp=TRUE;}
  if (B->BlackPawns & FileMask[FileE]) {score -= CENTRE_PAWN_BONUS;cp=TRUE;}
  if (!cp) score += NO_CENTRE_PAWNS;
#ifdef DEBUG_TACTICS
  if (score!=oldscore) fprintf(stdout,"Black centre pawns : %d\n",oldscore-score);
#endif

   /* Return the final positional score */
#ifdef DEBUG_TACTICS
  fprintf(stdout,"Positional Score : %d\n",score);
#endif
  return score;
}

/* Returns a score depending on how well defended or badly attacked 
 * your king is. This is a very important function, and is well worth
 * spending some time improving. Greatly reduces (or removes) this score
 * when we reach the endgame and the king is often better off on easily-
 * attacked squares. */
int KingDefence(const Board *B, const int kpos, const int side) {
  int p,nopatt=1,sq,kx=File(kpos),ky=Rank(kpos),shield=0,cancastle,px,def=0;
  int n,*Area = KingArea[kpos],score=0,open=0,kshield=0,qshield=0,tsq,count_attacks;
  BITBOARD OppQueens,OppQTargets,PShield,PStorm,FriendlyPawns,Att;
  BOOL QAttack = FALSE;
#ifdef COUNT_CHECKS
  BITBOARD Weakness, Enemy;
#endif
#ifdef DEBUG_TACTICS
  int olddef;
#endif  

  /* Loop through the squares near the king */
  if (side == WHITE) FriendlyPawns = B->WhitePawns;
  else FriendlyPawns = B->BlackPawns;
  for (n=0; (sq=Area[n])>-1; n++) {
    p = B->pieces[sq];
#ifdef DEBUG_TACTICS
  if (p) PrintPiece(p);
#endif
    if (side==BLACK) p=-p;
    
    /* Test for the pieces attacking/defending */
    switch(p) {
    case (wpawn)   : def++;  if (PairMask[sq]&FriendlyPawns) def++; break;  /* Nearby pawn */
//    case (wrook)   : def++;  break;  /* Nearby rook */
//    case (wknight) : def++;  break;  /* Nearby knight */
//    case (wbishop) : def++;  break;  /* Nearby bishop */
    case (wqueen)  : def++;  break;  /* Nearby queen */
    case (bpawn)   : def--;  break;  /* Nearby enemy pawn */
    case (brook)   : def-=4; break;  /* Nearby enemy rook */
    case (bknight) : def-=2; break;  /* Nearby enemy knight */
    case (bbishop) : def--;  break;  /* Nearby enemy bishop */
    case (bqueen)  : def-=6; break;  /* Nearby enemy queen */
    }
     /* Check for double attacks next to the king */
    if (Mask[sq]&KingMoves[kpos]) {
      Att = AttacksTo(B,sq);
      count_attacks = Count(Att&B->WhitePieces) - Count(Att&B->BlackPieces);
      if (side == WHITE && count_attacks<0) {
        def += count_attacks<<1;
#ifdef DEBUG_TACTICS
   fprintf(stdout,"X");
#endif
      }
      if (side == BLACK && count_attacks>0) {
        def -= count_attacks<<1;
#ifdef DEBUG_TACTICS
   fprintf(stdout,"X");
#endif
      }
    }
  } 

#ifdef DEBUG_TACTICS
  fprintf(stdout," DEF=%d, ",def);
#endif

   /* Side specific stuff for a White King */
  if (side==WHITE) {
     /* Get a rough idea of opponent's attacking strength */
    nopatt = B->bmaj*2 + B->bmin + 1;

#ifdef ADJACENT_PAWNS
     /* Bonus for adjacent pawns */
    def += Count(KingMoves[kpos]&B->WhitePawns);
# ifdef DEBUG_TACTICS
    fprintf(stdout,"P=%d, ",Count(KingMoves[kpos]&B->WhitePawns));
# endif
#endif // ADJACENT_PAWNS

     /* Penalty for being on an empty file.
      * ...but only if opp. has rook(s)/queen(s) and there are lots of pieces left. */
    if (B->Gamestage<Endgame && B->bmaj) {
      if (!(B->WhitePawns & FileUpMask[kpos])) {
        open=1;
        def -= HALF_OPEN_FILE_K; /*  King on half-open file */
        if (!(B->BlackPawns & FileUpMask[kpos])) {
          open=2;
          def -= OPEN_FILE_K; /*  ..or fully open file */
        }
#ifdef DEBUG_TACTICS
        if (open==2) fprintf(stdout,"OF, ");
        else fprintf(stdout,"SOF, ");
#endif
      }
    }

     /* Penalise if defending pawns are in fianchetto formation and the defending bishop is
      * not still on the board. */
#ifdef DEBUG_TACTICS
    olddef=def;
#endif
    if (ky == Rank1) {
      if (kx>FileE) {
        if (B->pieces[g2] != wpawn) {
          if (!(B->WhiteBishops&WhiteMask)) def--;
          if ((Mask[f3]|Mask[h3]) & (B->BlackQueens | B->BlackBishops | B->BlackPawns)) def--;
        }
      }
      else if (kx<FileD) {
        if (B->pieces[b2] != wpawn) {
          if (!(B->WhiteBishops&BlackMask)) def--;
          if ((Mask[a3]|Mask[c3]) & (B->BlackQueens | B->BlackBishops | B->BlackPawns)) def--;
        }
      }
    }
#ifdef DEBUG_TACTICS
    if (olddef!=def) fprintf(stdout,"Ftto/NoB, ");
    olddef = def;
#endif

     /* Get pawn shield */
    cancastle = B->castle & 3;
    if (cancastle) { /* Preserve side file pawns if we can still castle */
      if (cancastle&1) { /* Can still castle king's side - this is preferable */
        if (B->WhitePawns & FileMask[FileF]) kshield++;
        if (B->WhitePawns & FileMask[FileG]) kshield++;
        if (B->WhitePawns & FileMask[FileH]) kshield++;
      }
      if (cancastle&2) { /* Can still castle queen's side */
        if (B->WhitePawns & FileMask[FileA]) qshield++;
        if (B->WhitePawns & FileMask[FileB]) qshield++;
        if (B->WhitePawns & FileMask[FileC]) qshield++;
      }
      // See which is best
      shield = max(kshield,qshield);
    }
    else { /* We can't castle - consider pawns in front of king only */
      if (kx==FileH) px=FileG;
      else px = max(kx, FileB);
      if (B->WhitePawns & FileMask[px-1]) shield++;
      if (B->WhitePawns & FileMask[px]  ) shield++;
      if (B->WhitePawns & FileMask[px+1]) shield++;      
    }

    /* Check for queen+rook attacks on adjoining, open files */
    if (kx>FileA && !(B->WhitePawns & FileUpMask[kpos-1]))
      def -= (2*Count(FileUpMask[kpos-1]&B->BlackQueens) + Count(FileUpMask[kpos-1]&B->BlackRooks)); 
    if (kx<FileH && !(B->WhitePawns & FileUpMask[kpos+1]))
      def -= (2*Count(FileUpMask[kpos+1]&B->BlackQueens) + Count(FileUpMask[kpos+1]&B->BlackRooks)); 
    if (!(B->WhitePawns & FileUpMask[kpos]))
      def -= (2*Count(FileUpMask[kpos]&B->BlackQueens) + Count(FileUpMask[kpos]&B->BlackRooks)); 
#ifdef DEBUG_TACTICS
    if (olddef!=def) fprintf(stdout,"FAtt %d, ",def-olddef);
    olddef = def;
#endif

    /* Check for pawn storms */
    PShield = PawnShieldMaskWhite[kpos] & B->WhitePawns;
    PStorm  = ((PShield>>7) & ~FileMask[FileA]) | ((PShield>>9) & ~FileMask[FileH]);
    PStorm |= ((PStorm&(~(B->All)))>>8);
    def -= Count(PStorm&B->BlackPawns);
#ifdef DEBUG_TACTICS
    if (olddef!=def) fprintf(stdout,"Storm %d, ",def-olddef);
    olddef = def;
#endif

    /* Beware of dangerous side-file attacks if enemy has rooks or queens
     * and the sides of the board are open */
    if (B->BlackQueens || B->BlackRooks) {
      if (kx>=FileE && !((B->WhitePawns | B->BlackPawns)&FileMask[FileH])) {
        score -= SideAttack[B->Gamestage];
        if (FileMask[FileH] & (B->BlackQueens | B->BlackRooks)) score -= SideAttack[B->Gamestage];
      }
      if (kx<=FileD && !((B->WhitePawns | B->BlackPawns)&FileMask[FileA])) {
        score -= SideAttack[B->Gamestage];
        if (FileMask[FileA] & (B->BlackQueens | B->BlackRooks)) score -= SideAttack[B->Gamestage];
      }
#ifdef DEBUG_TACTICS
      if (score) fprintf(stdout,"Side %d, ",score);
#endif
    }
#ifdef COUNT_CHECKS
    /* Test for vulnerability to checking moves */
    Weakness = QueenMoves(B,kpos)&(~B->BlackPieces);
    Enemy = B->BlackPieces&(~B->BlackPawns);
    while (Enemy) {
      sq = FirstPiece(Enemy);
      switch(B->pieces[sq]) {
       case (bqueen): def -= Count(QueenMoves(B,sq)&Weakness); break;
       case (brook): def -= Count(RookMoves(B,sq)&Weakness&RookMask[kpos]); break;
       case (bbishop): def -= Count(BishopMoves(B,sq)&Weakness&BishopMask[kpos]); break;
       case (bknight): if (DoubleKnightMove[sq]&Mask[kpos]) def--; break;
      }
      RemoveFirst(Enemy);
    }
# ifdef DEBUG_TACTICS
    if (olddef != def) fprintf(stdout,"NChk %d, ", olddef-def);
    olddef = def;
# endif
#endif

#ifdef DEFENDED_QUEEN_ATTACKS
    OppQueens = B->BlackQueens;
    // Loop through opponent's queens
    while (OppQueens) {
      sq = FirstPiece(OppQueens);
      OppQTargets = QueenMoves(B,sq)&KingMoves[kpos];
      // Loop through the squares to which this queen can move
      // and which are next to your king
      while (OppQTargets) {
        tsq = FirstPiece(OppQTargets);
        // Check to see if this square is only defended by the king,
        // and that the queen is defended OK.
        Att = AttacksTo(B,sq);
        if (Count(Att&B->WhitePieces) == 1 && Count(Att&B->BlackPieces)>1) {
          if (QAttack) def--;
          else def = min(def-DEFENDED_Q_ATTACK,-DEFENDED_Q_ATTACK);
          QAttack = TRUE;
        }
        RemoveFirst(OppQTargets);
      }
      RemoveFirst(OppQueens);
    }    
# ifdef DEBUG_TACTICS
    if (olddef != def) fprintf(stdout,"QAtt %d, ", def-olddef);
# endif
#endif // DEFENDED_QUEEN_ATTACKS
  }


   /* Side specific stuff for a Black King */
  else {
     /* Get a rough idea of opponent's attacking strength */
    nopatt = B->wmaj*2 + B->wmin + 1;

#ifdef ADJACENT_PAWNS
     /* Bonus for adjacent pawns */
    def += Count(KingMoves[kpos]&B->BlackPawns);
# ifdef DEBUG_TACTICS
    fprintf(stdout,"P=%d, ",Count(KingMoves[kpos]&B->BlackPawns));
# endif
#endif // ADJACENT_PAWNS

    /* Penalty for being on an empty file.
     * ...but only if opp. has rook(s)/queen(s) and there are lots of pieces left. */
    if (B->Gamestage < Endgame && B->wmaj) {
      if (!(B->BlackPawns & FileDownMask[kpos])) {
        open=1;
        def -= HALF_OPEN_FILE_K; /*  King on half-open file */
        if (!(B->WhitePawns & FileDownMask[kpos])) {
          open=2;
          def -= OPEN_FILE_K; /*  ..or fully open file */
        }
#ifdef DEBUG_TACTICS
        if (open==2) fprintf(stdout,"OF, ");
        else fprintf(stdout,"SOF, ");
#endif
      }
    }

     /* Penalise if defending pawns are in fianchetto formation and the defending bishop is
      * not still on the board. */
#ifdef DEBUG_TACTICS
    olddef=def;
#endif
    if (ky == Rank8) {
      if (kx>FileE) {
        if (B->pieces[g7] != bpawn) {
          if (!(B->BlackBishops&BlackMask)) def--;
          if ((Mask[f6]|Mask[h6]) & (B->WhiteQueens | B->WhiteBishops | B->WhitePawns)) def--;
        }
      }
      else if (kx<FileD) {
        if (B->pieces[b7] != bpawn) {
          if (!(B->BlackBishops&WhiteMask)) def--;
          if ((Mask[a6]|Mask[c6]) & (B->WhiteQueens | B->WhiteBishops | B->WhitePawns)) def--;
        }
      }
    }
#ifdef DEBUG_TACTICS
    if (olddef!=def) fprintf(stdout,"Ftto/NoB, ");
    olddef = def;
#endif

    /* Get pawn shield */
    cancastle = (B->castle & 12);
    if (cancastle) { /* Preserve side file pawns if we can still castle */
      if (cancastle&4) { /* Can still castle king's side - this is preferable */
        if (B->BlackPawns & FileMask[FileF]) kshield++;
        if (B->BlackPawns & FileMask[FileG]) kshield++;
        if (B->BlackPawns & FileMask[FileH]) kshield++;
      }
      if (cancastle&8) { /* Can still castle queen's side */
        if (B->BlackPawns & FileMask[FileA]) qshield++;
        if (B->BlackPawns & FileMask[FileB]) qshield++;
        if (B->BlackPawns & FileMask[FileC]) qshield++;
      }
      // See which is best
      shield = max(kshield,qshield);
    }
    else { /* We can't castle - consider pawns in front of king only. px = centre of shield. */
      if (kx==FileH) px=FileG;
      else px = max(kx, FileB);
      if (B->BlackPawns & FileMask[px-1]) shield++;
      if (B->BlackPawns & FileMask[px]  ) shield++;
      if (B->BlackPawns & FileMask[px+1]) shield++;
    }

    /* Check for queen+rook attacks on adjoining, open files */
    if (kx>FileA && !(B->BlackPawns & FileDownMask[kpos-1]))
      def -= (2*Count(FileDownMask[kpos-1]&B->WhiteQueens) + Count(FileDownMask[kpos-1]&B->WhiteRooks)); 
    if (kx<FileH && !(B->BlackPawns & FileDownMask[kpos+1]))
      def -= (2*Count(FileDownMask[kpos+1]&B->WhiteQueens) + Count(FileDownMask[kpos+1]&B->WhiteRooks)); 
    if (!(B->BlackPawns & FileDownMask[kpos]))
      def -= (2*Count(FileDownMask[kpos]&B->WhiteQueens) + Count(FileDownMask[kpos]&B->WhiteRooks)); 
#ifdef DEBUG_TACTICS
    if (olddef!=def) fprintf(stdout,"FAtt %d, ",def-olddef);
    olddef = def;
#endif

    /* Check for pawn storms */
    PShield = PawnShieldMaskBlack[kpos] & B->BlackPawns;
    PStorm  = ((PShield<<7) & ~FileMask[FileH]) | ((PShield<<9) & ~FileMask[FileA]);
    PStorm |= ((PStorm&(~(B->All)))<<8);
    def -= Count(PStorm&B->WhitePawns);
#ifdef DEBUG_TACTICS
    if (olddef!=def) fprintf(stdout,"Storm %d, ",def-olddef);
    olddef = def;
#endif

    /* Beware of dangerous side-file attacks if enemy has rooks or queens
     * and the sides of the board are open */
    if (B->wmaj) {
      if (kx>=FileE && !((B->WhitePawns | B->BlackPawns)&FileMask[FileH])) {
        score -= SideAttack[B->Gamestage];
        if (FileMask[FileH] & (B->WhiteQueens | B->WhiteRooks)) score -= SideAttack[B->Gamestage];
      }
      if (kx<=FileD && !((B->WhitePawns | B->BlackPawns)&FileMask[FileA])) {
        score -= SideAttack[B->Gamestage];
        if (FileMask[FileA] & (B->WhiteQueens | B->WhiteRooks)) score -= SideAttack[B->Gamestage];
      }
#ifdef DEBUG_TACTICS
      if (score) fprintf(stdout,"Side %d, ",score);
#endif
    }
#ifdef COUNT_CHECKS
    /* Test for vulnerability to checking moves */
    Weakness = QueenMoves(B,kpos)&(~B->WhitePieces);
    Enemy = B->WhitePieces&(~B->WhitePawns);
    while (Enemy) {
      sq = FirstPiece(Enemy);
      switch(B->pieces[sq]) {
       case (wqueen): def -= Count(QueenMoves(B,sq)&Weakness); break;
       case (wrook): def -= Count(RookMoves(B,sq)&Weakness&RookMask[kpos]); break;
       case (wbishop): def -= Count(BishopMoves(B,sq)&Weakness&BishopMask[kpos]); break;
       case (wknight): if (DoubleKnightMove[sq]&Mask[kpos]) def--; break;
      }
      RemoveFirst(Enemy);
    }
#ifdef DEBUG_TACTICS
    if (olddef != def) fprintf(stdout,"NChk %d, ", olddef-def);
#endif
#endif

#ifdef DEFENDED_QUEEN_ATTACKS
    OppQueens = B->WhiteQueens;
    // Loop through opponent's queens
    while (OppQueens) {
      sq = FirstPiece(OppQueens);
      OppQTargets = QueenMoves(B,sq)&KingMoves[kpos];
      // Loop through the squares to which this queen can move
      // and which are next to your king
      while (OppQTargets) {
        tsq = FirstPiece(OppQTargets);
        // Check to see if this square is only defended by the king,
        // and that the queen is defended OK.
        Att = AttacksTo(B,sq);
        if (Count(Att&B->BlackPieces) == 1 && Count(Att&B->WhitePieces)>1) {
          if (QAttack) def--;
          else def = min(def-DEFENDED_Q_ATTACK,-DEFENDED_Q_ATTACK);
          QAttack = TRUE;
        }
        RemoveFirst(OppQTargets);
      }
      RemoveFirst(OppQueens);
    }    
# ifdef DEBUG_TACTICS
    if (olddef != def) fprintf(stdout,"QAtt %d, ", def-olddef);
# endif
#endif // DEFENDED_QUEEN_ATTACKS
  }
  
  /* Subtract penalties if your pawn shield is bad
   * This is also covered by considering the attack squares, but the idea of
   * a pawn shield is more of a forward-planning concept. */
#ifdef DEBUG_TACTICS
  fprintf(stdout,"Sh %d",shield);
  if (open==1) fprintf(stdout,"-");
  if (open==2) fprintf(stdout,"=");
  fprintf(stdout," ");
#endif
  // Don't penalise bad shields much if the opp. only has a bishop
  if (nopatt < 2) shield++;
  // Penalise for a bad pawn shield
  switch (shield) {
    case (0) : def -= SHIELD_ZERO; break;
    case (1) : def -= SHIELD_ONE;  break; 
    case (2) : def -= SHIELD_TWO;  break;
  }
  
   /* Scale the score.  Scale by more if the score is negative. */
  if (def<0) def *= nopatt;
  else def = (def * nopatt)>>1;
  score += def;

  /* Scale the score by the points left on the board */
  score = (score * min(B->WPts+B->BPts,78)) >> 6;
  
#ifdef DEBUG_TACTICS
  fprintf(stdout,"[%d]\n",score);
#endif

  return score;
}

/* Return a penalty score if the specified piece can be driven away by an
 * opposing pawn, and the opposing pawn will not be attacked by a defending pawn. */
int DriveAway(const int sq, const Board *B, const int side) {
  int y = Rank(sq), x=File(sq), sc=0;
  
  if (side==WHITE) {
    if (y<3) return 0;
    if (x>0 && B->pieces[sq-9]==empty && !(B->WhitePawns&PawnAttacksBlack[sq-9])) {
      if (B->pieces[sq-17]==bpawn) sc+=DRIVE_AWAY_SHORT;
      else if (y==4 && B->pieces[sq-25]==bpawn && B->pieces[sq-17]==empty) sc+=DRIVE_AWAY_LONG;
    }
    if (x<7 && B->pieces[sq-7]==empty && !(B->WhitePawns&PawnAttacksBlack[sq-7])) {
      if (B->pieces[sq-15]==bpawn) sc+=DRIVE_AWAY_SHORT;
      else if (y==4 && B->pieces[sq-23]==bpawn && B->pieces[sq-15]==empty) sc+=DRIVE_AWAY_LONG;
    }
  }
  else {
    if (y>4) return 0;
    if (x>0 && B->pieces[sq+7]==empty && !(B->BlackPawns&PawnAttacksWhite[sq+7])) {
      if (B->pieces[sq+15]==wpawn) sc+=DRIVE_AWAY_SHORT;
      else if (y==3 && B->pieces[sq+23]==wpawn && B->pieces[sq+15]==empty) sc+=DRIVE_AWAY_LONG;
    }
    if (x<7 && B->pieces[sq+9]==empty && !(B->BlackPawns&PawnAttacksWhite[sq+9])) {
      if (B->pieces[sq+17]==wpawn) sc+=DRIVE_AWAY_SHORT;
      else if (y==3 && B->pieces[sq+25]==wpawn && B->pieces[sq+17]==empty) sc+=DRIVE_AWAY_LONG;
    }
  }
#ifdef DEBUG_TACTICS
  if (sc) fprintf(stdout,"(DA %d), ",-sc);
#endif
  return sc;
}

/* Calculate the value of a passed pawn */
int ScorePassedPawn(int sq, Board *B, int *tpp) {
  BOOL ks=FALSE,safe=FALSE,Clear=FALSE, nopiece=FALSE;
  int p,y=Rank(sq),b=y,x=File(sq),side=WHITE,pdist,kdist,see_val,natt=0,Danger=DANGEROUS_PP;
  int score = 0, efsq = sq, bside,np;
#ifdef DEBUG_TACTICS
  int oldscore = 0;
#endif

  /* Get the piece ID and setup the advancement distance */
  p = B->pieces[sq];
  if (p<0) {b=7-b;side=BLACK;efsq = Flip[sq];}
  pdist = b;
  if (pdist>5) pdist=5; /* Pawn 2-square jump on first move */
  if (B->side != side) pdist++; /* Penalty for not being the side to move */
  
  /* This is by default NOT a totally passed pawn */
  *tpp=0;

  /* Count the pawns */
  np = Count(B->BlackPawns | B->WhitePawns);

  /* Reward for pushing forward */
  score = PassedPawn[efsq];
#ifdef DEBUG_TACTICS
  fprintf(stdout,"Pos %d, ",score);
#endif
  
  if (side==WHITE) { /* WHITE PAWN */
     /* Test to see if the promotion path is clear */
    if (!(FileUpMask[sq]&B->All)) {
      Clear = TRUE;
       /* Test for king safety */
      if (B->Gamestage >= LateMid) {
        if (Rank(B->BlackKing)<=pdist) {
          kdist = abs(File(B->BlackKing)-x);
          if (kdist>pdist) ks=TRUE;
           /* Check for interposing piece on the diagonal in borderline case */
          if (kdist==pdist) {
            if (x<File(B->BlackKing)) {
              if (DiagMaska8h1[B->BlackKing]&B->All&AboveMask[Rank(B->BlackKing)]) ks=TRUE;
            }
            else {
              if (DiagMaska1h8[B->BlackKing]&B->All&AboveMask[Rank(B->BlackKing)]) ks=TRUE;
            }
          }
        }
        /* If opp. king is behind pawn then reward automatically */
        else ks=TRUE;
      }
    }
    /* Test to see if we've trapped a black piece on the eighth rank */
    if (y == Rank7 && B->pieces[sq-8]<0) score += PieceValue10[PType(B->pieces[sq-8])];
#ifdef DEBUG_TACTICS
    if (y == Rank7 && B->pieces[sq-8]<0) 
      fprintf(stdout,"Trap %d, ",PieceValue10[PType(B->pieces[sq-8])]);
#endif
  }
  
  else { /* BLACK PAWN */
     /* Test to see if the promotion path is clear */
    if (!(FileDownMask[sq]&B->All)) {
      Clear = TRUE;
       /* Test for king safety if the path to promotion is clear */
      if (B->Gamestage >= LateMid) {
        if (Rank(B->WhiteKing)>=(7-pdist)) {
          kdist = abs(File(B->WhiteKing)-x);
          if (kdist>pdist) ks=TRUE;
           /* Check for interposing piece on the diagonal in borderline case */
          if (kdist==pdist) {
            if (x<File(B->WhiteKing)) {
              if (DiagMaska1h8[B->WhiteKing]&B->All&BelowMask[Rank(B->WhiteKing)]) ks=TRUE;
            }
            else {
              if (DiagMaska8h1[B->WhiteKing]&B->All&BelowMask[Rank(B->WhiteKing)]) ks=TRUE;
            }
          }
        }
        /* If opp. king is behind pawn then reward automatically */
        else ks=TRUE;
      }
    }
    /* Test to see if we've trapped a white piece on the first rank */
    if (y == Rank2 && B->pieces[sq+8]>0) score += PieceValue10[B->pieces[sq+8]];
#ifdef DEBUG_TACTICS
    if (y == Rank2 && B->pieces[sq+8]>0) 
      fprintf(stdout,"Trap %d, ",PieceValue10[B->pieces[sq+8]]);
#endif
  }

  /* Bonus for being safe from the opponent king */
  if (ks) {
#ifdef DEBUG_TACTICS
    fprintf(stdout,"Safe from King (%d), ",KingSafetyPP[np][pdist]);
#endif
    score += KingSafetyPP[np][pdist];
    PPKSMask |= Mask[sq];
  }
   /* Penalty for being blocked */
  else {
    if (side==WHITE && (FileUpMask[sq]&(B->BlackPieces|B->WhitePawns))) Danger >>= 1;
    if (side==BLACK && (FileDownMask[sq]&(B->WhitePieces|B->BlackPawns))) Danger >>= 1;
  }

#ifdef DEBUG_TACTICS
  oldscore = score;
#endif

  /* Check level of opposition */
  if (side==WHITE) {
     /* Bonus if supported */
    if (FileDownMask[sq]&B->WhiteRooks) score += RankBonus[y];
     /* Bonus if not challenged */
    if (!B->BlackRooks && !B->BlackQueens) {
      score += (2 + Clip(1 + Distance[sq][B->BlackKing] - Distance[sq][B->WhiteKing])) * KING_TROPISM;
      if (!B->BlackKnights && !B->BlackBishops) {
        nopiece = TRUE;
        score += RankBonus[b];
        /* No opponent pieces */
        if (ks || (KingMoves[sq]&Mask[B->WhiteKing] && Clear)) {safe=TRUE;*tpp=1;}
        else {
          score += DANGEROUS_PP;
          if (AttacksTo(B,sq)&B->WhitePieces) score += DANGEROUS_PP;
        }
      }
      else {
        /* Some minor opponent pieces - are they dangerous? */
        natt = ks ? 0 : 1;
        /* Knights attacking current sq. or promotion sq. */
        if (B->BlackKnights & KnightAttackPromotionW[sq]) natt++;
        /* See if a bishop can attack the promotion square */
        if (B->BlackBishops) {
          if (!(File(sq)&1) && (B->BlackBishops&WhiteMask)) natt++;
          if ((File(sq)&1) && (B->BlackBishops&BlackMask)) natt++;
        }
        switch(natt) {
          case 0 : score += Danger;  // Fall through
          case 1 : score += Danger;  // Fall through
          case 2 : score += Danger; break;
        }
      }
#ifdef DEBUG_TACTICS
      if (safe) fprintf(stdout,"Unchallenged (%d), ",score-oldscore);
      else fprintf(stdout,"Dangerous (%d), ",score-oldscore);
#endif
    }
  }
  else {
     /* Bonus if supported */
    if (FileUpMask[sq]&B->BlackRooks) score += RankBonus[b];
     /* Bonus if not challenged */
    if (!B->WhiteRooks && !B->WhiteQueens) {
      score += (2 + Clip(1 + Distance[sq][B->WhiteKing] - Distance[sq][B->BlackKing])) * KING_TROPISM;
      if (!B->WhiteKnights && !B->WhiteBishops) {
        nopiece=TRUE;
        score += RankBonus[b];
        /* No opponent pieces */
        if (ks || (KingMoves[sq]&Mask[B->BlackKing] && Clear)) {safe = TRUE;*tpp=1;}
        else {
          score += DANGEROUS_PP;
          if (AttacksTo(B,sq)&B->BlackPieces) score += DANGEROUS_PP;
        }
      }
      else {
        /* Some minor opponent pieces - are they dangerous? */
        natt = ks ? 0 : 1;
        /* Knights attacking current sq. or promotion sq. */
        if (B->WhiteKnights & KnightAttackPromotionB[sq]) natt++;
        /* See if a bishop can attack the promotion square */
        if (B->WhiteBishops) {
          if ((File(sq)&1) && (B->WhiteBishops&WhiteMask)) natt++;
          if (!(File(sq)&1) && (B->WhiteBishops&BlackMask)) natt++;
        }
        switch(natt) {
          case 0 : score += Danger;   // Fall through
          case 1 : score += Danger;   // Fall through
          case 2 : score += Danger; break;
        }
      }
#ifdef DEBUG_TACTICS
      if (safe) fprintf(stdout,"Unchallenged (%d), ",score-oldscore);
      else fprintf(stdout,"Dangerous (%d), ",score-oldscore);
#endif
    }
  }
   
   /* About to promote & promotion square is empty.
    * Test to see if the pawn is unstoppable. */
  if (!safe && b==1 && Clear) {
#ifdef DEBUG_TACTICS
    fprintf(stdout,"Almost Promoted, ");
#endif
    if (side==WHITE) {
      bside = B->side;
      B->side = side;
      /* See if the promotion square is unattacked */
      if ((AttacksTo(B,sq-8) & B->BlackPieces) == EMPTY) {
         /* Make sure there isn't an opposing rook/queen behind the pawn */
        if ((ReevaluateAttacks(B, EMPTY, sq, sq-8) & B->BlackPieces) == EMPTY) {
          if (B->side==WHITE) *tpp=1;
          else score += V_DANGEROUS_PP;
          safe = TRUE;
#ifdef DEBUG_TACTICS
          fprintf(stdout,"Unstoppable, ");
#endif
        }
      }
      /* Promotion square is attacked.  Do an SEE test */
      if (!safe) {
        see_val = SEE(B,sq,sq-8,queen);
        if (see_val > 0) {
           /* If this is a pawn endgame and the pawn is either (a) defended by its king
            * or (b) not attacked, then it's a winner */
          if (nopiece &&
             (!(KingMoves[sq]&Mask[B->BlackKing]) || (AttacksTo(B,sq) & B->WhitePieces))) {
            *tpp=1;
          }
           /* Otherwise, promoting this pawn still wins material.  Therefore give a bonus
            * proportional to the material gain */
          else {
            if (B->side == WHITE) score += (see_val * PAWN_SCORE * 4) / 5;
            else                  score += (see_val * PAWN_SCORE) / 2;
          }
        }
#ifdef DEBUG_TACTICS
        fprintf(stdout,"Promote SEE = %d, ",see_val);
#endif
      }
      B->side = bside;
    }
    if (side==BLACK) {
      bside = B->side;
      B->side = side;
      /* See if the promotion square is unattacked */
      if ((AttacksTo(B,sq+8) & B->WhitePieces) == EMPTY) {
         /* Make sure there isn't an opposing rook/queen behind the pawn */
        if ((ReevaluateAttacks(B, EMPTY, sq, sq+8) & B->WhitePieces) == EMPTY) {
          if (B->side == BLACK) *tpp=1;
          else score += V_DANGEROUS_PP;
          safe = TRUE;
#ifdef DEBUG_TACTICS
          fprintf(stdout,"Unstoppable, ");
#endif
        }
      }
      /* Promotion square is attacked.  Do an SEE test */
      if (!safe) {
        see_val = SEE(B,sq,sq+8,queen);
        if (see_val > 0) {
           /* If this is a pawn endgame and the pawn is either (a) defended by its king
            * or (b) not attacked, then it's a winner */
          if (nopiece && 
             (!(KingMoves[sq]&Mask[B->WhiteKing]) || (AttacksTo(B,sq) & B->BlackPieces))) {
            *tpp=1;
          }
           /* Otherwise promoting this pawn still wins material.  Therefore give a bonus
            * proportional to the material gain */
          else {
            if (B->side == BLACK) score += (see_val * PAWN_SCORE * 4) / 5;
            else                  score += (see_val * PAWN_SCORE) / 2;
          }
        }
#ifdef DEBUG_TACTICS
        fprintf(stdout,"Promote SEE = %d, ",see_val);
#endif
      }
      B->side = bside;
    }
  }
  
#ifdef DEBUG_TACTICS
  if (*tpp) fprintf(stdout,"TPP! ");
#endif
  return score;
}
