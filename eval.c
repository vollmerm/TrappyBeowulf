/**********************************
 *    eval.c                      *
 *    Colin Frayn                 *
 *    March 2001                  *
 **********************************/

/*
  This file contains all the functions for evaluating
  board positions.  Actual piece tactics are held in tactics.c
*/

#include <stdlib.h>
#include <stdio.h>

#include "common.h"
#include "eval.h"
#include "params.h"
#include "tactics.h"
#include "board.h"
#include "attacks.h"
#include "comp.h"
#include "probe.h"
#include "checks.h"

//#define ExchangeOff
//#define PAWN_STORM
//#define DEBUG_EVAL
//#define AsymmetryDebug
#define DRAWISH_ENDINGS
#define USE_EXTRA_MINOR

extern BITBOARD WhiteMask, BlackMask, Mask[64], FileMask[8], InvMask[64], PairMask[64];
extern BITBOARD Movesa1h8[64][256],Movesa8h1[64][256], FileUpMask[64], FileDownMask[64];
extern BITBOARD QueenMask[64],MovesRank[64][256],MovesFile[64][256], OffsideMask[8];
extern BITBOARD DiagMaska1h8[64],DiagMaska8h1[64], WhiteMask, BlackMask, KingMoves[64];
extern BITBOARD CentreMask, EdgeMask, FlankMask[64], RankMask[8],FlankMaskUp[64],FlankMaskDown[64];
extern int OffsidePenalty[9], OffsidePenalty2[9], OffsidePenalty3[9];
extern int PawnPosition[6][64], Distance[64][64];
extern int RookPosition[64],KnightPosition[64],BishopPosition[64],QueenPosition[64];
extern int ConnectedPP[8],ConnectedPP2[8],ConnectedPP3[8];
extern int DiagShifts_a1h8[64],DiagShifts_a8h1[64], PawnCount, Skill;
extern int DiagonalMask_a1h8[64],DiagonalMask_a8h1[64], SquareColour[64];
extern int Flip[64], Gamestage[256], ConvertSq[64], SpaceWon[6][64], SpaceDefended[6][64];
extern int CornerBoard[64],CentreBoard[64],CornerBoard2[64],CentreBoard2[64],CornerBoard3[64];
extern long int EvalCuts;
extern longlong Evalcount;
extern Board Current_Board;
extern BOOL TBON;
#ifdef DEBUG_EVAL
extern int PieceValue100[7];
#endif

/* Globals defined here for storing which pawns are passed or candidate passers.
 * PPKSMask = Passed Pawn King Safety Mask */
BITBOARD PassedMask, HiddenMask, PPKSMask;

/* Is there mating material for each side?  These are calculated in the LazyEval, and
 * reused in the full eval. 'prematscore' is used for keeping track of the score at the
 * end of LazyEval() before it is truncated due to lack of mating material.  This is in
 * case we need to do a full eval. */
BOOL wmat, bmat;
int prematscore;

/* Display the current board analysis and print it out neatly */
void Analyse(Board B) {
  int side = B.side, sc;
  double score;
  BOOL TB = FALSE;

  fprintf(stdout,"Current Position\n----------------\n\n");
  
  fprintf(stdout,"White Points: %d\n",B.WPts);
  fprintf(stdout,"Black Points: %d\n",B.BPts);
  if (B.WPts==B.BPts) fprintf(stdout,"(Even Sides");
  if (B.WPts<B.BPts) {
    fprintf(stdout,"(Black is Ahead by %d Pt",B.BPts-B.WPts);
    if (B.BPts-B.WPts>1) fprintf(stdout,"s");
  }
  if (B.WPts>B.BPts) {
    fprintf(stdout,"(White is Ahead by %d Pt",B.WPts-B.BPts);
    if (B.WPts-B.BPts>1) fprintf(stdout,"s");
  }
  fprintf(stdout,")\n");
  
   /* Probe the Endgame Tablebases (if any exist) and if this fails then
    * eval() the position */
  if (TBON && ProbeEGTB(&B,&sc,0)) {
    TB = TRUE;
  } 
  else sc = Eval(&B,-INFINITY,INFINITY);
  score = ((double)sc)/100.0;
  
  fprintf(stdout,"\nStatic Analysis Score\n");
  if (sc==0) fprintf(stdout,"Exactly Even Sides");
  if (sc>0)  {
    if (side==WHITE) fprintf(stdout,"White is Ahead by %.2f Pawns",score);
    else             fprintf(stdout,"Black is Ahead by %.2f Pawns",score);
  }
  if (sc<0) {
    if (side==WHITE) fprintf(stdout,"Black is Ahead by %.2f Pawns",-score);
    else             fprintf(stdout,"White is Ahead by %.2f Pawns",-score);
  }
  
  if (TB) fprintf(stdout,"  <TB>");
  fprintf(stdout,"\n\n");
}

/* The driver for the eval() function */
int Eval(Board *B, int alpha, int beta) {
#ifdef AsymmetryDebug
  CheckAsymmetry(B);
#endif
  Evalcount++;
  if (B->side==WHITE) return EvalMain(B,alpha,beta);
  return -EvalMain(B,-beta,-alpha);
}

/* Evaluate the current board position for the side stored in the board
 * structure. Score for white.  (Score for black = -1* this.)  We also pass the values
 * of alpha and beta to this routine on the expectation that we might be able to get a
 * cutoff if the score is particularly high or low without having to do the time consuming
 * part of the eval().  */
int EvalMain(Board *B, int alpha, int beta) {
  int sq,p,npw=0,tpts=B->WPts+B->BPts,sq2,def, score=0;
  int *SpDef, *SpWon, lazyscore;
  BITBOARD pieces = B->All;
  BOOL one_sided;
#ifdef DEBUG_EVAL
  int control = 0, oldscore = 0, contestcount = 0;
#endif
  
  /* Check for a drawn position */
  if (IsDrawnMaterial(B))
    return ((Current_Board.side == WHITE) ? (DRAW_SCORE) : -(DRAW_SCORE));
  
  /* Check if the game is theoretically won/lost */
  score = IsWonGame(B);
#ifdef DEBUG_EVAL
  if (score != 0) fprintf(stdout,"\nPosition Theoretically Won : %d\n\n",score);
#endif

  /* Check to see if we can just cutoff here - this score is so good that
   * we needn't bother working it out exactly - it's going to cause a cutoff. 
   * We have to be very careful because the score difference gained by doing
   * a proper eval here might be huge, therefore we only cutoff if this
   * position is theoretically won, and beta isn't, or it is theoretically
   * lost and alpha isn't. We can't just use standard futility cutoffs like
   * we do below, because in theoretically won positions, the score returned
   * by LazyEval() will almost always be much larger than EVAL_FUTILITY. */
  if (USE_EVAL_SC && score!=0 && ((score > 0 && beta<T_WIN_BOUND) ||
                                  (score < 0 && alpha>-(T_WIN_BOUND)))) {
    EvalCuts++;
#ifdef DEBUG_EVAL
    fprintf(stdout,"Early Cut [1] %d  (A=%d,B=%d)\n",score,alpha,beta);
#endif
    return score;
  }
  
   /* Get a lazy score evaluation
    * (material score & simple positional terms plus passed pawns) */
  lazyscore = LazyEval(B) + score;

   /* Check to see if we can just cutoff here.  The expectation is that the LazyEval
    * is alway within EVAL_FUTILITY of the true score.  Of course this isn't always
    * true, but we hope that it is true most of the time. */
  if (USE_EVAL_SC && (lazyscore > (beta+EVAL_FUTILITY) || lazyscore < (alpha-EVAL_FUTILITY))) {
    EvalCuts++;
#ifdef DEBUG_EVAL
    fprintf(stdout,"Early Cut [2] %d  (A=%d,B=%d)\n",lazyscore,alpha,beta);
#endif
    return lazyscore;
  } 

   /* We didn't get a cutoff so we have to be more careful and evaluate the board properly.
    * Begin with the LazyEval() score we obtained BEFORE any possible truncation due to lack
    * of mating material (we don't want to do this twice!). */
  score += prematscore;

#ifdef DEBUG_EVAL
  fprintf(stdout,"\nFull Eval\n---------\n");
#endif

   /* Generate arrays storing how many times each side attacks each square on the board.
    * This takes quite some time, but it is worth the effort! */
  GenerateAttacks(B);

   /* Now loop through all the pieces on the board and sum their contributions.
    * Also include the contributions of the empty spaces, measuring board
    * domination and territorial control.
    * There is the specialised version for the endgame below. */
  if (B->Gamestage < Endgame) {

     /* Work out how much square possession is worth */
    SpWon = SpaceWon[B->Gamestage];
    SpDef = SpaceDefended[B->Gamestage];
     /* Loop through the board */
    for (sq=0;sq<64;sq++) {
      p = B->pieces[sq];
      
#ifdef DEBUG_EVAL
      fprintf(stdout,"Square %c%d  [",File(sq)+97,8-Rank(sq));
      PrintPiece(p);
      fprintf(stdout,"] : ");
#endif
      switch(p) {
        /* White Piece */
      case (wpawn)  : score += TactPawn(sq,B,WHITE);
                      npw += SquareColour[sq]; break;
      case (wrook)  : score += TactRook(sq,B,WHITE); break;
      case (wknight): score += TactKnight(sq,B,WHITE); break;
      case (wbishop): score += TactBishop(sq,B,WHITE); break;
      case (wqueen) : score += TactQueen(sq,B,WHITE); break;
      case (wking)  : score += TactKing(sq,B,WHITE); break;
        /* Black Piece */
      case (bpawn)  : score -= TactPawn(sq,B,BLACK);
                      npw += SquareColour[sq]; break;
      case (brook)  : score -= TactRook(sq,B,BLACK); break;
      case (bknight): score -= TactKnight(sq,B,BLACK); break;
      case (bbishop): score -= TactBishop(sq,B,BLACK); break;
      case (bqueen) : score -= TactQueen(sq,B,BLACK); break;
      case (bking)  : score -= TactKing(sq,B,BLACK); break;
        /* Empty square - reward for possession by one side */
      case (empty)  : sq2=ConvertSq[sq];
#ifdef DEBUG_EVAL
        oldscore = score;
#endif
	    /* If only one side is attacking a square, then it is won
	     * by that side.  Otherwise it can only be defended. */
        one_sided = (!(B->WAttacks[sq2]) || !(B->BAttacks[sq2]));
        def = B->WAttacks[sq2] - B->BAttacks[sq2];
        if (one_sided) {
          if (def>0) score += SpWon[sq];
          else if (def<0) score -= SpWon[Flip[sq]];
        }
        else {
		      /* We have no clear winner, so evaluate the ownership of the square (slow!!) */
          def = EvaluateOwnership(B,sq);
          if (def > 0) score += SpDef[sq];
          else if (def < 0) score -= SpDef[Flip[sq]];
#ifdef DEBUG_EVAL
		  contestcount++;
#endif
        }
        
#ifdef DEBUG_EVAL
        control += score-oldscore;
        fprintf(stdout,"%d", score-oldscore);
#endif
        break;
      }
#ifdef DEBUG_EVAL
      fprintf(stdout,"\n");
#endif
    }
  }
    /* If we're in the endgame then use this simpler version */
  else {
     /* Loop through the pieces */
    while (pieces) {     
      sq = FirstPiece(pieces);
      p = B->pieces[sq];
#ifdef DEBUG_EVAL
      fprintf(stdout,"Square %c%d  [",File(sq)+97,8-Rank(sq));
      PrintPiece(p);
      fprintf(stdout,"] : ");
#endif

      switch(p) {
        /* White Piece */
      case (wpawn)  : score += TactPawn(sq,B,WHITE); break;
      case (wrook)  : score += TactRook(sq,B,WHITE); break;
      case (wknight): score += TactKnight(sq,B,WHITE); break;
      case (wbishop): score += TactBishop(sq,B,WHITE); break;
      case (wqueen) : score += TactQueen(sq,B,WHITE); break;
      case (wking)  : score += TactKing(sq,B,WHITE); break;
        /* Black Piece */
      case (bpawn)  : score -= TactPawn(sq,B,BLACK); break;
      case (brook)  : score -= TactRook(sq,B,BLACK); break;
      case (bknight): score -= TactKnight(sq,B,BLACK); break;
      case (bbishop): score -= TactBishop(sq,B,BLACK); break;
      case (bqueen) : score -= TactQueen(sq,B,BLACK); break;
      case (bking)  : score -= TactKing(sq,B,BLACK); break;
      }
      RemoveFirst(pieces);
#ifdef DEBUG_EVAL
    fprintf(stdout,"\n");
#endif
    }
  }
#ifdef DEBUG_EVAL
  fprintf(stdout,"After Piece Tactics : %d\n",score);
  fprintf(stdout,"Control Balance = %d\n",control);
  fprintf(stdout,"Contested Empty Squares : %d\n\n",contestcount);
#endif 
  
  /* Add on general positional score */
  if (B->Gamestage <= Middle) score += TacticsPositional(B);
  
  /* -- General score modifiers -- */
  
  /* Modifiers for pawns blocking bishops.  Your pawns should be on different squares to your
   * own bishops, but the same as your opponent's */

  /* White Bishops */
  if (B->WhiteBishops) {
    /* Only black square bishops */
    if (!(B->WhiteBishops & WhiteMask)) score += npw*PAWN_BLOCK;
    /* Only white square bishops */
    else if (!(B->WhiteBishops & BlackMask)) score -= npw*PAWN_BLOCK;
    /* Bonus for having two bishops (or more) on opposite colours */
    else score += TWO_BISHOPS;
#ifdef DEBUG_EVAL
    if (npw!=0) {
      if (!(B->WhiteBishops & WhiteMask)) fprintf(stdout,"White has only black square bishops : Pawn Block = %d\n",-npw*PAWN_BLOCK);
      else if (!(B->WhiteBishops & BlackMask)) fprintf(stdout,"White has only white square bishops : Pawn Block = %d\n",npw*PAWN_BLOCK);
    }
    if ((B->WhiteBishops & WhiteMask) && (B->WhiteBishops & BlackMask)) fprintf(stdout,"White has a bishop pair [+%d]\n",TWO_BISHOPS);
#endif
  }

  /* Black Bishops */
  if (B->BlackBishops) {
    /* Only black square bishops */
    if (!(B->BlackBishops & WhiteMask)) score -= npw*PAWN_BLOCK;
    /* Only white square bishops */
    else if (!(B->BlackBishops & BlackMask)) score += npw*PAWN_BLOCK;
    /* Penalty for opponent having two bishops (or more) on opposite colours */
    else score -= TWO_BISHOPS;
#ifdef DEBUG_EVAL
    if (npw!=0) {
      if (!(B->BlackBishops & WhiteMask)) fprintf(stdout,"Black has only black square bishops : Pawn Block = %d\n",npw*PAWN_BLOCK);
      else if (!(B->BlackBishops & BlackMask)) fprintf(stdout,"Black has only white square bishops : Pawn Block = %d\n",-npw*PAWN_BLOCK);
    }
    if ((B->BlackBishops & WhiteMask) && (B->BlackBishops & BlackMask)) fprintf(stdout,"Black has a bishop pair [-%d]\n",TWO_BISHOPS);
#endif
  }
  
  /* Penalty for having no pawns */
  if (!B->WhitePawns) score -= NO_PAWNS;
  if (!B->BlackPawns) score += NO_PAWNS;

  /* If there is no mating material for one side then the score cannot favour that side
   * (Note - we calculated this in the LazyEval and retained the results) */
  if (!wmat) score = min(0,score);
  if (!bmat) score = max(0,score); 
#ifdef DEBUG_EVAL
  if (!wmat) fprintf(stdout,"No mating material for white\n");
  if (!bmat) fprintf(stdout,"No mating material for black\n");
#endif 
#ifdef DRAWISH_ENDINGS
  /* Test for a 'drawish' ending, and reduce the score if so */
  if (Drawish(B)) score = (score * (50+tpts)) / 100;
#ifdef DEBUG_EVAL
  if (Drawish(B)) fprintf(stdout,"Drawish Position (score reduced)\n");
#endif
#endif  

#ifdef DEBUG_EVAL
  fprintf(stdout,"Final Score : %d   [Delta %d]\n",score,score-lazyscore);
#endif

   // Return score, possibly altered by the skill level
  if (Skill == 10) return score;
  return score + Random(((10-Skill) * 4) + 1) + 2 * Skill - 20;
}


/* LazyEval() basically just sums up the material score on the board. It also
 * flags and scores passed pawns.  It does this by remembering where the first
 * and last pawns of each colour in each file were and checking for pawns without
 * opposing blockers / flanking attackers. */
int LazyEval(Board *B) {
  BITBOARD pieces;
  int sq,p,Pawns[16],npawns=0,x, offside, wkx, bkx, dangerw=0, dangerb=0;
  int LastBlackPawn[8]={64,64,64,64,64,64,64,64},LastWhitePawn[8]={-1,-1,-1,-1,-1,-1,-1,-1};
  int score=0,tpts=B->BPts + B->WPts;
  int wmin=0,wmaj=0,bmin=0,bmaj=0;
#ifdef ExchangeOff
  int sdiff=0;
#endif
#ifdef DEBUG_EVAL
  int oldscore = 0, material=0;
#endif

  B->Gamestage = Gamestage[B->WPts + B->BPts];
  wmat = bmat = TRUE;

   /* Danger level of opposing pieces (for scoring connected PPs and offside majorities). */
  if (B->WhiteQueens) dangerb=2;
  else if (B->WhiteRooks && B->WhiteBishops) dangerb=1;
  if (B->BlackQueens) dangerw=2;
  else if (B->BlackRooks && B->BlackBishops) dangerw=1;

#ifdef DEBUG_EVAL
  fprintf(stdout,"Lazy Eval\n---------\n");
  fprintf(stdout,"Game Stage = %d   [0=Opening, 5=Late Endgame]\n",B->Gamestage);
#endif

  /* Loop through all pieces, adding up their basic scores */
  /* White pieces */
  pieces = B->WhitePawns;
  while (pieces) {
    sq = FirstPiece(pieces);
    score += PAWN_SCORE + PawnPosition[B->Gamestage][sq];
    LastWhitePawn[File(sq)] = sq;
    Pawns[npawns++] = sq;
    RemoveFirst(pieces);
  }
  pieces = B->WhiteRooks;
  while (pieces) {
    sq = FirstPiece(pieces);
    score += ROOK_SCORE + RookPosition[sq];
    wmaj++;
    RemoveFirst(pieces);
  }
  pieces = B->WhiteKnights;
  while (pieces) {
    sq = FirstPiece(pieces);
    score += KNIGHT_SCORE + KnightPosition[sq];
    wmin++;
    RemoveFirst(pieces);
  }
  pieces = B->WhiteBishops;
  while (pieces) {
    sq = FirstPiece(pieces);
    score += BISHOP_SCORE + BishopPosition[sq];
     /* Penalise for getting trapped on a7 or h7 */
    if (sq==a7 && B->pieces[b6]==bpawn) score -= BISHOP_TRAPPED;
    if (sq==h7 && B->pieces[g6]==bpawn) score -= BISHOP_TRAPPED;
    wmin++;
    RemoveFirst(pieces);
  }
  pieces = B->WhiteQueens;
  while (pieces) {
    sq = FirstPiece(pieces);
    score += QUEEN_SCORE + QueenPosition[sq];
    /* Penalty for advancing too far too early */
    if (B->Gamestage <= Middle && Rank(sq)<Rank5)
      score -= OVERSTRETCHED * (1 + Middle - B->Gamestage);
    /* Penalty for advancing before minor pieces */
    if (B->Gamestage==Opening && Rank(sq)!=Rank1) {
      score -= (EARLY_QUEEN*Count((B->WhiteBishops | B->WhiteKnights) & RankMask[Rank1]));
    }
     /* Bonus for having a queen in the endgame */
    if (tpts<40) score += (40-tpts) * LATE_QUEEN_BONUS;
    wmaj += 2;
    RemoveFirst(pieces);
  }

  /* Black pieces */
  pieces = B->BlackPawns;
  while (pieces) {
    sq = FirstPiece(pieces);
    score -= (PAWN_SCORE + PawnPosition[B->Gamestage][(Flip[sq])]);
    if (LastBlackPawn[File(sq)] == 64)
      LastBlackPawn[File(sq)] = sq;
    Pawns[npawns++] = sq;
    RemoveFirst(pieces);
  }
  pieces = B->BlackRooks;
  while (pieces) {
    sq = FirstPiece(pieces);
    score -= (ROOK_SCORE + RookPosition[(Flip[sq])]);
    bmaj++;
    RemoveFirst(pieces);
  }
  pieces = B->BlackKnights;
  while (pieces) {
    sq = FirstPiece(pieces);
    score -= (KNIGHT_SCORE + KnightPosition[(Flip[sq])]);
    bmin++;
    RemoveFirst(pieces);
  }
  pieces = B->BlackBishops;
  while (pieces) {
    sq = FirstPiece(pieces);
    score -= (BISHOP_SCORE + BishopPosition[(Flip[sq])]);
     /* Penalise for getting trapped on a2 or h2 */
    if (sq==a2 && B->pieces[b3]==wpawn) score += BISHOP_TRAPPED;
    if (sq==h2 && B->pieces[g3]==wpawn) score += BISHOP_TRAPPED;
    bmin++;
    RemoveFirst(pieces);
  }
  pieces = B->BlackQueens;
  while (pieces) {
    sq = FirstPiece(pieces);
    score -= (QUEEN_SCORE + QueenPosition[(Flip[sq])]);
    /* Penalty for advancing too far too early */
    if (B->Gamestage <= Middle && Rank(sq) > Rank4) 
      score += OVERSTRETCHED * (1 + Middle - B->Gamestage);
    /* Penalty for advancing before minor pieces */
    if (B->Gamestage==Opening && Rank(sq)!=Rank8) {
      score -= (EARLY_QUEEN*Count((B->BlackKnights | B->BlackBishops) & RankMask[Rank8]));
    }
     /* Bonus for having a queen in the endgame */
    if (tpts<40) score -= (40-tpts) * LATE_QUEEN_BONUS;
    bmaj += 2;
    RemoveFirst(pieces);
  }

   /* King locations */
  switch (B->Gamestage) {
    case (Opening)  : score += CornerBoard3[B->WhiteKing] - CornerBoard3[B->BlackKing]; break;
    case (EarlyMid) : score += CornerBoard2[B->WhiteKing] - CornerBoard2[B->BlackKing]; break;
    case (Middle)   : score += CornerBoard2[B->WhiteKing] - CornerBoard2[B->BlackKing]; break;
    case (LateMid)  : score += CornerBoard[B->WhiteKing]  - CornerBoard[B->BlackKing];  break;
    case (Endgame)  : score += CentreBoard[B->WhiteKing]  - CentreBoard[B->BlackKing];  break;
    case (LateEnd)  : score += CentreBoard2[B->WhiteKing] - CentreBoard2[B->BlackKing]; break;
  }
  wkx = File(B->WhiteKing);
  bkx = File(B->BlackKing);
  // Store piece counts
  B->wmaj = wmaj;
  B->wmin = wmin;
  B->bmaj = bmaj;
  B->bmin = bmin;
  
#ifdef DEBUG_EVAL
  for (sq=0;sq<64;sq++) {
    if (B->pieces[sq]>0) material += PieceValue100[(B->pieces[sq])];
    else material -= PieceValue100[-(B->pieces[sq])];
  }
  fprintf(stdout,"\nMaterial Eval : %d\n",material);
  fprintf(stdout,"Positional Eval : %d\n",score-material);
#endif

   /* King Safety */
  if ((bmaj*2+bmin)>3) {
#ifdef DEBUG_EVAL
    fprintf(stdout,"White King Defence : ");
#endif
    score += KingDefence(B,B->WhiteKing,WHITE);
  }
  if ((wmaj*2+wmin)>3) {
#ifdef DEBUG_EVAL
    fprintf(stdout,"Black King Defence : ");
#endif
    score -= KingDefence(B,B->BlackKing,BLACK);
  }


   /* Reset Passed Pawn Bitboards */
  PassedMask = HiddenMask = EMPTY;
   /* Cycle through the pawns we found earlier.  Check for passed pawns. */
  if (B->Gamestage != Opening) {
    for (p=0;p<npawns;p++) {
      sq = Pawns[p];
      x = File(sq);
      if (B->pieces[sq]==wpawn) {
        if (LastBlackPawn[x]>sq &&
          (x==0 || LastBlackPawn[x-1]>=(sq-1)) &&
          (x==7 || LastBlackPawn[x+1]>sq)) {
          PassedMask |= Mask[sq];
          if (FileDownMask[sq] & B->WhiteRooks) score += ROOK_BEHIND_PP;
        }
        /* Check for disguised passed pawns, where only one of a pair of connected pawns
         * is blocked by an opposing pawn.  Clearly, one of the two will be able to convert
         * to a passer, provided the pawn on the open file has no attackers on its other
         * side.  Only consider pawns where the blocker is on its 3rd rank or less.  */
        else if (Rank(sq)<Rank4 && Mask[sq-8]&B->BlackPawns && !(FileUpMask[sq-8]&B->BlackPawns) && 
          ((x>0 && B->pieces[sq+7]==wpawn && !(FileUpMask[sq+7]&B->BlackPawns) && 
           (x==1 || !(FileUpMask[sq+6]&B->BlackPawns)) ||
          (x<7 && B->pieces[sq+9]==wpawn && !(FileUpMask[sq+9]&B->BlackPawns) &&
           (x==6 || !(FileUpMask[sq+10]&B->BlackPawns)))))) {
          HiddenMask |= Mask[sq];
        }
      }
      else {    
        if (LastWhitePawn[x]<sq &&
          (x==0 || LastWhitePawn[x-1]<sq) &&
          (x==7 || LastWhitePawn[x+1]<=(sq+1))) {
          PassedMask |= Mask[sq];
          if (FileUpMask[sq] & B->BlackRooks) score -= ROOK_BEHIND_PP;
        }
          /* Check for disguised passed pawns, where only one of a pair of connected pawns
           * is blocked by a opposing pawn.  Clearly, one of the two will be able to convert
           * to a passer, provided the pawn on the open file has no attackers on its other
           * side.   Only consider pawns where the blocker is on its 3rd rank or less.  */
        else if (Rank(sq)>Rank5 && Mask[sq+8]&B->WhitePawns && !(FileDownMask[sq+8]&B->WhitePawns) &&
          ((x>0 && B->pieces[sq-9]==bpawn && !(FileDownMask[sq-9]&B->WhitePawns) && 
          (x==1 || !(FileDownMask[sq-10]&B->WhitePawns)) ||
          (x<7 && B->pieces[sq-7]==bpawn && !(FileDownMask[sq-7]&B->WhitePawns) &&
          (x==6 || !(FileDownMask[sq-6]&B->WhitePawns)))))) {
          HiddenMask |= Mask[sq];
        }
      }
    }
  }
  
  /* Encourage Trade Off if Ahead */
#ifdef ExchangeOff
  if (B->WPts != B->BPts) {
    /* Calculate half the sum of the scores of pieces captured so far this game,
     * and subtract it from the loser's score.  Hence the winner is encouraged to
     * trade off to lower total scores. */
    sdiff = (78 - (B->WPts + B->BPts)) >> 1;
    if (B->WPts > B->BPts) score += sdiff;
    else                   score -= sdiff;
#ifdef DEBUG_EVAL
    if (B->WPts > B->BPts) fprintf(stdout,"Trade Off score : %d\n",sdiff);
    else fprintf(stdout,"Trade Off score : %d\n",-sdiff);
#endif
  }
#endif
  
   /* Evaluate Passed Pawns */
  if (PassedMask|HiddenMask) score += EvalPassedPawns(B,dangerw,dangerb);
#ifdef DEBUG_EVAL
  oldscore = score;
#endif

  /* Extra minor pieces in the endgame really help.  Q or 2R vs.
   * 3 minors is usually bad.  Rook vs. 2 minors is bad.  Much of
   * this code is based on that of Crafty v18.12, by Prof. R. Hyatt.
   * I added in some clauses of my own.  In particular, I only 
   * apply this bonus before the late endgame. */
#ifdef USE_EXTRA_MINOR
  if (B->Gamestage < LateEnd) {
    /* Count number of minors and majors for each side */
    if (wmin != bmin) {
      switch(abs(wmaj-bmaj)) {
      case 0:
        if (wmin > bmin) score += EXTRA_MINOR;
        else score -= EXTRA_MINOR;
        break;
      case 1:
        if (bmaj==wmaj+1 && wmin > bmin+1) score += EXTRA_MINOR;
        else if (wmaj==bmaj+1 && bmin > wmin+1) score -= EXTRA_MINOR;
        break;
      case 2:
        if (bmaj==wmaj+2 && wmin > bmin+2) score += EXTRA_MINOR;
        else if (wmaj==bmaj+2 && bmin > wmin+2) score -= EXTRA_MINOR;
        break;
      }
    }
#ifdef DEBUG_EVAL
    if (oldscore != score) fprintf(stdout,"Extra Minor (%d)\n",score-oldscore);
    oldscore = score;
#endif
  }
#endif  

  /* Test for an offside pawn majority for each side.  This is when the opposition
   * has a pawn majority on the opposite side of the board to your king.  We define
   * 'offside' as any file more than 2 distant from the king. This is penalised. */
  offside = Count(B->WhitePawns&OffsideMask[wkx]) - Count(B->BlackPawns&OffsideMask[wkx]);
  if (offside<0) {
    switch (dangerw) {
     case (2): score -= OffsidePenalty[-offside]; break;
     case (1): score -= OffsidePenalty2[-offside]; break;
     case (0): score -= OffsidePenalty3[-offside]; break;
    }
  }
  offside = Count(B->BlackPawns&OffsideMask[bkx]) - Count(B->WhitePawns&OffsideMask[bkx]);
  if (offside<0) {
    switch (dangerb) {
     case (2): score += OffsidePenalty[-offside]; break;
     case (1): score += OffsidePenalty2[-offside]; break;
     case (0): score += OffsidePenalty3[-offside]; break;
    }
  }
#ifdef DEBUG_EVAL
  if (score!=oldscore) fprintf(stdout,"Offside Pawns : %d\n",score-oldscore);
#endif

  /* If there is no mating material for one side then the score cannot favour that side.
   * Make sure we keep track of the score before truncation, in case we have to do a
   * full eval here. */
  prematscore = score;
  if (bmaj==0 && wmaj==0) CheckMatingMaterial(B);
  if (!wmat) score = min(0,score);
  if (!bmat) score = max(0,score); 
#ifdef DEBUG_EVAL
  if (!wmat) fprintf(stdout,"No mating material for white\n");
  if (!bmat) fprintf(stdout,"No mating material for black\n");
#endif 
#ifdef DRAWISH_ENDINGS
  /* Test for a 'drawish' ending, and reduce the score if so */
  if (Drawish(B)) score = (score * (50+B->WPts+B->BPts)) / 100;
#ifdef DEBUG_EVAL
  if (Drawish(B)) fprintf(stdout,"Drawish Position (score reduced)\n");
#endif
#endif

#ifdef DEBUG_EVAL
  fprintf(stdout,"\nTotal Lazy Eval : %d\n",score);
#endif
  return score;
}

/* Evaluate and score passed pawns as quickly as possible.
 * Danger parameters record how much opposition there is. */
int EvalPassedPawns(Board *B, int dangerw, int dangerb) {
  BITBOARD pieces=PassedMask;
  int sq,p,x,y,bestw=8,bestb=0,bestwx=-1,bestbx=-1,modifier=0;
  int diff,firstq,qsq,ksq,qx,qy,qsq2,ksq2,qx2,qy2,whiteadv=0,tpp=0,cppsc=0;
  int nwpp=0,nbpp=0, s, wppmod=0,bppmod=0,wpieces=0, bpieces=0, score=0;
  BITBOARD kingmask, mask, moves;
  BOOL capture;
#ifdef DEBUG_EVAL
  int oldscore;
#endif
  
   /* Store the pawns which are safe from the opp. king, for later use */
  PPKSMask = EMPTY;
 
   /* Loop through the passed pawns */
  while (pieces) {
    sq = FirstPiece(pieces);
    p = B->pieces[sq];
    x = File(sq);
    y = Rank(sq);
#ifdef DEBUG_EVAL
    fprintf(stdout,"Passed Pawn %c%d : ",x+97, 8-y);
    oldscore = score;
#endif    
    switch(p) {
    case (wpawn)  :
      score += (s = ScorePassedPawn(sq,B,&tpp));
      if (!(FileUpMask[sq]&PassedMask)) whiteadv++;
      wppmod += s/2;
       /* Test if this is totally passed, and if so store the distance to promotion */
      if (tpp) {
        nwpp++;
        if (y<bestw) {bestw=y;bestwx=x;}
      }
       /* Connected? */
      if (FlankMask[sq]&B->WhitePawns) {
         /* Reward depending on level of danger and the separation.
          * First check for pawns on adjacent files touching this one */
        if (PairMask[sq]&B->WhitePawns) {
          modifier = 0;
          switch (dangerw) {
            case (0) : cppsc = ConnectedPP3[y]; break;
            case (1) : cppsc = ConnectedPP2[y]; break;
            case (2) : cppsc = ConnectedPP[y]; break;
          }
          // Check that we're not challenged
          if (!(FileUpMask[sq]&B->BlackPawns)) {
            modifier++;
            if (!(FlankMaskUp[sq]&B->BlackPawns)) modifier++;
          }
          /* The touching pawn is also passed - add an extra bonus  */
          if ((PairMask[sq]&B->WhitePawns)&PassedMask) modifier++;
          cppsc = (cppsc*(modifier+4))/4;
          score += cppsc;
          wppmod += (cppsc - ConnectedPP[y]);
        }
        else score += DEFENDED_PAWN;
#ifdef DEBUG_EVAL
        fprintf(stdout,"Connected (%d) ",score-oldscore-s);
#endif 
      }
#ifdef DEBUG_EVAL
      fprintf(stdout,"[%d]\n",score-oldscore);
#endif 
      break;        
    case (bpawn)  :
      score -= (s = ScorePassedPawn(sq,B,&tpp));
      if (!(FileDownMask[sq]&PassedMask)) whiteadv--;
      bppmod += s/2;
       /* Test if this is totally passed, and if so store the distance to promotion */
      if (tpp) {
        nbpp++;
        if (y>bestb) {bestb=y;bestbx=x;}
      }
       /* Connected? */
      if (FlankMask[sq]&B->BlackPawns) {
         /* Reward depending on level of danger and the separation.
          * First check for pawns on adjacent files touching this one */
        if (PairMask[sq]&B->BlackPawns) {
          modifier = 0;
          switch (dangerb) {
            case (0) : cppsc = ConnectedPP3[7-y]; break;
            case (1) : cppsc = ConnectedPP2[7-y]; break;
            case (2) : cppsc = ConnectedPP[7-y]; break;
          }
          // Check that we're not challenged
          if (!(FileDownMask[sq]&B->WhitePawns)) {
            modifier++;
            if (!(FlankMaskDown[sq]&B->WhitePawns)) modifier++;
          }
          /* The touching pawn is also passed - add an extra bonus  */
          if ((PairMask[sq]&B->BlackPawns)&PassedMask) modifier++;
          cppsc = (cppsc*(modifier+4))/4;
          score -= cppsc;
          bppmod += (cppsc - ConnectedPP[7-y]);
        }
        else score -= DEFENDED_PAWN;
#ifdef DEBUG_EVAL
        fprintf(stdout,"Connected (%d) ",oldscore-score-s);
#endif 
      }
#ifdef DEBUG_EVAL
      fprintf(stdout,"[%d]\n",oldscore-score);
#endif 
      break;
    }
    RemoveFirst(pieces);
  }  
  
   /* Loop through the hidden passed pawns.  Consider them as if the blocking pawn
    * in front was not there. */
  pieces = HiddenMask;
  while (pieces) {
    sq = FirstPiece(pieces);
    p = B->pieces[sq];
    x = File(sq);
    y = Rank(sq);
#ifdef DEBUG_EVAL
    fprintf(stdout,"Hidden Passed Pawn %c%d : ",x+97, 8-y);
    oldscore = score;
#endif    
    switch(p) {
    case (wpawn)  :
      B->All ^= Mask[sq-8];
      B->BlackPieces ^= Mask[sq-8];
      s = ScorePassedPawn(sq,B,&tpp);
      if (B->BlackRooks||B->BlackQueens) s >>= 1;
      if (B->BlackBishops||B->BlackKnights) s >>= 1;
      score += s;
      B->All ^= Mask[sq-8];
      B->BlackPieces ^= Mask[sq-8];
       /* Test if this is totally passed, and if so store the distance to promotion */
      if (tpp) {
        nwpp++;
        if (y<bestw) {bestw=y;bestwx=x;}
      }
#ifdef DEBUG_EVAL
      fprintf(stdout,"[%d]\n",score-oldscore);
#endif 
      break;
    case (bpawn)  :
      B->All ^= Mask[sq+8];
      B->WhitePieces ^= Mask[sq+8];
      s = ScorePassedPawn(sq,B,&tpp);
      if (B->WhiteRooks||B->WhiteQueens) s >>= 1;
      if (B->WhiteBishops||B->WhiteKnights) s >>= 1;
      score -= s;
      B->All ^= Mask[sq+8];
      B->WhitePieces ^= Mask[sq+8];
       /* Test if this is totally passed, and if so store the distance to promotion */
      if (tpp) {
        nbpp++;
        if (y>bestb) {bestb=y;bestbx=x;}
      }
#ifdef DEBUG_EVAL
      fprintf(stdout,"[%d]\n",oldscore-score);
#endif 
      break;
    }
    RemoveFirst(pieces);
  }  

#ifdef PAWN_STORM
   /* Bonus if one side has more passers than the other */
  score += whiteadv*PP_STORM;
#ifdef DEBUG_EVAL
  if (whiteadv) fprintf(stdout,"Unbalanced Passed Pawn Storm %d\n",whiteadv*PP_STORM);
#endif
#endif

   /* See who wins promotion races if both sides have totally passed pawns */
  if (bestbx>-1 && bestwx>-1) {
    bestb = 7-bestb;
     /* Sort out queening distances for pawns with initial 2-square moves */
    if (bestb == 6) bestb = 5;
    if (bestw == 6) bestw = 5;
     /* Work out who is winning the race */
    diff = bestb-bestw;
     /* White wins */
    if ((diff > 1) || (diff==1 && B->side==WHITE)) {
      nbpp=0;
      score += bppmod;
#ifdef DEBUG_EVAL
      fprintf(stdout,"White wins promotion race\n");
#endif
    }
     /* Black wins */
    else if ((diff < -1) || (diff==-1 && B->side==BLACK)) {
      nwpp=0;
      score -= wppmod;
#ifdef DEBUG_EVAL
      fprintf(stdout,"Black wins promotion race\n");
#endif
    }
     /* It's a draw so test to see if the side to queen first gives check */
    else {
      firstq=B->side;
      if (diff) firstq= Opponent(firstq);
       /* White queens first */
      if (firstq == WHITE) {
        qsq = qx = bestwx;
        qsq2 = bestbx + a1;
        qx2 = bestbx;
        ksq = B->BlackKing;
        ksq2 = B->WhiteKing;
        qy = Rank8;
        qy2 = Rank1;
      }
       /* Black queens first */
      else {
        qsq = bestbx + a1;
        qx = bestbx;
        qsq2 = qx2 = bestwx;
        ksq = B->WhiteKing;
        ksq2 = B->BlackKing;
        qy = Rank1;
        qy2 = Rank8;
      }
       /* Test for giving check */
      kingmask = Mask[ksq];
      moves = EMPTY;
      if (kingmask & QueenMask[qsq]) {
        mask = (B->All >> (qy*8)) & FullRank;
        moves = MovesRank[qsq][mask];
        mask = (B->R90 >> (qx*8)) & FullRank;
        moves |= MovesFile[qsq][mask];
        mask = ((B->R45 >> DiagShifts_a1h8[qsq]) & DiagonalMask_a1h8[qsq]);
        moves |= Movesa1h8[qsq][mask];
        mask = ((B->L45 >> DiagShifts_a8h1[qsq]) & DiagonalMask_a8h1[qsq]);
        moves |= Movesa8h1[qsq][mask];
        moves &= kingmask;
        if (moves != EMPTY) {
          if (firstq==WHITE) nbpp=0;
          else               nwpp=0;
#ifdef DEBUG_EVAL
          fprintf(stdout,"First promoting pawn gives check therefore wins race\n");
#endif
        }
      }
       /* If the winner didn't give check then test the losing pawn, but only
        * if the first queen can't capture it. */
      if (moves == EMPTY) {
        capture = FALSE;
        // Test to see if the first queen will capture the second
        if (qx==qx2 || ((qsq+qsq2 == a1+h8) && (qx==FileA || qx==FileH))) {
          // Same file
          if (qx==qx2) {
            if ((FileMask[qx] & B->All & InvMask[bestw] & InvMask[bestb]) == EMPTY)
              capture = TRUE;
          }
          // Same diagonal
          else {
            if (qsq==a1 || qsq==h8) {
              if ((DiagMaska1h8[qsq] & B->All) == EMPTY) capture = TRUE;
           }
            else {
              if ((DiagMaska8h1[qsq] & B->All) == EMPTY) capture = TRUE;
            }
          }
          // Yes, the first queen _can_ capture the second
          if (capture == TRUE) {
            // Make sure the second queening square isn't defended
            if (firstq==WHITE) {
              if (!(AttacksTo(B,qsq2)&B->BlackPieces)) {nbpp=0;score += bppmod;}
            }
            else {
              if (!(AttacksTo(B,qsq2)&B->WhitePieces)) {nwpp=0;score -= wppmod;}
            }
#ifdef DEBUG_EVAL
            fprintf(stdout,"First promoting pawn captures the second\n");
#endif
          }
        }
         // If the second queen is safe and the first queen doesn't give check
         // then test to see if this one gives check
        if (capture == FALSE) {
          kingmask = Mask[ksq2];
          if (kingmask & QueenMask[qsq2]) {
            mask = (B->All >> (qy2*8)) & FullRank;
            moves = MovesRank[qsq2][mask];
            mask = (B->R90 >> (qx2*8)) & FullRank;
            moves |= MovesFile[qsq2][mask];
            mask = ((B->R45 >> DiagShifts_a1h8[qsq2]) & DiagonalMask_a1h8[qsq2]);
            moves |= Movesa1h8[qsq2][mask];
            mask = ((B->L45 >> DiagShifts_a8h1[qsq2]) & DiagonalMask_a8h1[qsq2]);
            moves |= Movesa8h1[qsq2][mask];
            if (moves & kingmask) {
              if (firstq==WHITE) score -= DANGEROUS_PP;
              else               score += DANGEROUS_PP;
#ifdef DEBUG_EVAL
              fprintf(stdout,"Second promoting pawn gives check therefore gains advantage\n");
#endif
            }
          }
        }
      }
    }
  }

   /* Add on the unstoppable pawns (after checking for pawn races) */
  if (nwpp!=nbpp) {
    score += (nwpp-nbpp) * UNSTOPPABLE_PP;
    if (nbpp==0) {
      score += bppmod;
      if (B->BlackQueens || B->BlackRooks || B->BlackBishops || B->BlackKnights) bpieces=1;
      else score += bppmod;
    }
    if (nwpp==0) {
      score -= wppmod;
      if (B->WhiteQueens || B->WhiteRooks || B->WhiteBishops || B->WhiteKnights) wpieces=1;
      else score -= wppmod;
    }
  }

#ifdef DEBUG_EVAL
  if (nwpp!=nbpp) {
    fprintf(stdout,"Unstoppable PP Balance = %d\n",(nwpp-nbpp) * UNSTOPPABLE_PP);
    if (wpieces==0) wppmod*=2;
    if (bpieces==0) bppmod*=2;
    if (nbpp==0) fprintf(stdout,"White TPP Penalises Black PPs = %d\n",bppmod);
    if (nwpp==0) fprintf(stdout,"Black TPP Penalises White PPs = %d\n",-wppmod);
  }
  fprintf(stdout,"Passed Pawns [W%d B%d] : Total Score %d\n",Count((HiddenMask|PassedMask)&B->WhitePawns),
                  Count((HiddenMask|PassedMask)&B->BlackPawns),score);
#endif
  return score;
}

/* Check to see if a game is theoretically won */
int IsWonGame(Board *B) {
  if (B->WPts>0 && B->BPts>0) return 0;
  if (B->WPts==0) {
    if (B->BlackQueens || B->BlackRooks) return -(THEORETICAL_WIN);
    if ((B->BlackBishops & BlackMask) && (B->BlackBishops & WhiteMask))
      return -(THEORETICAL_WIN);
  }
  else if (B->BPts==0) {
    if (B->WhiteQueens || B->WhiteRooks) return (THEORETICAL_WIN);
    if ((B->WhiteBishops & WhiteMask) && (B->WhiteBishops & BlackMask))
      return (THEORETICAL_WIN);
  }
  return 0;
}

/* Check if a board position is a draw (theoretically) */
BOOL IsDrawnMaterial(Board *B) {
  int tpts = B->WPts + B->BPts;

  if (tpts > 13) return FALSE;
  if (!B->WhitePawns && !B->BlackPawns) {
    if (B->WPts<4 && B->BPts<4)
      return TRUE; /* Only at most one minor each side on the board */
    if (tpts == 6) {
      if (B->WPts == 6 && !B->WhiteBishops)
        return TRUE;    /* Must be 2 knights -- drawn */
      if (B->BPts == 6 && !B->BlackBishops)
        return TRUE;    /* Must be 2 knights -- drawn */
    }
    if (tpts == 9 && B->BlackKnights && B->WhiteKnights)
        return TRUE;    /* Must be NB vs N or NN vs N -- drawn */
    if (tpts == 13 && B->BlackRooks && B->WhiteRooks)
        return TRUE;    /* Must be RB vs R or RN vs R -- drawn */
  }
  return FALSE;
}

/* Check to see if each side has mating material */
void CheckMatingMaterial(Board *B) {
  int wpieces=B->WPts-Count(B->WhitePawns),bpieces=B->BPts-Count(B->BlackPawns);
  int wkingdist, bkingdist;

   /* Firstly, test for white */
  if (B->WPts == 0) wmat = FALSE;
  else if (wpieces < 7) do {
    if (wpieces==3 && !B->WhitePawns) {wmat = FALSE; break;}
    if (wpieces==6 && !B->WhitePawns && !B->WhiteBishops) {wmat = FALSE;break;}
     /* Bad rook pawn(s) & bishop combination */
    if (wpieces<=3 && !B->WhiteKnights && (B->WhitePawns & EdgeMask)) {
       /* We have (at most) one bishop and several pawns */
       /* See if we have non-rook pawns */
      if (B->WhitePawns & CentreMask) break;
       /* See if we have rook pawns on both A and H files */
      if ((B->WhitePawns & FileMask[FileA]) && (B->WhitePawns & FileMask[FileH])) break;
       /* See if either of these two rook pawns are safe from the opp. king, therefore will queen */ 
      if (B->WhitePawns & PPKSMask) break;
       /* We only have an A-file pawn plus a dark-square bishop (or no pieces) */
      if ((B->WhitePawns & FileMask[FileA]) && !(B->WhiteBishops & WhiteMask)) {
        wkingdist = Distance[B->WhiteKing][a8] - (B->side==WHITE ? 1 : 0);
        bkingdist = Distance[B->BlackKing][a8];
        if (bkingdist < wkingdist) wmat = FALSE;
      }
       /* We only have an H-file pawn plus a light-square bishop (or no pieces) */
      if ((B->WhitePawns & FileMask[FileH]) && !(B->WhiteBishops & BlackMask)) {
        wkingdist = Distance[B->WhiteKing][h8] - (B->side==WHITE ? 1 : 0);
        bkingdist = Distance[B->BlackKing][h8];
        if (bkingdist < wkingdist) wmat = FALSE;
      }
    }
  } while (0);

   /* Now test for black */
  if (B->BPts == 0) bmat = FALSE;
  else if (bpieces < 7) do {
    if (bpieces==3 && !B->BlackPawns) {bmat = FALSE;break;}
    if (bpieces==6 && !B->BlackPawns && !B->BlackBishops) {bmat = FALSE;break;}
     /* Bad rook pawn(s) & bishop combination */
    if (bpieces<=3 && !B->BlackKnights && (B->BlackPawns & EdgeMask)) {
       /* We have (at most) one bishop and several pawns */
       /* See if we have non-rook pawns */
      if (B->BlackPawns & CentreMask) break;
       /* See if we have rook pawns on both A and H files */
      if ((B->BlackPawns & FileMask[FileA]) && (B->BlackPawns & FileMask[FileH])) break;
       /* See if either of these two rook pawns are safe from the opp. king, therefore will queen */ 
      if (B->BlackPawns & PPKSMask) break;
       /* We only have an A-file pawn plus a light-square bishop (or no pieces) */
      if ((B->BlackPawns & FileMask[FileA]) && !(B->BlackBishops & BlackMask)) {
        wkingdist = Distance[B->WhiteKing][a1] - (B->side==WHITE ? 1 : 0);
        bkingdist = Distance[B->BlackKing][a1];
        if (wkingdist < bkingdist) bmat = FALSE;
      }
       /* We only have an H-file pawn plus a dark-square bishop (or no pieces) */
      if ((B->BlackPawns & FileMask[FileH]) && !(B->BlackBishops & WhiteMask)) {
        wkingdist = Distance[B->WhiteKing][h1] - (B->side==WHITE ? 1 : 0);
        bkingdist = Distance[B->BlackKing][h1];
        if (wkingdist < bkingdist) bmat = FALSE;
      }
    }
  } while (0);
}

#ifdef DRAWISH_ENDINGS
/* Check to see if a board position is likely to end in a draw */
BOOL Drawish(Board *B) {
  BOOL OppBish=FALSE;
  if (B->WPts>15 || B->BPts>15) return FALSE;
  if (Count(PassedMask) > 1) return FALSE;
  if (B->WhiteBishops && B->BlackBishops) {
    if (!(B->BlackBishops&WhiteMask) && !(B->WhiteBishops&BlackMask)) OppBish = TRUE;
    else if (!(B->BlackBishops&BlackMask) && !(B->WhiteBishops&WhiteMask)) OppBish = TRUE;
    if (OppBish) {
      if (B->wmaj==B->bmaj && B->wmin==B->bmin) {
        if (abs(B->WPts-B->BPts)<2) return TRUE;
      }
    }
  }
  return FALSE;
}
#endif


#ifdef AsymmetryDebug
#include "utils.h"
#include "parser.h"
// Check the board for scoring asymmetries
void CheckAsymmetry(Board *B) {
  Board Backup = *B;
  int sq, score, reverse;
  char FEN[128];

//  PrintBoard(*B);
  score = EvalMain(B,-INFINITY, INFINITY);

  for (sq=0;sq<64;sq++) {
    Backup.pieces[sq] = -B->pieces[Flip[sq]];
  }
  Backup.castle = 0;
  if (B->castle&1) Backup.castle |= 4;
  if (B->castle&2) Backup.castle |= 8;
  if (B->castle&4) Backup.castle |= 1;
  if (B->castle&8) Backup.castle |= 2;
  if (B->ep > 0) Backup.ep = Flip[B->ep];
  Backup.side = Opponent(B->side);
  BoardToFEN(&Backup,FEN);
  SetBoard(&Backup, FEN,FALSE);
//  PrintBoard(Backup);
  reverse = EvalMain(&Backup, -INFINITY, INFINITY);

  if (score + reverse != 0) {
    fprintf(stderr,"Score asymmetry!\n");
    PrintBoard(*B);
    fprintf(stderr,"Score = %d\nReverse Score = %d\n",score,reverse);
    BoardToFEN(B,FEN);
    fprintf(stderr,"FEN = %s\n",FEN);
    while (1);
  }
}
#endif
