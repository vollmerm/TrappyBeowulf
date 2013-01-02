/**********************************
 *    computil.c                  *
 *    Colin Frayn                 *
 *    November 2001               *
 **********************************/

/*
  This file contains all the miscellaneous
  utility functions for the computer search.
*/

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <math.h>
#include <string.h>
#ifdef _WIN32
# include <io.h>
# include <signal.h>
# define strncasecmp strnicmp
#else
# include <unistd.h>
#endif

#include "common.h"
#include "computil.h"
#include "moves.h"
#include "eval.h"
#include "utils.h"
#include "board.h"
#include "params.h"
#include "checks.h"
#include "parser.h"
#include "comp.h"

extern Undo UndoHistory[1000];
extern MOVE MoveHistory[1000];
extern CompDat Params;
extern Board Current_Board;
extern BOOL XBoard,ComputerGO,AnalyseMode,Pondering,UCI;
extern int mvno, Skill;
extern KeyType RandomTable[64][13], CastleKey[16];
extern BITBOARD RookMask[64],KnightMoves[64],BishopMask[64],FileMask[8];

/* Stuff from comp.c */
extern int Skill,GlobalDepth,InitFifty,TopPlyNMoves,TopPlyMoveno;
extern int InitialScore,PreviousScore,RootAlpha,RootBeta;
extern long int History[64][64];
extern MOVE Killer1[MAX_PV],Killer2[MAX_PV],MateKiller[MAX_PV];
extern MOVE MoveToPlay;  // For parsing move input during search/ponder
extern MOVE BestMoveRet; // The best move returned from the last recursive search
extern BOOL AbortFlag,CMFound,PrintedPV,QStoreAll,QStore,bEasyMove;

HashElt *TableB=NULL,*TableW=NULL;
int TimeToSolution;
KeyType Prev[1000];
MOVE LastBest;  // For running tests
float MaxTime;
char PV_text[1000];
BOOL bRushed;

/* Counters */
longlong Nodecount,Evalcount,Qnodes;
long int HashEntries,HashProbes,HashHits,NHash=0;
long int CheckExtensions,ThreatExtensions,OneReplyExtensions,PawnPushExtensions;
long int RevCheckExtensions,RecapExtensions;
long int CMCuts,DeltaCuts,SEECuts,RazorCuts,EvalCuts;
long int TBProbes, TBHits;
long int SortNodes,BestFirst;

/* Get the best move in this subsection of the move list and put it
 * at the front. */
void SortFrom(FullMove *Full,const int startno, const int NMoves) {
  int n,best=startno,bestscore=-INFINITY;
  FullMove temp;
   
   /* Loop through the moves and check for the highest score */
  for (n=startno;n<NMoves;n++) {
     /* Check to see if this move is better than the current best */
    if (Full[n].score > bestscore) {bestscore = Full[n].score;best=n;}
  }
   /* Put the best move in the correct place */
  if (best!=startno) {
    temp = Full[startno];
    Full[startno] = Full[best];
    Full[best] = temp;
  }
}

/* Print off the principal variation */
void PrintPV(Board *B) {
  WritePVToText(B);
  fprintf(stdout,PV_text);
  fprintf(stdout,"\n");
}

/* Print off the principal variation to a text string (PV_text) */
void WritePVToText(Board *B) {
  HashElt *Entry;
  Board Temp = *B;
  MOVE m=NO_MOVE;
  int count=0,i;
  KeyType KeyHistory[MAX_PV];
  char *pv = PV_text;
  BOOL First = TRUE;
#ifdef DEBUG_VERIFY
  KeyType CheckKey;
#endif

  *pv = 0;
  do { 
     /* Get the key for this position */
#ifdef DEBUG_VERIFY
    CheckKey = B->Key;
    GenerateHashKey(B);
    if (CheckKey != B->Key) {fprintf(stderr,"Warning - Key Corrupted in WritePVToText\n");while(1);}
#endif
    KeyHistory[count] = B->Key;
    /* Check the hash table */
    Entry = HashProbe(B);
    if (Entry) m = Entry->move;
    else m = NO_MOVE;

    /* Check for repeated positions */
    if (count>3) {
      for (i=count-4;i>=0;i-=2) if (KeyHistory[i] == B->Key) {
        strcat(PV_text," &rep.");
        pv += 6;
        m=NO_MOVE;
      }
    }

     /* If there is an entry then print off the suggested move */
    if (m!=NO_MOVE) {
      if (!First) *pv++ = ' ';
      pv = MoveToText(pv, m, B);
      First = FALSE;
      (void)DoMove(B,m);
    }
    count++;
    /* Continue until we're out of moves or we quit because of a repetition */
  } while (Entry && m!=NO_MOVE);   
  *B = Temp;
}

/* Pretty-print a move in PGN notation */
char *MoveToText(char *movestr, MOVE m, Board *B) {
  int to, from, chkpiece;
  Undo U;
  BITBOARD clash;

  if (m == NO_MOVE) {*movestr = 0;return movestr;}
  
  from=MFrom(m);
  to=MTo(m);

#ifdef DEBUG_VERIFY
  if ((B->side == WHITE && B->pieces[from] <= 0) ||
      (B->side == BLACK && B->pieces[from] >= 0) || 
      (abs(B->pieces[from]) > 6)) {
    fprintf(stderr,"Bad bestmove to print (%d - %d) (side=%d)\n",from,to,B->side);
    PrintBoard(*B);
    while (1);
  }
#endif

  if (IsCastle(m)) {
    *movestr++ = 'O';
    *movestr++ = '-';
    *movestr++ = 'O';
    if (from>to) {
      *movestr++ = '-';
      *movestr++ = 'O';
    }
  }
  else {
    clash = EMPTY;
    switch (PType(B->pieces[from])) {
    case pawn   : if (File(from)!=File(to)) *movestr++ = (char)(File(from) + 97); break;
    case knight : *movestr++ = 'N'; clash = B->WhiteKnights|B->BlackKnights; break;
    case bishop : *movestr++ = 'B'; clash = B->WhiteBishops|B->BlackBishops; break;
    case rook   : *movestr++ = 'R'; clash = B->WhiteRooks|B->BlackRooks; break;
    case queen  : *movestr++ = 'Q'; clash = B->WhiteQueens|B->BlackQueens; break;
    case king   : *movestr++ = 'K'; break;
    }
    /* Resolve Ambiguities */
    if (B->side==WHITE) clash &= B->WhitePieces;
    else clash &= B->BlackPieces;
    if (Count(clash)>1) {
      switch(PType(B->pieces[from])) {
      case rook: clash &= (QueenMoves(B,to) & RookMask[to]); break;
      case knight: clash &= KnightMoves[to]; break;
      case bishop: clash &= (QueenMoves(B,to) & BishopMask[to]);
      case queen: clash &= QueenMoves(B,to); break;
      }
      if (Count(clash)>1) {
        if (Count(clash&FileMask[File(from)]) == 1)
          *movestr++ = (char)(File(from)+97);
        else *movestr++ = (char)(56-Rank(from));
      }
    }
    if (IsEP(m) || B->pieces[to]!=empty) *movestr++ = 'x';
    *movestr++  = (char)(File(to)+97);
    *movestr++  = (char)(56-Rank(to));
    if (IsPromote(m)) *movestr++ = '=';
    switch (IsPromote(m)) {
    case (1) : *movestr++  = 'q'; break;
    case (2) : *movestr++  = 'r'; break;
    case (3) : *movestr++  = 'n'; break;
    case (4) : *movestr++  = 'b'; break;
    }
  }
  U = DoMove(B,m);
  if ((chkpiece = InCheck(B,B->side)) != FALSE) {
    if (InCM(B,chkpiece)) *movestr++ = '#';
    else *movestr++ = '+';
  }
  UndoMove(B,m,U);
  *movestr = 0;
  return movestr;
}

/* Reset some values ready to start the search */
void ResetValues(Board *B) {
  int i;

  Nodecount = Evalcount = Qnodes = 0;
  SortNodes = BestFirst = 0;
  memset(History, 0, sizeof(History));
  for (i=0;i<MAX_PV;i++) {
    Killer1[i] = NO_MOVE;
    Killer2[i] = NO_MOVE;
    MateKiller[i] = NO_MOVE;
  }
  AbortFlag=FALSE;
  HashEntries = HashProbes = HashHits = 0;
  TBProbes = TBHits = 0;
  DeltaCuts = SEECuts = CMCuts = EvalCuts = 0;
  RazorCuts = 0;
  InitialScore = Eval(B,-INFINITY,INFINITY);
  PreviousScore = InitialScore;
  bRushed = FALSE;  // Are we rushed to make a quick move?
  if (XBoard) GetTimeLimit();
  else {QStore = TRUE; QStoreAll = TRUE;}
  MaxTime = Params.MoveTime;
  CheckExtensions = ThreatExtensions = RecapExtensions = 0;
  PawnPushExtensions = OneReplyExtensions = RevCheckExtensions = 0;
  CMFound=PrintedPV=FALSE;
  TimeToSolution = 0;
  LastBest = NO_MOVE;
  MoveToPlay = NO_MOVE;
  if (B->side==WHITE) Skill = Params.WSkill;
  else                Skill = Params.BSkill;
  bEasyMove = FALSE;
}

/* Increase the counts for interesting looking moves at this ply */
void IncreaseCounts(const FullMove f,const int ply,const int score, const int side) {
  int m = f.move;
  int from = MFrom(m), to = MTo(m);

   /* Increase history counts. In order to stop low nodes heavily
    * altering the results, I clip this when the depth is too low. I add
    * the current search depth to the count so that top ply nodes score
    * more highly and hence influence the score more.  It is important to
    * get move ordering better at top ply moves so that the pruning works
    * most efficiently. */
  if (ply < GlobalDepth) History[from][to] += (GlobalDepth - ply);
   
   /* Update Killer Moves.  These are moves which, at this ply, seem to 
    * have caused more beta cuts than any others recently.  MateKillers
    * are killer moves which also return positive mate scores. */
  if (IsCM(score)==1) {
    if (m == Killer1[ply]) {
      Killer1[ply] = Killer2[ply];
      Killer2[ply] = NO_MOVE;
      MateKiller[ply] = m;
    }
    else if (m == Killer2[ply]) {
      MateKiller[ply] = m;
      Killer2[ply] = NO_MOVE;
    }
    else MateKiller[ply] = m;
  }
  else if (Killer1[ply] != m) {
    Killer2[ply] = Killer1[ply];
    Killer1[ply] = m;
  } 
}
					  

/* Test to see if we should continue the search based on the current 
 * positional score, the current best move score, the time searched so
 * far and the current computer params. */
BOOL ContinueSearch(const int score, const int side, const int depth) {
  float diffsec;
  
   /* Check to see if we've got a drawn game returning very quickly */
  if (Nodecount == 0) return FALSE;  /* CM or immediate draw */
  if (abs(score) == abs(DRAW_SCORE)) {
    if (GlobalDepth>20 && Nodecount<200000) return FALSE;
  }
   
   /* Don't continue if we've got an abort flag */
  if (AbortFlag) return FALSE;
   /* Always continue if we've not searched to the required depth yet */
  if (depth<=Params.Depth) return TRUE;
   /* If we've done the minimum depth search, and we have a time limit of zero then
    * quit now */
  if (Params.MoveTime == 0) return FALSE;

  diffsec = (float)GetElapsedTime() / 100.0f;
   
   /* Check how we're doing and modify the recommended search time accordingly. */
  ModifyTime(score);

   /* If not running a test then quit if we've used 2/3 of the suggested time.
    * Of course, this suggested time is modified regularly depending on how we're
    * doing so far.  We use a value of 2/3 because it is roughly at this stage
    * that starting a new search would be fruitless as we would not be able to 
    * finish analysing the first move in the next depth. */
  if (!Params.Test && diffsec*1.5f > MaxTime) return FALSE;
   /* If we're doing a test then quit only when we've used all our available time */
  if (Params.Test && diffsec > MaxTime) return FALSE;
   
  return TRUE;
}

/* Check if we're out of time.  First of all, calculate what a sensible
 * search time should be based on how much time we initially allotted for
 * this move, and how many points we're losing so far.  Then see if we've
 * exceeded this time or not.  Return TRUE if we're out of time. */
BOOL CheckTime(const int ply, const int score) {
  float diffsec;

  diffsec = (float)GetElapsedTime() / 100.0f;
   
   /* Modify the maximum time allowance if we're at the top ply */
  if (ply==0) ModifyTime(score);
	
   /* Check if we should still be storing quiescence nodes in the HT */
  if (HashEntries*3>NHash*2) QStore = FALSE; 
  if (HashEntries*4>NHash) QStoreAll = FALSE; 

   /* If we're out of time then return TRUE */
  if (diffsec >= MaxTime) return TRUE;
   /* If we're going to use up all our time then return TRUE */
  if (XBoard && (Params.Time - (int)(diffsec*100.0) < 100)) return TRUE;

   /* We're OK - we still have time left! */
  return FALSE;
}

/* Check if a board position is a draw (theoretically) */
BOOL IsDrawn(Board *B,const int ply, const int fifty, BOOL MainSearch) {
  int tpts = max(B->WPts, B->BPts);

   /* Firstly, check for draw by the fifty move rule */
  if (fifty>100) return TRUE;
   
   /* Now check for draw by lack of force */
  if (tpts < 4 && !(B->WhitePawns | B->BlackPawns)) return TRUE;
   
   /* Finally, check for draw by repetition.  Don't bother in the qsearch
    * becuse that deals with repetition chain breaking moves only */
  if (!MainSearch) return FALSE;
  return RepeatedPosition(B,ply,fifty);
}

/* Print off the current thinking to the screen */
void PrintThinking(const int score,Board *B) {
  int diffcentisec;
  longlong nodes = Nodecount + Qnodes;
  HashElt *Entry;
   
  diffcentisec = GetElapsedTime();

   /* Check for Time to Solution updates */
  if (Params.Test) {
     /* Get the key for this position */
    GenerateHashKey(B);
    /* Check the hash table */
    Entry = HashProbe(B);
    if (Entry && Entry->move != NO_MOVE) {
      if (Entry->move != LastBest) {
        TimeToSolution = diffcentisec;
        LastBest = Entry->move;
      }
    }
  }
   
   /* Don't bother printing off if this is uninteresting */
  if (!PrintedPV && !XBoard && IsCM(score)!=1 && ((float)diffcentisec<(Params.MoveTime*10)) && diffcentisec<40) return;
   
  PrintedPV = TRUE;
   

  if (UCI) {
    WritePVToText(B);
    fprintf(stdout,"info depth %d time %d nodes %.0f ",GlobalDepth,diffcentisec*10,(double)Nodecount);
    fprintf(stdout,"tbhits %d score %ld pv %s\n",TBHits,score,PV_text);
  }
  else {
    Pad((long)GlobalDepth,2);
    fprintf(stdout,"%d ",GlobalDepth);
    if (!XBoard && IsCM(score)) {
      if (score>0) {
        if (CMSCORE-score<10) fprintf(stdout," ");
        fprintf(stdout,"Mate%d ",CMSCORE-score);
      }
      else {
        if (CMSCORE+score<10) fprintf(stdout," ");
        fprintf(stdout,"Lost%d ",CMSCORE+score);
      }
    }
    else {
      if (!XBoard && score>=THEORETICAL_WIN) fprintf(stdout,"   Won ");
      else if (!XBoard && score<=-THEORETICAL_WIN) fprintf(stdout,"  Lost ");
      else {
        Pad(score,5);
        fprintf(stdout,"%d",score);
        if (score >= RootBeta) fprintf(stdout,"!");
        else if (score <= RootAlpha) fprintf(stdout,"?");
        else fprintf(stdout," ");
      }
    }
  }
  Pad(diffcentisec,6);
  fprintf(stdout,"%d",diffcentisec);
  Pad((long)nodes,10);
  fprintf(stdout,"%.0f   ",(double)nodes);
  PrintPV(B);
}

/* Pad out a number with spaces */
void Pad(long int num,int maxlen) {
  int tally=0;
  if (num<0) tally++;
  while (abs(num)>9) {num/=10;tally++;}
  while (++tally<=maxlen) fprintf(stdout," ");
}


/* Setup the hash table */
void SetupHash(void) {
  long int HashMem;
   
  if (NHash==0) {
     /* Work out how much hash table memory we have to play with */
    HashMem = (1<<Params.HashSize)*512;
   
     /* Calculate a sensible number of hash table entries, using the largest
      * prime number less than the calculated maximum number. */
    NHash   = LargestPrime(HashMem/((long int)sizeof(HashElt)));

     /* Allocate the memory */
    TableW = (HashElt *)calloc(sizeof(HashElt), (size_t)NHash);
    assert(TableW!=NULL);
    TableB = (HashElt *)calloc(sizeof(HashElt), (size_t)NHash);
    assert(TableB!=NULL);
  }
}

/* Get the initial suggested base time limit per move in an
 * xboard game with fixed time controls. */
void GetTimeLimit(void) {
  int left;
  int oleft,tpts=Current_Board.WPts+Current_Board.BPts;
  float mvleft;
   
   /* Get how much time we have to play with, and how much our opponent has */
  left = Params.Time;
  oleft = Params.OTime;

   /* If we know the number of moves per time control, then work out how many
    * we have left to play.  Otherwise, estimate the average game is about
    * GAME_LENGTH moves long.
    * Assume also that we're probably going to have to play about another
    * MIN_LEFT moves minimum from any position, unless we're playing a certain number
    * of moves per time control.  We should also take into account that we
    * might need to spend a bit longer occasionally because of blunder
    * avoidance */
  if (Params.Mps != 0) {
    mvleft = (float)(Params.Mps + 1 - ((mvno/2)%Params.Mps));
     /* If we have several moves left then allocate a slight buffer of time in
      * case we have to extend a search later because of blunder avoidance. */
    if (mvleft>1) mvleft *= 1.1f;
  }
  else {
    mvleft = (float)((GAME_LENGTH*2)-mvno)/2.0f;
    if (mvleft<(float)MIN_LEFT) mvleft = (float)MIN_LEFT;
  }
   
   /* If we're down on time compared to our opponent then reduce time per move */
  if (oleft > left && mvleft > 10.0f) mvleft++;
   
   /* If we're in the opening still then spend more time per move */
  if (tpts > 70 && mvleft > 10.0f) mvleft--;

   /* Set the suggested thinking time */
  Params.MoveTime = (float)left / (mvleft * 100.0f);
   /* If we have 8 seconds left or less then move very quickly and turn
    * off one or two complicated algorithms (for accuracy),
    * provided we're not playing in a time-per-session game or an increment game. */
  if (left<800 && Params.Mps == 0 && Params.Inc == 0) {Params.MoveTime = 0.1f;bRushed = TRUE;}
   /* If we're playing with a time increment per move then factor in the
    * extra time we have  If this is more than the available time then it'll
    * get trapped in the time control code. Hopefully :) */
  if (Params.Inc>0) Params.MoveTime += (float)Params.Inc;
   /* Check if we have enough space in the hash table to store quiescence
    * positions during this search. */
  QStore = QStoreAll = FALSE;
  if (Params.HashSize>8 && (float)(1<<(Params.HashSize-9)) > Params.MoveTime) QStore = TRUE; 
  if (Params.HashSize>11 && (float)(1<<(Params.HashSize-12)) > Params.MoveTime) QStoreAll = TRUE; 
}

/* Modify the recommended search time remaining depending on how we're doing so far,
 * but only if we're playing a game under XBoard.  Otherwise, leave it alone ;) 
 * We check for score drops from the last iteration, and extend accordingly to try
 * to find a better move. */
void ModifyTime(const int score) {
  float scaled_loss,ext,maxt;
   
   /* Set the maximum time to the initial recommended value */   
  MaxTime = Params.MoveTime;

   /* Only bother doing blunder avoidance if we're playing a game
    * and we're not rushed */
  if (!XBoard && !bRushed) return;
   
   /* Check if this is an easy move - in which case reduce the time available to one third */
  if (XBoard && bEasyMove) MaxTime /= 3.0f;

   /* If this move improves the score since last iteration then don't extend */
  if (score >= PreviousScore) return;

   /* If were in a test then don't modify the time */
  if (Params.Test) return;

   /* Calculate how much material we're losing in terms of how many times
    * the scale factor, LOSS_SCALE, the score has dropped since last iteration */
  scaled_loss = (float)(PreviousScore-score)/(float)LOSS_SCALE;
  if (scaled_loss>5.0) scaled_loss=5.0;
   
   /* If we're still ahead then reduce the damage ;) */
  if (score>0) scaled_loss *= 0.6f;

   /* Calculate the factor by which the search time must be increased */
  ext = (float)pow(BASE_INCR/10.0,scaled_loss);
   
   /* Return the new search time, as recommended based on the loss avoidance
    * tactics.  Basically, it is wise to spend longer contemplating ways of avoiding
    * losing pieces.  The game is probably lost anyway if you lose the piece, so
    * it is well worth actually trying not to, even at the expense of some time. */
  MaxTime *= ext;
   /* Make sure we don't allocate more time than we have */
  maxt = (float)Params.Time / 200.0f;  /* Half the remaining time */
  if (Params.Mps == 0 && ext > 1.0 && MaxTime>maxt) MaxTime = maxt;
  if (Params.Mps == 0 && maxt < 2.0) MaxTime=0.0;
}

/* Probe the hash table for the specified board */
HashElt *HashProbe(Board *B) {
  long int hashentry;
  HashElt *H=NULL;
  StoreKey SK;
  KeyType Hashkey = B->Key; 

//  hashentry = (long int)(Hashkey % (KeyType)NHash);
  hashentry = ((unsigned long int)(Hashkey>>32)) % NHash;
   
  switch (B->side) {
   case WHITE: H = &TableW[hashentry]; break;
   case BLACK: H = &TableB[hashentry]; break;
  }

   /* Update counter */
  HashProbes++;

   /* Check if this entry exists, and return it if so */
  if (H && H->depth>0) {
    SK = PackToStoreKey(Hashkey);
    if (H->Key == SK) {ResetStaleness(H->flags);HashHits++;return H;}
  }
   /* This entry doesn't exist :( */
  return NULL;
}

/* Update the hash table with the given information */
void HashUpdate(Board *B, int score, MOVE bestmove, int depth,
		short int flag, BOOL threat, int ply) {
  long int hashentry;
  HashElt *Entry;
  KeyType Hashkey = B->Key; 

#ifdef DEBUG_VERIFY
  int from = MFrom(bestmove), to = MTo(bestmove);
  if (bestmove != NO_MOVE) {
    if ((B->side == WHITE && B->pieces[from] <= 0) ||
        (B->side == BLACK && B->pieces[from] >= 0) || 
        (abs(B->pieces[from]) > 6)) {
      fprintf(stderr,"Bad bestmove to store (%d - %d) (side=%d)\n",from,to,B->side);
      PrintBoard(*B);
      while (1);
    }
  }
#endif
   /* See if we already have an entry for this position */
  Entry = HashProbe(B);
   /* If we already have an entry then just update this entry if
    * the new information is from a deeper search. */
  if (Entry) {
     /* We might have a shallower entry this time, but with a bestmove
      * which we don't already have. Check for this. Always update top ply
      * values. */
    if (Entry->move==NO_MOVE) Entry->move=bestmove;
    if (ply>0 && depth < Entry->depth) return;
    Entry->depth = depth;
    Entry->score = (short)score;
    if (bestmove != NO_MOVE) Entry->move = bestmove;
    ResetStaleness(Entry->flags);
  }
   /* Need a new entry */
  else {
//    hashentry = (long int)(Hashkey % (KeyType)NHash);
    hashentry = ((unsigned long int)(Hashkey>>32)) % NHash;
    switch (B->side) {
     case WHITE: Entry = &TableW[hashentry]; break;
     case BLACK: Entry = &TableB[hashentry]; break;
    }
     /* We might have a clash if this entry is already in use (storing a different
      * position which happens to give the same hash key). We replace the old entry if;
      * (1) The new entry is from the top ply,
      * (2) The new result is from an equal depth or deeper search than the old entry or
      * (3) The old entry is stale
      */
    if (ply > 0 && GetStaleness(Entry->flags) == 0 && Entry->depth > depth) return;

    // We're OK to replace the entry
    Entry->Key = PackToStoreKey(Hashkey);
    Entry->move = bestmove;
    Entry->score = (short)score;
    Entry->depth = depth;
    ResetStaleness(Entry->flags);

     /* Update counter */
    HashEntries++;
  }
  if (threat) SetHashType(Entry->flags,HASH_NULL);
  else SetHashType(Entry->flags,flag);
}

/* Generate Hash Keys for this position */
void GenerateHashKey(Board *B) {
  int sq,p;
  KeyType key=0;
   /* We do this by XOR'ing together a list of 64 bit integers
    * which were randomly generated at runtime.  These integers represent one
    * different number for each piece on each square.  The combined XOR of these is
    * used as a key for this position */
  for (sq=64;--sq>=0;) {
    p = B->pieces[sq] + 6;
    key ^= RandomTable[sq][p];
  }
   /* Include the castling permissions */
  key ^= CastleKey[B->castle];
  B->Key = key;
}

/* Check if the current position is repeated */
BOOL RepeatedPosition(const Board *B, const int ply, const int fifty) {
  int a;

   /* Set up the hash value for this position in the history list */
  Prev[ply+InitFifty] = B->Key;

   /* Test to see if this position has been repeated before.  If we've had fewer
    * than 4 quiescent/drawable moves in a row then this can't possibly be the case. */
  if (fifty<4) return FALSE;
   /* Otherwise test the recent positions for this side, starting with the most recent 
    * one which could possibly have been a repetition of the current board */
  for (a=ply+InitFifty-4;a>=ply+InitFifty-fifty;a-=2) {
    if (Prev[a] == B->Key) return TRUE;
  }
  return FALSE;
}

/* Get a list of positions that have been played since the last capture/pawn move.
 * This is then used for the draw-by-repetition check */
int GetRepeatedPositions(void) {
  int n,fifty=0;
  MOVE m;
  Board B = Current_Board;
   
  for (n=0;n<1000;n++) Prev[n]=0;
   
  n=mvno;
   /* Parse through and get the number of moves, then parse through again
    * to put them in the list in the correct (reverse) order */
  while (--n>=0) {
    fifty++;
    m = MoveHistory[n];
    if (UndoHistory[n].capt) break;
    if (IsEP(m) || IsPromote(m)) break;
    if (PType(B.pieces[MTo(m)])==pawn) break;
     /* Go back to the previous move */
    UndoMove(&B, MoveHistory[n], UndoHistory[n]);
  }
  B = Current_Board;
  for (n=0;n<=fifty;n++) {
     /* Store this position in the list */
    GenerateHashKey(&B);
     /* Yeah I know this is backwards, but it doesn't matter */
    Prev[fifty-n] = B.Key;
     /* Stop if we come across a capture/pawn move */
    if (mvno<=n) break;
    m = MoveHistory[mvno-n-1];
    if (UndoHistory[mvno-n-1].capt) break;
    if (IsPromote(m) || IsEP(m)) break;
    if (PType(B.pieces[MTo(m)])==pawn) break;
     /* Go back to the previous move */
    UndoMove(&B, MoveHistory[mvno-n-1], UndoHistory[mvno-n-1]);
  }

   /* Return how many drawable *moves* (not positions) we've had */
  return fifty;
}

/* Check if this position is a draw by repetition */
BOOL DrawByRepetition(int fifty, int ply, KeyType Key) {
  int count=0,n;

   /* Check if the draw move chain is long enough */   
  if (fifty<11) return FALSE;
   /* If so, then test these moves */
  for (n=InitFifty+ply-fifty;n<InitFifty+ply;n++) {
     /* We've found this position before! */
    if (Prev[n]==Key) {
      count++;
      if (count==2) return TRUE;
    }
  }
  return FALSE;
}

/* Check the move given (from the hashtable) and add on the
 * standard flags for EP and castling, if necessary */
MOVE CheckMove(MOVE m) {
  Board *B = &Current_Board;
  int to=(m>>6)&63,from=m&63;
   
   /* Check if this is an en-passant move */
  if (!IsEP(m) && PType(B->pieces[from])==pawn && File(to)!=File(from) && B->pieces[to]==empty) m |= SPECIAL_FLAG;
   /* Check if this is a castling move */
  if (!IsCastle(m)) {
    if (PType(B->pieces[from])==king && File(from)==FileE && File(to)==FileC) m |= SPECIAL_FLAG;
    if (PType(B->pieces[from])==king && File(from)==FileE && File(to)==FileG) m |= SPECIAL_FLAG;
  }
      
  return m;
}

/* Test to see if our position is worth resigning from.
 * Lower skill players take much longer to resign.  */
BOOL CheckResign(const int score) {
  if (score<(Skill*40)-1000) {
    if (XBoard) fprintf(stdout,"resign\n");
    else        fprintf(stdout,"I Resign!\n");
    ComputerGO = FALSE;
    return TRUE;
  }
  return FALSE;
}

/* Print out info for the thinking at the end of the search */
void PrintInfo(const int score) {
  int diffsec;
  float time_spent,frac;
  longlong nodes = Nodecount + Qnodes;
  int n;
   
  diffsec = GetElapsedTime() / 100;
  time_spent = (float)GetElapsedTime() / 100.0f;
   
  if (diffsec >= 1000) fprintf(stdout,"\nTime Taken : %d Seconds\n",diffsec);
  else if (diffsec >= 100) fprintf(stdout,"\nSearch Time : %.1f Seconds\n",time_spent);
  else fprintf(stdout,"\nSearch Time : %.2f Seconds\n",time_spent);

  if (Nodecount+Qnodes==0) frac=0.0;
  else frac = (float)Qnodes / (float)(Nodecount+Qnodes);
  frac *= 100;
   
  fprintf(stdout,"Total Nodes Searched   : %.0f  (%.1f%% Qui)\n",(double)nodes,frac);
  fprintf(stdout,"Total Nodes Evaluated  : %.0f\n",(double)Evalcount);
   
  fprintf(stdout,"Cuts       : Delta ");
  Pad(DeltaCuts,7);
  fprintf(stdout,"%ld  : SEE     ",DeltaCuts);
  Pad(SEECuts,6);
  fprintf(stdout,"%ld  : Mate     ",SEECuts);
  Pad(CMCuts,5);
  fprintf(stdout,"%ld\n",CMCuts);
  fprintf(stdout,"           : Razor  ");
  Pad(RazorCuts,6);
  fprintf(stdout,"%ld  : Eval    ",RazorCuts);
  Pad(EvalCuts,6);
  fprintf(stdout,"%ld\n",EvalCuts);
   
  fprintf(stdout,"Extensions : Check ");
  Pad(CheckExtensions,7);
  fprintf(stdout,"%ld  : OneRep  ",CheckExtensions);
  Pad(OneReplyExtensions,6);
  fprintf(stdout,"%ld  : CMThreat ",OneReplyExtensions);
  Pad(ThreatExtensions,5);
  fprintf(stdout,"%ld\n           : Pawn  ",ThreatExtensions);
  Pad(PawnPushExtensions,7);
  fprintf(stdout,"%ld  : Recap   ",PawnPushExtensions);
  Pad(RecapExtensions,6);
  fprintf(stdout,"%ld  : RevCheck ",RecapExtensions);
  Pad(RevCheckExtensions,5);
  fprintf(stdout,"%ld\n",RevCheckExtensions);
   
  fprintf(stdout,"Hash Stores (Size) : %ld  (%ld)\n",HashEntries,NHash);
  if (!USE_HASH) HashProbes = HashHits = 0;
  fprintf(stdout,"Hash Probes (Hits) : %ld  (%ld)\n",HashProbes,HashHits);
  fprintf(stdout,"EGTB Probes (Hits) : %ld  (%ld)\n",TBProbes,TBHits);
  if (SortNodes>0) fprintf(stdout,"Move Ordering      : %.1f%%\n",(double)BestFirst*100.0/(double)SortNodes);
  else fprintf(stdout,"Move Ordering      : N/A\n");
  fprintf(stdout,"Best Move = ");
  n=0;
  do {
    fprintf(stdout,"%c",PV_text[n]);
  } while (PV_text[++n]!=' ' && n<(int)strlen(PV_text));
  fprintf(stdout,"\n");
  fprintf(stdout,"Move Score = %d\n",score);
	if (bEasyMove) fprintf(stdout,"(Easy Move)\n");
}

/* Test to see whether or not a NULL move is safe in the given position.
 * This is false if;
 * (1) There is less material on the board than the defined minimum, AVOID_NULL_MAT
 * (2) There are fewer pieces than AVOID_NULL_PIECES
 * If a NULL move is safe then we calculate the amount by which we should reduce
 * the search depth. */
int NullOK(Board *B,int depth) {
   int cwp=0,cbp=0,base_reduction = (depth>IGNORE_ZUGZWANG) ? 0 : ONEPLY;
   
   /* If there is a risk of Zugzwang then return 0 (no Null move) */
  if (B->side==WHITE && B->WPts < AVOID_NULL_MAT) return base_reduction;
  if (B->side==BLACK && B->BPts < AVOID_NULL_MAT) return base_reduction;
  cwp = Count(B->WhitePieces);
  if (B->side==WHITE && cwp < AVOID_NULL_PIECES) return base_reduction;
  cbp = Count(B->BlackPieces);
  if (B->side==BLACK && cbp < AVOID_NULL_PIECES) return base_reduction;
      
  if (Skill<=8) return Skill;
   
   /*
    The formula below is from Ernst A. Heinz's book "Scalable Search in Computer Chess"
    It comes from pages 35-37 and is described elsewhere in the book.
    This method is called 'Adaptive Null Move Pruning' with R(adapt) = 3(6)~2.
    In English, the NULL move depth reduction is equal to two ply by default.  However,
    if either (a) both sides have fewer than 3 pieces and the current depth is 8 ply or
    more or (b) at least one side has greater than 2 pieces and the current depth is
    6 ply or more then increase the depth reduction to 3 full ply.
   */
//   return TWOPLY + ((depth) > ((6*ONEPLY) + (((cwp < 3 && cbp < 3) ? TWOPLY : 0))) ? ONEPLY : 0);
  
   /* This is my own formula, offering scaleable search depth reduction between one and
    * 2.5 ply depending on the depth to which you are searching */
   return ONEPLY + (depth > (ONEPLY*8) ? (ONEPLY) : (depth / ONEPLY)) + ((depth > ONEPLY) ? (HALFPLY) : (0));
}

/* Calculate the largest (odd) prime number not greater than n.
 * I know this is a dumb way of doing it, but it's easily fast enough 
 * considering that it should only need to be done once per game. */
long int LargestPrime(long int n) {
  int max_fact = (int)sqrt((double)n), i;
   /* This clause should never be needed, but it's worth keeping for safety */
  if (n<5) return 3;
  n += (n%2) + 1;
  do {
    n-=2;
    for (i=3;i<=max_fact;i+=2) if (n%i == 0) break;
  } while (i<=max_fact);
  return n;
}

/* Check for user input during the search */
int CheckUserInput(void) {
  char input[FILENAME_MAX]="", *endc;
  char CAPScopy[FILENAME_MAX];
  int bytes,n,centitime;
  MOVE tempmove;

  /* If there is no input waiting then return immediately */
  if (Bioskey()==0) return INPUT_NULL;

  /* If we're pondering then return right away */
  if (Pondering) return INPUT_STOP;

  /* Cross fingers and pray that this works ;) */
  do {
    bytes=read(fileno(stdin),input,FILENAME_MAX);
  } while (bytes<0);
  endc = strchr(input,'\n');
  if (endc) *endc=0;

  /* If we have input waiting then parse it */
  if (strlen(input)>0) {
    for (n=0;n<(int)strlen(input);n++) CAPScopy[n] = toupper(input[n]);
    if (CAPScopy[0] == 'Q') return INPUT_STOP;
    if (!strncmp(CAPScopy,"FORCE",5)) return INPUT_STOP;
    if (!strncmp(CAPScopy,"STOP",4)) return INPUT_STOP;
    if (!strncmp(CAPScopy,"EXIT",4)) return INPUT_STOP;
     // actually, "result 1/2-1/2 {draw}"
    if (!strncmp(CAPScopy, "RESULT", 6)) return INPUT_STOP;
    if (strstr(CAPScopy,"RESIGN")) return INPUT_RESIGN;
#ifdef BEOSERVER
    if (strstr(CAPScopy,"NODE")) {ReadWorkDone(input);return INPUT_WORK_DONE;}
#endif // BEOSERVER
    if (input[0] == '?') return INPUT_MOVE_NOW;
    if (AnalyseMode==TRUE) {
      if (!strncmp(CAPScopy,"UNDO",4)) return INPUT_UNDO;
      if (!strncmp(CAPScopy,"NEW",3)) return INPUT_NEW;
      if (input[0] == '.') {
        centitime = GetElapsedTime();
        fprintf(stdout,"stat01: %d %.0f %d %d %d\n",centitime,(double)(Nodecount+Qnodes),GlobalDepth,TopPlyNMoves-TopPlyMoveno-1,TopPlyNMoves);
        return INPUT_NULL;
      }
       /* -- Standard Move -- */
      if ((tempmove = ParseMove(input,Current_Board,FALSE)) != NO_MOVE) {
        MoveToPlay = tempmove;
        return INPUT_MOVE;
      }
    }
  }
  return INPUT_NULL;
}

/* Expire the hash elements & check staleness (after the search) */
void ExpireHash(void) {
  int n;

  // Don't bother to do this if we're rushed - it takes too much time for a large table
  if (bRushed) return;

   /* Expire old entries */
  for (n=0;n<NHash;n++) {
    TableB[n].flags++;
    TableW[n].flags++;
  }
}

/* Reset the entire hash table */
void ResetHash(void) {
  if (TableW) free(TableW);
  if (TableB) free(TableB);
  TableW = TableB = NULL;
  NHash = 0;
}

/* Stuff needed for the timing procedure */

#ifdef _WIN32

#include <sys/timeb.h>
struct timeb start;

/* Set the time for the start of the computation loop */
void SetStartTime(void) {
  (void)ftime(&start);
}

/* Get the elapsed time in centiseconds so far */
int GetElapsedTime(void) {
  struct timeb end;
  int TimeTaken;

  (void)ftime(&end);
  TimeTaken  = (int)(end.time - start.time)*100;
  TimeTaken += (int)((end.millitm - start.millitm) / 10.0);

  return TimeTaken;
}

#else

#include <unistd.h>
#include <sys/time.h>
#include <time.h>

struct timeval tvalStart;

void SetStartTime(void) {
  gettimeofday(&tvalStart, 0);
}

int GetElapsedTime(void) {
  struct timeval tvalStop;
  int TimeTaken;
  gettimeofday(&tvalStop, 0);
  TimeTaken = (tvalStop.tv_sec - tvalStart.tv_sec)*1000000;
  TimeTaken += (tvalStop.tv_usec - tvalStart.tv_usec);
  TimeTaken /= 10000;
  return TimeTaken;
}

#endif // _WIN32
