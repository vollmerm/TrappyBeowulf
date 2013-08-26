/**********************************
 *    comp.c                      *
 *    Colin Frayn                 *
 *    March 2001                  *
 **********************************/

/*
  This file contains all the functions for the
  computer player core movement algorithms.
*/

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <math.h>
#include <string.h>
#ifdef _WIN32
# include <signal.h>
# define strncasecmp strnicmp
#endif

#include "common.h"
#include "comp.h"
#include "computil.h"
#include "moves.h"
#include "eval.h"
#include "checks.h"
#include "utils.h"
#include "attacks.h"
#include "probe.h"
#include "params.h"
#include "board.h"
#include "trappy.h"


/* Defines for the parallel algorithm */

#ifdef BEOSERVER
# define MIN_SEQ_TIME    (300)   // How long (centiseconds) we spend searching natively.
                                 // The next ply after this time is searched parallel.
# define PRIORITY_START  (10000) // Initial value for the priority calculation
# define PRIORITY_SCALE  (100)   // Move step for the priority calculations
# define PRIORITY_STEP   (1)     // Ply step for the priority calculations
# define NODE_TABLE_SIZE (16384) // Size for the Node table for the leaf nodes
# define SPLIT_NODE_COST (3000)  // If a node looks like it will cost this much to search
                                 // then split it up if it needs to be searched again more
                                 // deeply.  This is normalised in arbitrary units such
                                 // that 1000 is the cost to search the opening position
                                 // to a depth of 9 ply.
# define TOUGH_NODE      (2000)  // If a node >= this complicated is aborted then split it
                                 // in the re-search.
# define EASY_NODE       (1000)  // Nodes predicted to be this easy after an extra depth
                                 // are extended by one.
# define MAX_SPLIT       (4)     // Maximum depth to which the tree can be split
# define ABORT_MARGIN    (25)    // Margin for re-searching potentially interesting nodes.
//# define EXTEND_EASY_NODES       // Allow depth extension for 'easy' search nodes.
#endif // BEOSERVER

// Use the PV search algorithm?
#define USE_PV_SEARCH

/* External variables from the main program */
extern Undo UndoHistory[1000];
extern MOVE MoveHistory[1000];
extern CompDat Params;
extern Board Current_Board;
extern BOOL AnalyseMode,TBON,BookON,XBoard,AutoMove,Post,UCI,bRushed;
extern int mvno;
extern int PieceValue[7],PromotePiece[5],PieceValue10[7],PieceValue100[7];
extern BITBOARD KnightMoves[64], Mask[64];

/* Global Variables */
int InitFifty,Skill=10,TopPlyNMoves,GlobalDepth,TimeTaken,TopPlyMoveno;
long int History[64][64];
int InitialScore,PreviousScore,RootAlpha,RootBeta, InputFlag, GlobalAlpha, GlobalBeta, TopOrderScore, NextBestOrderScore;
MOVE Killer1[MAX_PV],Killer2[MAX_PV], MateKiller[MAX_PV], MoveToPlay;
BOOL TBHit,AbortFlag,PrintedPV,CMFound=FALSE,IsCap[MAX_PV],QStore,QStoreAll,Pondering=FALSE, bEasyMove = FALSE;
MOVE BestMoveRet;

int TrapVectorScore[64][64][MAX_DEPTH+1][MAX_DEPTH+1][TRAP_KEY_SIZE] = {0};
int TrapVectorRecorded[64][64][MAX_DEPTH+1][MAX_DEPTH+1][TRAP_KEY_SIZE] = {0};
BOOL ENABLETRAP;
BOOL TrapSet;
int TrapsFound;
MOVE topmove;

#ifdef BEOSERVER
extern int BenchmarkSpeed;
extern BOOL bParallel;
int NextNodeID, SeqDepth, FirstParallelDepth=2;
float ExtendCost;
BOOL bTopMoveComplete, FirstIteration = TRUE;
MOVE InitHashMove;
NODETABLE *NodeTable;
BOOL Complete[MAX_PV];
#endif // BEOSERVER

/* Counters defined in computil.c */
extern longlong Nodecount,Qnodes;
extern long int CheckExtensions,ThreatExtensions,OneReplyExtensions,PawnPushExtensions;
extern long int RevCheckExtensions,RecapExtensions;
extern long int CMCuts,SEECuts,DeltaCuts,RazorCuts;
extern long int BestFirst,SortNodes;

/* Quiescence search options */
#define QUIESCENCE_STORE_ALL
#define QUIESCENCE_STORE_CUTS
#define QUIESCENCE_CHECK_EVASIONS
#define QUIESCENCE_EVASION_ALL

/* Offer a draw from EGTB probe? */
//#define OFFER_TB_DRAW

/* Polling period for inputs.  Alter only POLLING_DELAY. */
#define POLLING_DELAY   (12)
#define INPUT_PERIOD    ((1<<POLLING_DELAY)-1)

 
/* --- FUNCTIONS --- */


/* Analyse the given board position for the current side with the current
 * global computer parameters in Params. */
MOVE Comp(void) {
  int depth=2,inchk,val=0,score=0;
#ifdef BEOSERVER
  int n;
  float ExtendCostAv;
#endif // BEOSERVER
  longlong LastPlyNodecount=1;
  BOOL Continue=FALSE,resign=FALSE;
  Board BackupBoard=Current_Board,*B = &BackupBoard;
  MOVE Previous=NO_MOVE,BookMove=NO_MOVE,BestMove;
  HashElt *Entry=NULL;
  BOOL bBreakout = FALSE;

   /* Reset the input flag before we do anything.  This flag tells us about how
    * and why the comp() procedure exited.  Normally it is INPUT_NULL, which tells
    * us all is OK.  Sometimes it is set to different values, often in analysis mode */
  InputFlag = INPUT_NULL;

   /* Check the Opening Book First */
  if (AnalyseMode==FALSE && BookON) {
    BookMove = CheckOpening(B,&val);
  }
  if (BookMove!=NO_MOVE) {
     /* Check some values for the book move, i.e. EP and castle */
    BookMove = CheckMove(BookMove);
    if (UCI) fprintf(stdout,"bestmove ");
    else if (XBoard) fprintf(stdout,"move ");
#ifdef BEOSERVER
    else fprintf(stdout,"Best Move = ");
#endif
    PrintMove(BookMove,TRUE,stdout);
#ifdef BEOSERVER
    fprintf(stdout,"\n");
#endif
    fprintf(stdout,"  <Book %d%%>\n",val);
    if (!AnalyseMode && (XBoard || AutoMove)) {
      MoveHistory[mvno] = BookMove;
      UndoHistory[mvno] = DoMove(&Current_Board,BookMove);
      mvno++;
    }
    AnalyseMode = FALSE;
    return BookMove;
  }

   /* Setup the Hash Table */
  SetupHash();
   
   /* Setup the draw by repetition check. InitFifty holds the number of moves backwards
    * we can look before the last move which breaks a fifty move draw chain.  We need
    * to store this to help with the draw checking later on. */
  InitFifty = GetRepeatedPositions();

   /* Reset Initial values */
  ResetValues(B);
   
   /* Test for check */
  inchk = InCheck(B,B->side);

   /* Display the current positional score */
  fprintf(stdout,"Current Position = %.2f\n",(float)InitialScore/100.0f);

   /* Count the possible moves */
  TopPlyNMoves = (int)CountMoves(B,1,1);
  if (!XBoard) fprintf(stdout,"Number of Possible Moves = %d\n\n",TopPlyNMoves);
  if (TopPlyNMoves==0) {
    if (inchk) fprintf(stdout,"You are in Checkmate!\n");
    else       fprintf(stdout,"You are in Stalemate!\n");
    if (AnalyseMode) bBreakout = TRUE;
    else return NO_MOVE;
  }

#ifdef BEOSERVER
  // Reset the NodeID count
  NextNodeID = 0;
  fprintf(stdout,"Calculating Approximate NODE Complexities\n");
  // Calculate Node complexities for various depths
  for (n=1;n<5;n++) {
    // Output the details
    fprintf(stdout,"Count Nodes : Depth = %d,  N=%d\n",n,(int)CountMoves(B,n,1));
  }
#endif

   /* Generate the hash key for this position */
  GenerateHashKey(B);

   /* Start the Game Clock */
  SetStartTime();

   /* Set up the necessary algorithm variables */
  RootAlpha = GlobalAlpha; RootBeta = GlobalBeta;
  TBHit = TRUE;

#ifdef BEOSERVER
  /* Set up the Node Table for the Parallel algorithm */
  NodeTable = (NODETABLE *)calloc(sizeof(NODETABLE),NODE_TABLE_SIZE);
  for (n=0;n<NODE_TABLE_SIZE;n++) NodeTable[n].entries = 0;
  /* Set up the minimum parallel depth to 'not defined' */
  SeqDepth= -1;
#endif // BEOSERVER




   /* clear trap vectors */

  memset(TrapVectorScore, 0, sizeof(TrapVectorScore));
  memset(TrapVectorRecorded, 0, sizeof(TrapVectorRecorded));
  
  TrapSet = FALSE; // initially no trap has been set
  
  TrapsFound = 0; // initially no traps have been found


                  /*   ----===   Begin Iterative Deepening Loop   ===----   */
  do {
     // break out if we're in analysis mode and this position is
     // either checkmate or stalemate.  Go straight into a loop
     // waiting for input instead
    if (bBreakout) break;

    GlobalDepth = depth;

    ENABLETRAP = TRUE;

    
     
     /*   --==   Do Search Recursion Here   ==--   */

#ifdef BEOSERVER
     /* If we've spent long enough searching natively then start
      * distributing nodes to the peer nodes.  If we're pondering
      * then don't bother with the parallel search - just fill up
      * the native hash tables.  Don't parallel search easy nodes. */
    if (bParallel && GetElapsedTime() > MIN_SEQ_TIME && !Pondering && !bEasyNode) { 
      if (SeqDepth == -1) {
        SeqDepth = depth;
        // Estimate branching complexity
        ExtendCost   = (float)Nodecount / (float)TopPlyNMoves;
        ExtendCostAv = (float)pow(ExtendCost,1.0f/(float)(depth-1));
        fprintf(stdout,"Averaged Extend Cost = %.3f\n",ExtendCostAv);
        ExtendCost   = (float)Nodecount / (float)LastPlyNodecount;
        fprintf(stdout,"Last Ply Extend Cost = %.3f\n",ExtendCost);
        ExtendCost = min(ExtendCost, ExtendCostAv);
        fprintf(stdout,"Adopted Minimum Value of %.3f\n",ExtendCost);
      }
      // Do the parallel search
      score = RunParallel(B,depth,inchk,InitFifty,GetElapsedTime());
    }
    else
#endif // BEOSERVER

     /* Use the recursive negascout algorithm to find the score.  If we already
      * have an estimate of the true score then use this and search only in a window
      * around it. */
    if (!USE_WINDOW || depth==2)
      score = Search(B,-CMSCORE,CMSCORE,depth*ONEPLY,0,inchk,InitFifty,0,NO_MOVE);

    else {
      RootAlpha = PreviousScore - WINDOW;
      RootBeta = PreviousScore + WINDOW;
      LastPlyNodecount = Nodecount;
      score = Search(B,RootAlpha,RootBeta,depth*ONEPLY,0,inchk,InitFifty,0,NO_MOVE);
      /* If this aspiration search fails low or high then search it properly with
       * safer bounds instead.  Also increase the window size.  */
      if (score<=RootAlpha || score >= RootBeta) {
        if (!AbortFlag  && !IsCM(score)) {
          if (score >= RootBeta && Post) PrintThinking(score,B);
          if (score <= RootAlpha) {RootAlpha = -CMSCORE; RootBeta = score+1;}
          if (score >= RootBeta) {RootBeta = CMSCORE; RootAlpha = score-1;}
          score = Search(B,RootAlpha,RootBeta,depth*ONEPLY,0,inchk,InitFifty,0,NO_MOVE);
          /* If the second search also fails with a cutoff (strangely) then we must
           * do a full re-search with infinite bounds.  The expectation is that this
           * happens extremely rarely.  Of course, it shouldn't happen at all, ideally.
           * The fact that it sometimes does is due to a phenomenon called "search
           * instability", and it's a complete pain!  At least we'll have a full hash
           * table! */
          if (!AbortFlag && (score <= RootAlpha || score >= RootBeta)) {
            RootBeta = CMSCORE;
            RootAlpha = -CMSCORE;
            score = Search(B,RootAlpha,RootBeta,depth*ONEPLY,0,inchk,InitFifty,0,NO_MOVE);
          }
        }
        else if (!AbortFlag && IsCM(score) && Post) PrintThinking(score,B);
      }
      else if (!AbortFlag) PrintedPV = FALSE;
          
          // Is this an easy node? (i.e. a simple recapture move that must be played)
      if (TopOrderScore > NextBestOrderScore + EASY_MOVE_MARGIN && BestMoveRet != NO_MOVE && TopOrderScore == SEE(B,MFrom(BestMoveRet),MTo(BestMoveRet),IsPromote(BestMoveRet)) * 100) {
                  bEasyMove = TRUE;
          }
          else bEasyMove = FALSE;
    }
        
          /* --==  Search Finished ==--  */
        
          /* Probe the hashtable for the suggested best move */
    Entry = HashProbe(B);
    if (Entry) {
      BestMove = Entry->move;
      score = (int)Entry->score;
    }
    else {BestMove = NO_MOVE;fprintf(stdout,"Could Not Find First Ply Hash Entry!\n");}
    if (BestMove == NO_MOVE) {fprintf(stderr,"No Best Move! Assigning previous\n");BestMove = Previous;}
         
     /* If we aborted the search before any score was returned, then reset to the
      * previous ply's move score */
    if (AbortFlag && BestMove == NO_MOVE) score = PreviousScore;
     /* Otherwise store what we've found */
    else {     
      PreviousScore = score;
      Previous = BestMove;
    }
    BestMoveRet = Previous;

     /* Go to the next depth in our iterative deepening loop */
    depth++;
     
     /* Check to see if we should continue the search */
    if (AnalyseMode) Continue = Not(AbortFlag);
    else Continue = ContinueSearch(score,B->side,depth);
#ifdef MAX_DEPTH
  } while (depth <= MAX_DEPTH && IsCM(score)==0 && (TopPlyNMoves>1 || AnalyseMode) && Continue && !TBHit);
#else 
  } while (IsCM(score)==0 && (TopPlyNMoves>1 || AnalyseMode) && Continue && !TBHit);
#endif
                  /*   --==  End Iterative Deepening Loop  ==--   */

   /* Store total time taken in centiseconds */
  TimeTaken = GetElapsedTime();
  
   /* Expire the hash tables if we're playing a game. This involves making all the
    * existing entries 'stale' so that they will be replaced immediately in future,
    * but can still be read OK for the time-being. */
  if (!Params.Test && !AnalyseMode && (Params.Time > 100)) ExpireHash();

   /* If we've not got round to printing off a PV yet (i.e. we've had an early exit
    * before PrintThinking() has been called) then do so now. */
  if (!PrintedPV) {PrintedPV=TRUE;PrintThinking(score,B);}
   
   /* Check to see if we should resign from this position.  Only do this if we've thought
    * for long enough so that the search is reliable and the current position is also
    * losing by at least a pawn, depending on the skill level. */
  if (!Pondering && !Params.Test && Params.Resign && depth>=7 && Params.Time<Params.OTime && InitialScore<(Skill*10)-200)
     resign = CheckResign(score);

   /* Check to see if we should offer a draw from EGTB search */
#ifdef OFFER_TB_DRAW
  if (!Pondering && !Params.Test && XBoard && TBON && ProbeEGTB(B,&score,0)) {
    if (!IsCM(score)) fprintf(stdout,"offer draw\n");
  }
#endif
   
   /* Write the PV to a text string in case we're writing a logfile */
  WritePVToText(B);
  
   /* Print timing and search information */
  if (!XBoard) PrintInfo(score);

   /* Output details of the move chosen, and play that move if necessary */
  if (InputFlag == INPUT_RESIGN) InputFlag = INPUT_STOP;
  if (UCI) {
    fprintf(stdout,"bestmove ");
    PrintMove(Previous,FALSE,stdout);
    fprintf(stdout,"\n");
  }
  if (!UCI && !Pondering && (AutoMove || XBoard) && !resign && !AnalyseMode &&
      InputFlag != INPUT_STOP) {
    if (XBoard) {
      fprintf(stdout,"move ");
      PrintMove(Previous,FALSE,stdout);
      fprintf(stdout,"\n");
    }
    MoveHistory[mvno] = Previous;
    UndoHistory[mvno] = DoMove(&Current_Board,Previous);
    mvno++;
    MoveHistory[mvno] = NO_MOVE;
     /* We've been told to move immediately, we've just moved as required,
      * so flag that this is accomplished */
    if (InputFlag == INPUT_MOVE_NOW) InputFlag = INPUT_NULL;
  }
   
   /* If we're altering the position whilst in analysis mode, then do so */
  if (!Pondering && AnalyseMode) {
     // We got a CM score from this analysis - just wait for more commands
    if (InputFlag == INPUT_NULL) {
      // Wait for something to do
      while ((InputFlag = CheckUserInput()) == INPUT_NULL);
    }
    if (InputFlag == INPUT_MOVE) {
      MoveHistory[mvno] = MoveToPlay;
      UndoHistory[mvno] = DoMove(&Current_Board,MoveToPlay);
      mvno++;
      MoveHistory[mvno] = NO_MOVE;
    }
    if (InputFlag == INPUT_UNDO) {
      mvno--;
      UndoMove(&Current_Board,MoveHistory[mvno],UndoHistory[mvno]);
    }
    if (InputFlag == INPUT_NEW) {
      ResetBoard(&Current_Board);
    }
  }
    
   /* Tidy up */
  if (InputFlag == INPUT_NULL || InputFlag == INPUT_STOP || InputFlag == INPUT_WORK_DONE) AnalyseMode=FALSE;   
  if (IsCM(score)==1) CMFound=TRUE;
  if (Params.Test || AnalyseMode) ResetHash();
#ifdef BEOSERVER
  for (n=0;n<NODE_TABLE_SIZE;n++) {
    if (NodeTable[n].entries > 0) free(NodeTable[n].list);
   }
  free(NodeTable);
#endif // BEOSERVER

  // print out a trap if we set one
  if (TrapSet) {
    printf("Trap was set, and a non-ideal move is being chosen.\n");
  }

  writeTrapData(TrapSet, TrapsFound);

   /* Return the best move that we found */
  return Previous;
}


/* Do the recursive negascout search (with memory).  This works by calling the search
 * function with a zero window bound (beta=alpha+1).  This causes a quick cutoff so we
 * can then do a more accurate search with better defined bounds.  See Prof. Alexander
 * Reinefeld's www page for a paper detailing the algorithm. */
int Search(Board *B,const int alpha, const int beta, int depth, int ply,
                const int inchk, int fifty, int NullDepth, MOVE LastMove) {
  MOVE movelist[MAX_MOVES],m,*lastmove,hashmove=NO_MOVE,bestmove=NO_MOVE,trapMove=NO_MOVE;
  int score = -INFINITY, best = -INFINITY, evalscore=0;
  int NMoves,Moveno,newfifty=fifty+1,ep_carry,p,pto,LegalMoves=0,gchk;
  int newdepth,nullreduction,Base_Extensions=0,Extensions=0,excess=0;
  int EntryType, EntryScore, SEEScore;
  int rawEval, adjEval, dI, TScores[MAX_MOVES], profit, skipCount;
  float Tfactor, trapQuality, bestTrapQuality;
  BOOL TrapNode = (ply == 1);
  BOOL AbortTrap;
  BOOL DoNull = TRUE, IsCapTopMove = FALSE;
  BOOL LocalTrapSet = FALSE;
  FullMove Full[100];
  Undo U;
  BOOL threat=FALSE,Futile=FALSE,IsPV=FALSE,ReduceExtensions=FALSE;
  HashElt *Entry;

   /* Set up the temporary (changeable) values for alpha and beta at this node */
  int talpha = alpha, tbeta=beta;
#ifdef DEBUG_VERIFY
  KeyType CheckKey;
#endif

  /* Debugging information */
#ifdef DEBUG_VERIFY
  CheckKey = B->Key;
  GenerateHashKey(B);
  if (CheckKey != B->Key) {fprintf(stderr,"Warning - Key Corrupted in Search()\n");PrintBoard(*B);while (1);}
#endif

   /* Increase the Node Count */
  Nodecount++;
   
   /* Reset the best move */
  BestMoveRet = NO_MOVE;
   
   /* Reset 'the move on this ply is a capture' flag */
  IsCap[ply]=FALSE;
   
   /*   ------------====     TEST FOR DRAWN POSITIONS     ====--------------- */

   /* See if this is a repeated/drawn position.  Do not test at top ply */
  if (ply>0 && IsDrawn(B,ply,fifty,TRUE)) {
    return ((Current_Board.side==B->side) ? (DRAW_SCORE) : -(DRAW_SCORE));
  }

   /*    -----------====     PROBE ENDGAME TABLEBASES     ====------------- */
   
   /* If we're not on top ply then probe the Endgame Tablebases (if any exist) */
  if (TBON && ply>0 && ProbeEGTB(B,&best,ply)) {
    return best;
  }
  if (ply>0) TBHit = FALSE;

   /*      -----------====     PROBE TRANSPOSITION TABLE     ====------------  */
   
   /* See if this position is in the hash table */
  Entry = HashProbe(B);
   
   /*   ------------====     GET INFORMATION FROM HASH TABLE     ====------------- */
   
   /* See what we can learn from the hash table entry (if any exists). */
  if (USE_HASH  && Entry) {
     /* Get the suggested best move from the hash entry (if there is one) */
    hashmove = Entry->move;
    EntryType = GetHashType(Entry->flags);
    EntryScore = (int)Entry->score;
     /* Only accept this hash entry if the level to which it was initially searched was
      * greater than or equal to the current depth. Don't check at ply==0 so that we're
      * sure we can get a return value. */
    if (ply>0 && ((int)Entry->depth>=depth || (EntryType == HASH_EXACT && IsCM(EntryScore) == 1))) {
       /* This was an exact entry so return it */
      switch(EntryType) {
        case (HASH_EXACT) : return EntryScore;
         /* This was an upper bound, but still isn't greater than alpha, so return a fail-low */
        case (HASH_UPPER) : if (EntryScore <= talpha) return EntryScore;
         /* This was an upper bound, but was greater than alpha, so adjust beta if necessary */
                                   if (EntryScore < tbeta)   tbeta = EntryScore; break;
         /* This was a lower bound, but was still greater than beta, so return a fail-high */
        case (HASH_LOWER) : if (EntryScore >= tbeta)  return EntryScore;
         /* This was a lower bound, but was not greater than beta, so adjust alpha if necessary */
                                   if (EntryScore > talpha)  talpha = EntryScore; break;
         /* Check to see if we should avoid null moves */
              case (HASH_NULL)  : DoNull=FALSE; break;
      }
    }
  }

   /*    -----------====     TRY A NULL MOVE     ====------------- */
   
   /* Perform a NULL move if;
    * (1) We're not on the first 2 plies
    * (2) We are OK to NULL move at this position (from the hash table)
    * (3) We haven't done a NULL move already
    * (4) We're not in check
    * (5) There are enough pieces left to avoid high risk of zugzwang
    * (6) We are not rushed 
    * (7) We're not on a leaf node */
  if (USE_NULL && ply>1 && DoNull && NullDepth == 0 && !inchk && (nullreduction=NullOK(B,depth)) && !bRushed && depth > ONEPLY) {
     /* Increase the NULL reduction by one ply to account for the fact that we've passed
      * on this move. */
//    nullreduction += ONEPLY;
     /* Set up temporary flags to store the board position */
    ep_carry=B->ep;
    B->ep=-1;

     /* Do the null move reduced-depth search */
    B->side=Opponent(B->side);
    if (depth-nullreduction<ONEPLY) score = -Quiesce(B,-tbeta,-tbeta+1,ply+1,0,0);
    else score = -Search(B,-tbeta,-tbeta+1,depth-nullreduction,ply+1,0,0,NullDepth+1,NO_MOVE);

     /* Put things back */
    B->side=Opponent(B->side);

     /* Verification Search.  Here we re-search this node at a shallower depth
      * if the score is >= beta.  The reason we do this is simple: we want to make sure
      * that this really is a strong node, and therefore worth chopping. This time, 
      * instead of actually making a NULL move, we play a proper move. Note that
      * we don't bother doing this when close to the frontier nodes (this would take
      * us into the qsearch).  We only fail high if this second search ALSO fails
      * high. */
    if (score >= tbeta && depth-nullreduction >= ONEPLY)
      score = Search(B,tbeta-1,tbeta,depth-nullreduction,ply+1,0,fifty,NullDepth+1,LastMove);

     /* Replace the En-Passant flag */
    B->ep=ep_carry;
     
     /* If this move returned CM then we must be careful because there is a
      * CM threat dangerously close! Only do this on highest skill level. */
    if (IsCM(score)==-1 && Skill==10) threat=TRUE;
     
     /* A fail high means that the positional score here is so high that even if the
      * opponent is given a 'free move' then he still can't improve his position enough
      * to increase the value of alpha.  If this is the case then clearly the current
      * position is very strong.  In fact it is so strong that the opponent would never
      * let it occur in the first place, so we should cause a cut off.  */
    if (score >= tbeta) {
      HashUpdate(B,score,BestMoveRet,depth+ONEPLY-nullreduction,HASH_LOWER,FALSE,ply);
      return score;
    }
  }

   
   /*     ----------====     INTERNAL ITERATIVE DEEPENING     ====----------- */
   
   /* If we're not doing a NULL move and we don't have a hash move and we're at least 3
    * ply away from the quiescence search, then try to get a good guess for the best move
    * by doing a shallower search from here. */
  if (USE_IID && hashmove==NO_MOVE && NullDepth==0 && depth >= THREEPLY && Skill>8 && !bRushed) {
    score = Search(B,talpha,tbeta,depth - TWOPLY,ply,inchk,0,0,LastMove);
     /* Re-search properly if the previous search failed low, so that we know we're getting
      * a good move, not just the move with the highest upper bound (which is essentially
      * random and depends on the search order.) */
    if (score<=talpha) score = Search(B,-CMSCORE,talpha+1,depth - TWOPLY,ply,inchk,0,0,LastMove);
     /* Get the suggested best move that was returned and use it as a hashmove */ 
    hashmove = BestMoveRet;
  }
   

   /*     ----------====     GENERATE MOVELIST     ====------------  */

   /* Make some moves */
  if (!inchk) lastmove = GenerateMoves(B,B->side,movelist);
  else lastmove = GenerateCheckEvasions(B,B->side,movelist,inchk);

   /* Score the moves in the preliminary movelist.  Don't remove illegal moves just yet,
    * unless we're in check (needed for single-reply extension). If we're searching
    * the root then order moves according to root scores instead */
  NMoves = FilterMovelist(movelist,lastmove,B,Full,ply,hashmove,inchk);
   /* No moves - stalemate */
  BestMoveRet=NO_MOVE;
  if (NMoves == 0) return ((Current_Board.side==B->side) ? (DRAW_SCORE) : -(DRAW_SCORE));

   /*    -----------====     SEARCH EXTENSIONS     ====------------ */
   
   /* CM Threat extensions.  We extend here if a NULL move search
    * returned a checkmate threat. */
  if (threat)    {Base_Extensions += CMTHREAT_EXTEND; ThreatExtensions++;}
   /* Single Reply Extension.  If there's only one possible move then extend. */
  if (NMoves == 1) {Base_Extensions += ONEREPLY_EXTEND; OneReplyExtensions++;}

   /* Current Approximate Positional Score (for Razoring) if it might be needed */
  if (USE_RAZORING && !inchk && !threat && ply>1) {
    if (IsDrawnMaterial(B)) evalscore = (Current_Board.side == WHITE ? DRAW_SCORE : -DRAW_SCORE);
    else evalscore = LazyEval(B) + IsWonGame(B);
    if (B->side==BLACK) evalscore = -evalscore;
  }
  /* Check to see if we should reduce extensions here as we're getting way too deep.
   * Start doing this after we've extended by a total of MAX_EXTEND ply, or we've
   * searched to twice the default depth, whichever is larger.  Put the best move
   * first. */
  ReduceExtensions = (ply > (GlobalDepth + max(GlobalDepth,MAX_EXTEND)));
   
  
  /*     -----------====     CYCLE THROUGH MOVES - INNER LOOP     ====---------- */

  for (Moveno = 0 ; Moveno < NMoves ; Moveno++) {

    TrapSet = FALSE; // set global trapset to false, then see if recursive calls change it

    /* Get the highest scoring move from those left on the list.  Put it at the top. */
#ifdef NO_ORDER
    if (ply != 1)
#endif
      SortFrom(Full,Moveno,NMoves);
    
    m = Full[Moveno].move;

    /* Keep track of the top ply move */
    if (ply==0 && (depth == (GlobalDepth*ONEPLY))) {
      /* Work out the SEE score for this move */
      SEEScore = SEE(B,MFrom(m),MTo(m),IsPromote(m)) * 100;
      /* Set up the top two (the hash move and the largest competitor) */
      if (Moveno == 0) TopOrderScore = SEEScore;
      else if (SEEScore > NextBestOrderScore || Moveno == 1) NextBestOrderScore = SEEScore;
      /* Set the top ply move that we're working on */
      TopPlyMoveno = Moveno;
    }

    /* Identify the piece that's moving, and the piece on the target square */
    p   = PType(B->pieces[MFrom(m)]);
    pto = PType(B->pieces[MTo(m)]);

     /* Do the move */
    U = DoMove(B,m);


     /* Filter out illegal moves which leave us in check.
      * If we're in check before the move then this has already been done. */
    if (!inchk && InCheck(B,Opponent(B->side))) {
      UndoMove(B,m,U);
      continue;
    }
     /* This move is legal, so increase the count */
    LegalMoves++;

     /* Test to see if this move gives check */
    gchk = GivesCheck(B,m);

     /* Test for immediate CM cutoff */
    if (gchk && InCM(B,gchk)) {
      UndoMove(B,m,U);
      HashUpdate(B,CMSCORE-ply-1,m,depth,HASH_EXACT,FALSE,ply);
      IncreaseCounts(Full[Moveno],ply,CMSCORE,B->side);
      BestMoveRet = m;
      if (LegalMoves==1) BestFirst++;
      SortNodes++;
      if (Post && ply==0) PrintThinking(CMSCORE-ply-1,B);
      return CMSCORE - ply - 1;
    }
    
     /* Test to see if this move breaks the 50 move draw chain, else increase the count */
    if (pto!=empty || IsEP(m)) IsCap[ply] = TRUE;
    else IsCap[ply] = FALSE;
    if (IsCastle(m) || IsCap[ply] || p==pawn) newfifty = 0;
    else newfifty = fifty + 1;
 
     /*  -------------====     MORE EXTENSIONS     ====------------------ */
     
    Extensions = Base_Extensions;
     
     /* Check extension (if we're giving check this move) */
    if (gchk && Skill>2) {
      Extensions += CHECK_EXTEND;
      CheckExtensions++;
       /* Revealed Check Extension (Not 100% accurate, but near enough)
        * Test to see if the piece giving check is not the piece moving.
        * (This fails if the piece moving also gives check, and is tested first.) */
      if (gchk != p) {
        Extensions += REVCHECK_EXTEND;
        RevCheckExtensions++;
      }
    }     
     
     /* Recapture Extension.  We do this in the following cases;
      * (1) We are not on the first ply (obviously)
      * (2) The last ply was a capture move
      * (3) This move captures the piece that captured last move 
      * (4) The value of this piece equals the value of the target piece */
    if (ply>0 && IsCap[ply-1] && MTo(m) == MTo(LastMove) && PieceValue[pto] == PieceValue[p] && Skill>4) {
      Extensions += RECAP_EXTEND;
      RecapExtensions++;
    }

     /* A Pawn Push Extension is used if we're moving a pawn to
      * within one rank of promotion. */
    if (p==pawn && (MTo(m)<=h7 || MTo(m)>=a2) && Skill>7) {
      Extensions += PAWNPUSH_EXTEND;
      PawnPushExtensions++;
    }
     
    /* Fork extension */
    if (p == wknight && Count(KnightMoves[MTo(m)]&(B->BlackQueens|B->BlackRooks|Mask[B->BlackKing]))>1) Extensions += ONEPLY;
    if (p == bknight && Count(KnightMoves[MTo(m)]&(B->WhiteQueens|B->WhiteRooks|Mask[B->WhiteKing]))>1) Extensions += ONEPLY;

    /* Seemingly bad top ply moves are reduced when there is an obvious easy move.
     * Do not do this if we are capturing or giving check, or if this is a shallow search. */
    if (ply == 0 && bEasyMove && newdepth >= THREEPLY) {
      if (Moveno == 0) IsCapTopMove = IsCap[ply];
      if (IsCap[ply] && SEEScore + EASY_MOVE_MARGIN < TopOrderScore && m != Killer1[ply] && m != Killer2[ply]) Extensions -= ONEPLY;
    }
    
    /* Reduce Extensions if we're searching too deep already */
    if (ReduceExtensions) Extensions /= 2;
     /* Add on the extensions, limiting them to 1.5 plies maximum,
      * plus reduce the depth to the next ply. */
    newdepth = depth + min(Extensions,ONEANDAHALFPLY) - ONEPLY;
     
     /*  ----------------====     PRUNING     ====-------------------- */
     
     /* Check to see if this node might be futile.  This is used in the subsequent algorithm.
      * This is true if;
      * (1) We are not in check, nor is this a checking move
      * (2) We are not in danger of losing to CM
      * (3) We are not capturing or promoting on this move
      * (4) Alpha is not a CM score, either for or against
      * (5) We are not on the first four ply, so that the search has had a chance to go 
      *     bad. */
    if (USE_RAZORING && !inchk && !gchk && !threat && !IsCap[ply] &&
        !IsPromote(m) && !IsCM(talpha) && ply>3)  Futile=TRUE;
    else Futile=FALSE;
     
     /* Adaptive razoring.  This is similar to futility pruning (see Heinz et al
      * in various papers), except that it works at several plies, not just
      * frontier and pre-frontier nodes.
      * If the evaluation is significantly below alpha, and this move is futile
      * (see above) then we reduce the depth to which we wish to search this node.
      * The margin we use increases with increasing depth.  Clearly, we need to be
      * much more careful about high-depth nodes as pruning them could have much
      * larger consequences.
      * This algorithm is a bit dodgy, but generally seems to work OK. */
    if (Futile && LegalMoves>AVOID_RAZ_NUM && newdepth >= ONEPLY) {
      excess = talpha - (evalscore + ((newdepth*newdepth*RAZOR_SCALE)/10) + RAZOR_MARGIN);
      if (excess > 0) {
         /* If this is well below alpha then prune by a whole ply, otherwise just
          * prune by a fraction of a ply. */
        if (excess > RAZOR_HARSH) newdepth -= ONEPLY;
        else newdepth -= ((ONEPLY * excess) / RAZOR_HARSH);
        RazorCuts++;
      }
    }
     if (ply == 0) { topmove = m; }
   
          /* -----------------====     RECURSE TO THE NEXT PLY     ====---------------- */
     
     /* If at bottom depth then return quiescence score.  Basically, we find a quiet
      * position before we accept the static evaluation.   This avoids the so-called
      * 'horizon effect' where bad captures are pushed off the end of the search tree
      * causing total mis-evaluation of the position.  (See notes at top of Quiesce()
      * function below). */

    if (newdepth<ONEPLY) {
      score = - Quiesce(B,-tbeta,-talpha,ply+1,newfifty, gchk);
    /* Recurse to the next ply using negascout search heuristic */
    } else {
        /* If this is the first move then search it properly */
#ifdef USE_PV_SEARCH
          if (LegalMoves==1)
#endif
        score = - Search(B,-tbeta,-talpha,newdepth,ply+1,gchk,newfifty,0,m);

       /* Otherwise this is NOT the first move - use Negascout search */
#ifdef USE_PV_SEARCH
      else {
             /* First try a zero-width search.  This tests if this position is going
              * to improve alpha, but does not return an exact score.  It fails much
          * more quickly in the (hopefully more probable) case that the move does
          * NOT improve alpha. */
        score = - Search(B,-talpha-1,-talpha,newdepth,ply+1,gchk,newfifty,0,m);
               /* Looks like this move will improve alpha */
        if (score>talpha && score<tbeta && !AbortFlag)
                 /* .. therefore search again with sensible bounds */
                score = - Search(B,-tbeta,-score,newdepth,ply+1,gchk,newfifty,0,m);
      }
#endif
    }
#ifdef DEBUG_VERIFY
  CheckKey = B->Key;
  GenerateHashKey(B);
  if (CheckKey != B->Key) {fprintf(stderr,"Warning - Key Corrupted in Search() iteration step, ply=%d, depth=%d\n",ply,depth);PrintBoard(*B);while (1);}
#endif

     /* Undo the move */
    UndoMove(B,m,U);
    
    if (ply <= MAX_TRAP_DEPTH && ply % 2 == 1) {
      TrapVectorScore[MFrom(m)][MTo(m)][GlobalDepth][ply][B->Key % TRAP_KEY_SIZE] = score;
      TrapVectorRecorded[MFrom(m)][MTo(m)][GlobalDepth][ply][B->Key % TRAP_KEY_SIZE] = TRUE;
    }

     /*  ---------------====     HAVE WE IMPROVED OUR BEST SCORE?     ====------------- */

     /* Update scores */
    if (!AbortFlag && score>best) {
      best = score;
      bestmove = m;
      if (ply == 0 && GlobalDepth >= MAX_DEPTH) {
        printf("*** BEST: ");
        PrintMove(bestmove, TRUE, stdout);
        printf(" %d", best);
        printf("\n");
      }
      LocalTrapSet = TrapSet; // record if a deep trap is changing the move

       /* Have we improved alpha? (i.e. this an interesting move) */
#ifdef NO_AB
      if (ply > 1 && best>talpha) {
#else
      if (best>talpha) {
#endif
              IncreaseCounts(Full[Moveno],ply,score,B->side);
               /* Fail-high cutoff.  This means that this move is so good that it will
                * never be played as our opponent will never allow it (he/she has better
                * alternatives) so don't bother continuing the search. */
        if (score>=tbeta) {
           /* Update the hash table & return the move score */
          BestMoveRet = bestmove;
                SortNodes++;
          HashUpdate(B,score,bestmove,depth,HASH_LOWER,threat,ply);
                if (LegalMoves==1) BestFirst++;
                return best;
        }
        /* Print off the PV if we're posting to XBoard and this is the top ply and
         * we're not doing the IID loop. */
        if (ply==0 && (depth == (GlobalDepth*ONEPLY))) {
          HashUpdate(B,score,bestmove,depth,HASH_EXACT,threat,ply);
          if (Post) PrintThinking(score,B);
        }
        /* We know we're in the PV Now */
        IsPV=TRUE;
        /* We know that this score > alpha, so improve (the temporary) alpha */
        talpha=best;
      }
       /* Always store the first top ply entries (to make sure that we have something
        * that we can return).  Don't store every move because if we get a fail-
        * low in the window search it might well beat the best move so far just
        * by fluke - we might score Move1<100 and Move2<200, giving Move2 a
        * better score, but of course the reality could be that Move1=99, Move2=0. 
        * If we're getting only fail-lows in the window search then we need to
        * re-search the root position anyway, using the previous best move (the one
        * searched first) at the top of the list. See also the upper bound hash stores
        * below in the hash table update section. */
      else if (ply==0 && LegalMoves==1 && (depth == (GlobalDepth*ONEPLY))) {
        HashUpdate(B,score,bestmove,depth,HASH_UPPER,threat,ply);
        if (Post) PrintThinking(score,B);
      }
    }

    /* Check for time problems and new input periodically */
    if (!AbortFlag && ((Nodecount&INPUT_PERIOD)==0 || ply<4) && !bRushed) {
      if (GlobalDepth>Params.Depth && !AnalyseMode) AbortFlag = CheckTime(ply,score);
      if (!AbortFlag) {
        InputFlag = CheckUserInput();
        if (InputFlag && InputFlag != INPUT_WORK_DONE) AbortFlag = TRUE;
      }
    }
    if (AbortFlag) break;
  }



   /* Check for stalemate.  Note that we've already filtered out CMs at the top of
    * this function. */
  if (LegalMoves == 0) {
    best = ((Current_Board.side==B->side) ? (DRAW_SCORE) : -(DRAW_SCORE));
    bestmove = NO_MOVE;
  }


  TrapSet = LocalTrapSet;


        /******************************************************************************************** 
         * TRAPPY MINIMAX CODE
         * BEWARE: HERE BE DRAGONS
         *
         * This code is a mess. I apologize. I edit and re-edit it all the time
         * and commit randomly. It's in no state to be read by anyone,
         * probably even me. 
         ********************************************************************************************/



#ifdef MAX_DEPTH
#if TRAPPY == 1
  //printf(" TRAPPY? %d\n", GlobalDepth);
  /* Calculate trappiness */
  if (!AbortFlag && GlobalDepth >= MAX_DEPTH && ply < GlobalDepth && ply % 2 == 1 && ply <= MAX_TRAP_DEPTH && TopPlyMoveno != 0) {
                /* I'm still not clear on why the minimax paper had code for
                 * calculating the trappiness of even ply nodes, so I skip
                 * them.
                 */
    rawEval = best;
    bestTrapQuality = 0;

    for (Moveno = 0 ; Moveno < NMoves ; Moveno++) {
      m = Full[Moveno].move;
      if (m == bestmove) continue;
      U = DoMove(B,m);
      GenerateHashKey(B);
      AbortTrap = FALSE; // *try* to find a trap, but give up if we don't have enough information
      skipCount = 0;
      for (dI = GlobalDepth - 2; !AbortTrap && dI >= 0; dI--) {
        if (TrapVectorRecorded[MFrom(m)][MTo(m)][dI+2][ply][B->Key % TRAP_KEY_SIZE]) {
          TScores[dI] = TrapVectorScore[MFrom(m)][MTo(m)][dI+2][ply][B->Key % TRAP_KEY_SIZE];
        } else {
          // I have not convinced myself that this actually works:
          // try to "fill in" holes in the TScores array by borrowing
          // the next-higher value.
          if (dI < GlobalDepth - 2 && skipCount < MAX_DEPTH - 3) {
            skipCount++;
            TScores[dI] = TScores[dI+1];
          } else {
            AbortTrap = TRUE; // time to give up
            break;
          }
        }
      }
      UndoMove(B,m,U);
      if (AbortTrap) {
        continue;
      }
      Tfactor = trappiness(TScores[GlobalDepth-2], TScores, GlobalDepth - 2, ply);
      if (Tfactor == 0) { continue; }
      profit = rawEval - TScores[GlobalDepth-2];
      trapQuality = profit * Tfactor;
      //printf("Tfactor = %f, tQ = %f, rawEval = %d, TScore=%d\n",Tfactor,trapQuality,rawEval,TScores[GlobalDepth-2]);
      if (trapQuality > bestTrapQuality) {
        printf("New best trap: tq = %f\n", trapQuality);
        PrintMove(m, TRUE, stdout);
        printf("\n");
        for (dI = 0; dI < GlobalDepth-1; dI++) {
          printf("TScores[%d] = %d\n", dI+2, TScores[dI]);
        }
        bestTrapQuality = trapQuality;
        trapMove = m;
      }
    }
    if (bestTrapQuality != 0 && bestmove != trapMove) { 
      adjEval = rawEval - ceil(scale(bestTrapQuality, rawEval));
      printf("Top move: ");
      PrintMove(topmove, TRUE, stdout); 
      printf("; Current best: ");
      PrintMove(bestmove, TRUE, stdout);
      printf("\n");
      printf("Best move score %d\n", best);
      printf("New move: ");
      PrintMove(trapMove, TRUE, stdout);
      printf("; Adding %d to score for ply %d move to yield %d.\n", adjEval-rawEval, ply, adjEval);
      printf("---\n");
      WriteBoardDataLite(Current_Board, topmove, bestmove, trapMove, rawEval, adjEval, profit);
      TrapsFound++;
      best = adjEval;
      bestmove = trapMove;
      TrapSet = TRUE;
    }
  }

#endif
#endif

   
   /*  -------------====     UPDATE THE HASH TABLE     ====--------------  */
   
   /* We store the hashtable results as an exact value or an upper bound depending on
    * whether we're in the PV or not.  If we have found a move that increased alpha
    * then this move is in the PV and hence is an exact score.  Otherwise, all
    * moves in this level are a result of fail-high cutoffs from the next ply, and hence
    * all we really have is an upper bound on the best score.  We know we can't get any
    * better than this, but we don't really know how bad the score truly is.  All that
    * matters is that the best we could possibly get isn't good enough. Don't store 
    * top ply (ply=0) fail-low moves here because a fail low in the window search can
    * cause bad moves to be stored instead of the best move so far.  This is because we
    * get only bounds, and one lower bound being higher than another tells us nothing
    * about the actual scores themselves. (We DO score ply=0 moves if this is the first 
    * iteration, however, to make sure that *something* is stored!). */
  if (!AbortFlag) {
    if      (IsPV)   HashUpdate(B,best,bestmove,depth,HASH_EXACT,threat,ply);
    else if (ply>0 || GlobalDepth == 2)
                     HashUpdate(B,best,bestmove,depth,HASH_UPPER,threat,ply);
  }
   
   /* If we've run out of time then print the thinking as far as we've got.  Don't bother
    * if we didn't actually finish searching the first move, though.  Also don't bother if the
    * opponent has just resigned. */
  if (AbortFlag && ply==0 && depth == (GlobalDepth*ONEPLY) && best>-INFINITY && InputFlag != INPUT_RESIGN)
    PrintThinking(best,B);
 
   /* Return the best value found */
  BestMoveRet = bestmove;
  return best;
}


 /*       --------------====     QUIESCENCE SEARCH     ====------------      */


/* Do the quiescence search.  This is a clever trick used by ALL chess programs to
 * avoid the so-called 'Horizon Effect'.  This is where a terrible capture is pushed
 * off the end of the search tree and therefore missed.  The quiescence search simply
 * continues the search and only evaluates the board when all is quiet and calm.
 * In the Quiescence search, we continue the a-b tree search, but consider only capturing
 * moves.  If we find ourselves in check here then we simply ignore it and just
 * generate captures anyway.  At each point we allow ourselves the choice of making
 * no move at all (on the expectation that there might be a better move which is
 * NOT a capture, so therefore *forcing* a capture might give spurious results.) */
int Quiesce(Board *B, const int alpha, const int beta, int ply, int fifty, int inchk) {
  int NMoves,Moveno,newfifty=fifty,gchk=0;
  MOVE movelist[MAX_MOVES],m,*lastmove,hashmove=NO_MOVE,bestmove=NO_MOVE;
  int score, best, talpha=alpha, tbeta=beta, EntryScore;
  FullMove Full[100];
  HashElt *Entry;
  BOOL IsPV=FALSE, Filtered=FALSE;
  Undo U;

   /* Increase the Node Count */
  Qnodes++;
     
   /* Check for draw */
  if (IsDrawn(B,ply,fifty,FALSE))
    return ((Current_Board.side==B->side) ? (DRAW_SCORE) : -(DRAW_SCORE));

   /* As this is quiescence search, we can choose to stand pat if we aren't currently
    * trying to get out of check.  Standing pat basically means that during the quiescence
    * search, there are positions that arise where the best move is not a capture.  It
    * might be that several captures exist, but they all lose material.  In these cases,
    * we give the computer a chance to play none of the (aggressive) moves and just accept
    * the static evaluation score at this position instead. */
#ifdef QUIESCENCE_CHECK_EVASIONS
   /* If we're evading check then don't give the option of standing pat */
  if (inchk) best = -INFINITY;
  else {
#endif
    best = Eval(B,-INFINITY,tbeta);
#ifdef QUIESCENCE_CHECK_EVASIONS
  }
#endif   

   /* Perform a stand-pat cutoff.  Basically, we always allow the option of
    * NOT performing a capture move.  This is because it might be that all possible
    * capture moves we try are bad as they all lose material.  Because we have only
    * considered capturing moves, it would be stupid to force the computer to accept 
    * one of them, simply because there might be a perfectly good quiescent move that
    * we have not even looked at. */
  if (best > talpha) {
    if (best>=tbeta) return best;
    talpha = best;
  }
   
   /*      -----------====     PROBE TRANSPOSITION TABLE     ====------------  */
   
   /* See if this position is in the hash table */
  Entry = HashProbe(B);
   
   /*   ------------====     GET INFORMATION FROM HASH TABLE     ====------------- */
   
   /* See what we can learn from the hash table entry (if any exists) */
  if (USE_HASH && Entry) {
     /* Get the suggested best move from the hash entry (if there is one) */
    hashmove = Entry->move;
    EntryScore = (int)Entry->score;
     /* This was an exact entry so return it */
    switch(GetHashType(Entry->flags)) {
      case (HASH_EXACT) : return EntryScore;
       /* This was an upper bound, but still isn't greater than alpha, so return a fail-low */
      case (HASH_UPPER) : if (EntryScore <= talpha) return EntryScore;
       /* This was an upper bound, but was greater than alpha, so adjust beta if necessary */
                          if (EntryScore < tbeta)   tbeta = EntryScore; break;
       /* This was a lower bound, but was still greater than beta, so return a fail-high */
      case (HASH_LOWER) : if (EntryScore >= tbeta)  return EntryScore;
       /* This was a lower bound, but was not greater than beta, so adjust alpha if necessary */
                          if (EntryScore > talpha)  talpha = EntryScore; break;
       /* Check to see if we should avoid null moves */
      case (HASH_NULL)  : break;
    }
  }

   /* If this is too deep then just return the stand-pat score to avoid long
    * quiescence searches. */
#ifndef NO_QUIESCE
  if ((ply - GlobalDepth) > MAX_QUI) {
    if (!inchk) return best;
    return Eval(B,-INFINITY,tbeta);
  }
#else
    if (!inchk) return best;
    return Eval(B,-INFINITY,tbeta);
#endif
   
#ifdef QUIESCENCE_CHECK_EVASIONS
   /* Generate check evasions if we're in check */
  if (inchk) {
    lastmove = GenerateCheckEvasions(B,B->side,movelist,inchk);
    NMoves = FilterMovelist(movelist,lastmove,B,Full,ply,hashmove,inchk);
    Filtered=TRUE; /* We've filtered out illegal moves */
  }
  else {
#endif
   /* Generate all (not necessarily legal) non-quiescent moves */
  lastmove = GenerateCaptures(B,B->side,movelist);
           //GenerateNonQuiescentMoves(B,B->side,movelist);

   /* Score and filter the movelist.  This function is optimised to
    * order capture moves. talpha-best = delta (the amount that the
    * position score needs to be improved in order to improve alpha).
    * Put the best move first. */
  NMoves = FilterMovelistQui(movelist,lastmove,B,Full,ply,talpha-best,hashmove);
#ifdef QUIESCENCE_CHECK_EVASIONS
  }
#endif
   
   /*     -----------====     CYCLE THROUGH MOVES - INNER LOOP     ====---------- */
  for (Moveno = 0 ; Moveno < NMoves ; Moveno++) {
     
     /* Get the highest scoring move from those left on the list.  Put it at the top. */
    SortFrom(Full,Moveno,NMoves);
    m = Full[Moveno].move;

     /* Check to see if this move breaks the 50 move draw chain, else increase the count */
    if (IsCastle(m) || (B->pieces[MTo(m)]) || (PType(B->pieces[MFrom(m)])==pawn)) newfifty = 0;
    else newfifty = fifty+1;
     
     /* After a certain depth, filter out the non-capturing moves, unless we're evading check */
    if (ply-GlobalDepth > 3 && B->pieces[MTo(m)]==empty && !inchk) continue;
    
     /* Do the move */
    U = DoMove(B,m);

    /* Check that this is a legal move, provided illegal moves have not already
     * been filtered out. */
    if (!Filtered && InCheck(B,Opponent(B->side))) {
      UndoMove(B,m,U);
      continue;
    }
    
     /* Test to see if this move gives check */
    gchk = GivesCheck(B,m);
     /* Test for immediate CM cutoff */
    if (gchk && InCM(B,gchk)) {
      UndoMove(B,m,U);
#ifdef QUIESCENCE_STORE_CUTS
       /* Store beta cut nodes in the hash table */
      if (QStore) HashUpdate(B,CMSCORE-ply-1,m,1,HASH_EXACT,FALSE,ply);
#endif
      IncreaseCounts(Full[Moveno],ply,CMSCORE,B->side);
      return CMSCORE - ply - 1;
    }
     /* Recurse to the next ply */
#ifdef QUIESCENCE_EVASION_ALL
    score = -Quiesce(B,-tbeta,-talpha,ply+1,newfifty,gchk);
#else
    score = -Quiesce(B,-tbeta,-talpha,ply+1,newfifty,0);
#endif
    UndoMove(B,m,U);

     /* Update scores */
    if (score>best) {
      best = score;
      bestmove = m;
      if (best>talpha) {
        IncreaseCounts(Full[Moveno],ply,score,B->side);
               /* Fail-high cutoff */
        if (best>=tbeta) {
#ifdef QUIESCENCE_STORE_CUTS
           /* Store beta cut nodes in the hash table */
          if (QStore) HashUpdate(B,score,bestmove,1,HASH_LOWER,FALSE,ply);
#endif
          return best;
        }
        IsPV=TRUE;
        talpha=best;
      }
    }
  }

#ifdef QUIESCENCE_STORE_ALL
  if (QStoreAll) {
     /* Update hash table if this isn't a beta cut node */
    if (IsPV) HashUpdate(B,best,bestmove,1,HASH_EXACT,FALSE,ply);
    else      HashUpdate(B,best,bestmove,1,HASH_UPPER,FALSE,ply);
  }
#endif
  
   /* Return the best value found */
  return best;
}


/* Filter the given movelist.  Perform simple scoring heuristics to work out which
 * moves are likely to be the best.  Don't bother filtering out illegal moves at this
 * point, except illegal castles and illegal check evasions.  Put the best move at the
 * front.The order is approximately the following;
 * (1) Hash moves
 * (2) Captures which gain material, best first.
 * (3) Captures which maintain material balance.
 * (4) Killer Moves.
 * (5) Promotions, most valuable first.
 * (6) Captures which appear to lose material, best first.
 * (7) All other moves, sorted by history score and a few other things.
 */
int FilterMovelist(MOVE *movelist,MOVE *lastmove,Board *B, FullMove *Full,const int ply,
                   const MOVE hashmove,const int inchk) {
  MOVE *move = movelist;
  int score,bestscore=0,swapgain;
  int p,from,to,nmoves=0,capt,promote,foundhash=-1;
  Undo U;
  BOOL Illegal = FALSE;

  while (move<lastmove) {
     /* Set up some variables which tell us about the properties of the move.
      * These macros are from the common.h header file. */
    from = MFrom(*move); to = MTo(*move);
    p = PType(B->pieces[from]);
    capt = PType(B->pieces[to]);
    promote = PromotePiece[IsPromote(*move)];
     /* Set up capture piece properly for en-passant captures */
    if (IsEP(*move)) capt=pawn;
       
     /* Filter out illegal castling moves (due to intermediate check) */
    if (IsCastle(*move) && !IsLegalCastle(B,*move)) {move++;continue;}
     
     /* Reset the move ordering score */ 
    score=0;

    /* Filter out illegal check evasions. */
    if (inchk) {
      U=DoMove(B,*move);
      if (InCheck(B,Opponent(B->side))) Illegal = TRUE;
      else Illegal = FALSE;
      UndoMove(B,*move,U);
      if (Illegal) {move++;continue;}
    }
    
            /* -- Calculate move-ordering score for this move -- */
       
     /* Score bonus for a capture.  Uses MVV/LVA algorithm - increase the score the 
      * more valuable the target value is, and decrease score the higher the value of
      * the capturing piece. This ensures that we order highly moves which capture
      * valuable pieces with less-valuable pieces. If we're capturing a less valuable
      * piece then use the SEE to see if the exchange is wise. */
    if (capt) {
      swapgain=0;
      if (USE_SEE==0) {
        score += PieceValue100[capt] - PieceValue10[p];
        switch(promote) {
           // No promotion
          case 0 : break;
           // Reward queen promotions
          case 5 : score += QUEEN_SCORE; break;
           // Penalise promotion moves which don't promote to queen
          default: score -= KNIGHT_SCORE;
        }
      }
      else {swapgain = SEE(B,from,to,promote); score += swapgain*100;}
       /* Reward good captures */
      if (swapgain>=0 && Skill>3) score += GOOD_CAPTURE;
    }
     
     /* Increase the score if this move is a killer move */
    if (USE_KILLERS && Skill>4) {
      if      (*move == MateKiller[ply]) score += MATEKILLER;
      else if (*move == Killer1[ply])    score += KILLER1;
      else if (*move == Killer2[ply])    score += KILLER2;
    }

     /* If this is a boring move then penalise it so that it gets searched after all
      * the interesting ones, regardless of history score. */
    if (score == 0) score = -ORDINARY_MOVE;
     
     /* Add on history score */
    if (USE_HISTORY && Skill>5) score += History[from][to];
     
     /* Random Offset for Skill Levels */
    if (Skill<10) score += Random((10-Skill)*8);

    /* Keep track of this move if it is the best move */
    if (*move == hashmove) foundhash=nmoves;
    
     /* Update the score */
    Full[nmoves].move = *move;       
    Full[nmoves].score = score;
    if (score > bestscore) bestscore = score;
    nmoves++;

     /* Get the next move to test */
    move++;
  }

   /* Score the hash move most highly */
  if (foundhash>-1) Full[foundhash].score = bestscore + 1;

   /* Return the number of moves */
  return nmoves;
}


/* Filter the given movelist from the quiescence search, removing some illegal moves.
 * Return the number of semilegal moves.  We remember that several moves are cut
 * here because of Delta cuts and SEE cuts. */
int FilterMovelistQui(MOVE *movelist,MOVE *lastmove,Board *B, FullMove *Full,const int ply,
                      const int delta, MOVE hashmove) {
  MOVE *move = movelist;
  Undo U;
  int score,evalscore,bestscore=0,foundhash=-1;
  int p,from,to,nmoves=0,capt,promote=0,swapgain=0;

  while (move<lastmove) {
     
    /* Set up some variables */
    from = MFrom(*move); to = MTo(*move);
    p = PType(B->pieces[from]);
    capt = PType(B->pieces[to]);
     /* Set up capture piece properly for en-passant captures */
    if (IsEP(*move)) capt=pawn;

     /* And promotions... */
    promote = PromotePiece[IsPromote(*move)];

     /* Score bonus for a capture.  If we're capturing a more valuable piece
      * then score highly, otherwise evaluate the expectation from the
      * exchange and see what we hope to gain (if anything) */
    swapgain = score = 0;
    if (USE_SEE==0 || capt==empty) {
      swapgain = PieceValue100[promote] + PieceValue100[capt] - PieceValue10[p];
      switch(promote) {
         // No promotion -> non-capturing checking move -> always keep these!
        case 0 : swapgain = 0; break;
         // Reward queen promotions
        case 5 : swapgain += QUEEN_SCORE; break;
         // Penalise promotion moves which don't promote to queen
        default: swapgain += PieceValue100[promote]; score -= (swapgain + KNIGHT_SCORE);
      }
    }
    else swapgain = SEE(B,from,to,PType(promote)) * 100;
    
     /* This move is illegal (king capturing into check) */
    if (swapgain < -QUEEN_SCORE) {move++;continue;}

     /* If this move looks like it won't improve alpha then ignore it. Simple qsearch
      * futility pruning called 'Delta cutting'.  The safety margin is usually one pawn
      * score, but is adjustable.  The higher, the safer!  At this stage we
      * prune only _really_ bad moves (DELTA_LEVEL + BISHOP_SCORE) */
    if (USE_DELTA && (swapgain + DELTA_LEVEL + BISHOP_SCORE) < delta &&
        *move != hashmove && Skill>6)
       {DeltaCuts++;move++;continue;}
     
     /* If this looks like a good move */
    if (swapgain>=0) {

       /* Do the move */
      U = DoMove(B,*move);
      
       /* Experimental new (safer) type of delta cuts.  Basically, if it looks like a delta
        * cut should be taken then actually check the LazyEval score to see if the estimate
        * was at all accurate.  If the score plus a bound is still below delta then
        * really do the cut. */
      if (USE_DELTA && (score + swapgain + DELTA_LEVEL) < delta &&
          Skill>6) {
        if (IsDrawnMaterial(B)) evalscore = (Current_Board.side==WHITE ? DRAW_SCORE : -DRAW_SCORE);
        else evalscore = LazyEval(B) + IsWonGame(B);
        if (B->side==BLACK) evalscore = -evalscore;
        if ((evalscore + DELTA_LEVEL) < delta) {
          DeltaCuts++;
          UndoMove(B,*move,U);
          break;
        }
      }

      /* Score for the expected gain from capture sequences & promotions */
      if (Skill>4) score += swapgain;
      
      /* Increase the score if this move is a killer move */
      if (USE_KILLERS && Skill>4) {
        if      (*move == MateKiller[ply]) score += MATEKILLER;
        else if (*move == Killer1[ply])    score += KILLER1;
        else if (*move == Killer2[ply])    score += KILLER2;
      }
      
      /* Random Offset for Skill Levels */
      if (Skill<10) score += Random((10-Skill)*4);
      
      /* Keep track of this move if it is the hash move */
      if (*move == hashmove) foundhash=nmoves;

      /* Store the move */
      Full[nmoves].move = *move;
      /* Store this move's ordering score */
      Full[nmoves].score = score;
      if (score > bestscore) bestscore = score;
      nmoves++;

      /* Undo the move */
      UndoMove(B,*move,U);
    }
    else SEECuts++;
    
     /* Get the next move to test */
    move++;
  }

   /* Put the best move at the front */
  if (foundhash>-1) Full[foundhash].score = bestscore + 1;

   /* Return the number of moves */
  return nmoves;
}


/* Ponder the current position, using the best guess from the hashtable
 * as a first move (if a hash entry exists) */
void PonderPosition(void) {
  int val;
  HashElt *Entry=NULL;
  Board *B = &Current_Board;
  BOOL OldAnaly = AnalyseMode;
  MOVE BestMove = NO_MOVE, BookMove = NO_MOVE;
  Undo U;

   /* Do not ponder if we have input waiting to be processed */
  if (Bioskey())  return;

   /* Check the Opening Book First.  If this is a book position
    * then don't bother pondering. */
  if (BookON) BookMove = CheckOpening(B,&val);
  if (BookMove != NO_MOVE) return;

   /* Setup the Hash Table, in case it hasn't been done already */
  SetupHash();

   /* Check to see if we have a hash entry for this position */
  Entry = HashProbe(B);
  if (Entry) {
    BestMove = Entry->move;
  }

   /* Play the suggested best move, if one exists.  This is Beowulf trying to
    * guess what the opposition will play, and then begin thinking about the
    * resulting position before it's our turn. */
  if (BestMove != NO_MOVE) {
    U = DoMove(B,BestMove);
  }

   /* Ponder the resultant position until interrupted */
  fprintf(stdout,"Initiating Pondering\n");
  Pondering = TRUE;
  AnalyseMode = TRUE;
  (void)Comp();
  Pondering = FALSE;
  AnalyseMode = OldAnaly;
  fprintf(stdout,"Finished Pondering\n");

   /* Undo the move, if we previously did one, leaving the board just as 
    * we were given it at the beginning of this function. */
  if (BestMove != NO_MOVE) UndoMove(B, BestMove, U);
}




/*   ----------=====###  Parallel Search Code for BeoServer  ###=====----------   */


/* The following code is included only for the BeoServer implementation for the
 * Chessbrain project.  There is no point in fiddling with this because it
 * is never used in the standard operation of the Beowulf engine. */


#ifdef BEOSERVER

/* Sort out the parallel search algorithm.
 * Build up a table of nodes that need searching and keep the server program updated
 * with the current priorities etc. */
int RunParallel(Board *B,int depth, const int inchk, int fifty,int PreviousTime) {
  long int score = 0;
  int n,k,ParallelNodesDone,TotalNodesToSearch,LastTime,tdiff,EstimatedCost;
  int CountDelete=0, FirstTime, TotalCost=0;
  float ElapsedTime;
  char FEN[128];
  BOOL bDel=FALSE;
  HashElt *Entry = NULL;
  NODE *Nd;

  // Set things up
  BoardToFEN(B,FEN);
  fprintf(stdout,"Working on position \"%s\"\n",FEN);
  fprintf(stdout,"Beginning Parallel Search for Depth %d Ply\n",depth);
  RootAlpha = -CMSCORE;
  RootBeta = CMSCORE;
  bTopMoveComplete = FALSE;
  EstimatedCost = (int)(((float)PreviousTime*1000.0f)/(float)BenchmarkSpeed);
  FirstIteration = TRUE;
  FirstTime = GetElapsedTime();
  // Probe the hashtable for previous results
  Entry = HashProbe(B);
  assert(Entry != NULL);
  InitHashMove = Entry->move;
  fprintf(stdout,"TREE_NEWBRANCH -1 0 Root\n");

  /* Begin the loop.  Continuously cycle through the top few ply until
   * all the nodes have been fully searched. */
  do {
    // Reset a few variables
    TotalNodesToSearch = 0;
    ParallelNodesDone = 0;
    // Keep a note of when we started this iteration
    LastTime = GetElapsedTime();
    // Run this search iteration 
    score = SearchParallel(B, -CMSCORE, CMSCORE, depth*ONEPLY, 0, inchk, fifty,
                           PRIORITY_START, NO_MOVE, EstimatedCost, -1);
    // Debug if we've aborted on time
    if (AbortFlag) fprintf(stdout,"** Abort on Time\n");
    // If we've not aborted on time then check the completion
    if (!AbortFlag) {
      bDel = FALSE;
      // Check update of work units
      for (n=0;n<NODE_TABLE_SIZE;n++) {
        for (k=0;k<NodeTable[n].entries;k++) {
          Nd = &(NodeTable[n].list[k]);
          // Is this node still required in the search?
          if (Nd->Required) {
            ParallelNodesDone += Nd->Done;
            TotalNodesToSearch ++;
          }
          // This node is no longer required in the search
          else {
            // See if we're awaiting a result from this node
            // If so then delete it from the queue
            if (Nd->Awaiting) {
              if (bDel == FALSE) {
                fprintf(stdout,"DELETE_BEGIN\n");
                bDel = TRUE;
              }
              fprintf(stdout,"DELETE [%d]\n",Nd->ID);
              Nd->Awaiting = FALSE;
              CountDelete++;
            }
          }
          // Reset this node ready for next iteration.
          Nd->Required = FALSE;
        }
      }
      if (bDel) fprintf(stdout,"DELETE_END\n");
      
      // Print out statistics 
      fprintf(stdout,"Parallel Nodes Done %ld/%ld\n",ParallelNodesDone, TotalNodesToSearch);
      
      /* Check for new input from PeerNodes
       * Also delay so that this search takes a minimum of half a second.  This stops us
       * from going too quickly and producing huge logfiles. */
      do {
        InputFlag = CheckUserInput();
        tdiff = GetElapsedTime() - LastTime;
      } while (InputFlag == INPUT_WORK_DONE || tdiff < 50);
    }

    // Send a command to CBS to distribute the nodes
    if (FirstIteration) fprintf(stdout,"PROCESS\n");
    FirstIteration = FALSE;

  } while (!Complete[0] && !AbortFlag);

  /* If we haven't finished searching properly the top ply then reset
   * the hash entry with the proper best move.  Simply use the result
   * from the previous ply search (that we extracted above). */
  if (!bTopMoveComplete) {
    // Debugging
    fprintf(stdout,"** Top ply not searched properly (Hash Move unfinished).\n");
    fprintf(stdout,"** Reverting to last ply result.\n");
  }
  else {
    fprintf(stdout,"* Inserting hashtable entry\n");
    Entry->move = BestMoveRet;
    Entry->score = (short)score;
  }


  // We've finished the search.  Clear up a bit. Send a DELETE message to
  // the SuperNode for obsolete work units that we are still awaiting.
  // Reset all the NODE elements that will need to be re-searched next ply.
  fprintf(stdout,"[Cleanup phase]\n"); // This deletes all remaining nodes
  bDel = FALSE;
  for (n=0;n<NODE_TABLE_SIZE;n++) {
    for (k=0;k<NodeTable[n].entries;k++) {
      Nd = &(NodeTable[n].list[k]);
      if (Nd->Awaiting) CountDelete++;
      Nd->Awaiting = FALSE;
      Nd->Split = FALSE;
      Nd->Required = FALSE;
      if (Nd->Done) TotalCost += Nd->cost;
      // Retain nodes that are definite checkmates - no point in searching them again
      // If score == -CMSCORE then this NODE hasn't been searched - ignore it.
      if (IsCM(Nd->score) && Nd->score > -CMSCORE) {
        fprintf(stdout,"Retaining NODE ID %d result - CM score (%d)\n",Nd->ID,IsCM(Nd->score));
        if (Nd->Done == FALSE) fprintf(stdout,"** Potential problem - CM NODE is not 'Done'!\n");
        Nd->Done= TRUE;
      }
      // Flag the nodes that are *not* definite checkmates to be re-searched
      else Nd->Done = FALSE;
      // Reset alpha and beta ready for next iteration
      Nd->alpha = Nd->talpha = -CMSCORE;
      Nd->beta  = Nd->tbeta  = CMSCORE;
    }
  }

  // Output some performance information
  fprintf(stdout,"--------STATISTICS--------\n");
  fprintf(stdout,"Total parallel nodes done    = %d\n",ParallelNodesDone);
  fprintf(stdout,"Total parallel nodes deleted = %d\n",CountDelete);
  if (CountDelete + ParallelNodesDone > 0) fprintf(stdout,"Resulting pruning factor     = %d%%\n",(CountDelete*100)/(CountDelete+ParallelNodesDone));
  else fprintf(stdout,"Resulting pruning factor     = N/A\n");
  fprintf(stdout,"Total useful NODE cost       = %d\n",TotalCost);
  ElapsedTime = (float)(GetElapsedTime() - FirstTime)/100.0f;
  fprintf(stdout,"Time taken for this ply      = %.2f seconds\n",ElapsedTime);
  fprintf(stdout,"App. Speed ratio over server = %.2f times\n",(float)TotalCost / (BenchmarkSpeed * ElapsedTime));
  fprintf(stdout,"--------------------------\n");

  // Setup the input flag (i.e. set that we've finished dealing with all
  // returned NODE information and there is nothing left to parse.)
  InputFlag = INPUT_NULL;

  return score;
}

/* Do a native search iteration for the parallel algorithm.  This involves searching
 * without any guesswork pruning and keeping track of the current depth so that we
 * can start distributing leaf nodes to PeerNode servers. */
int SearchParallel(Board *B,const int alpha, const int beta, int depth, int ply,
                const int inchk, int fifty, int Priority, MOVE LastMove, int PreviousCost, int ParentNode) {
  MOVE movelist[MAX_MOVES],m,*lastmove,hashmove=NO_MOVE,bestmove=NO_MOVE;
  int score = -INFINITY, best = -INFINITY, evalscore=0;
  int NMoves,Moveno,newfifty=fifty+1,p,pto,LegalMoves=0,gchk;
  int newdepth,Base_Extensions=0,Extensions=0,excess=0, PredictedCost;
  BOOL DoNull = TRUE;
  FullMove Full[100];
  Undo U;
  BOOL threat=FALSE,Futile=FALSE;
  BOOL LeafNode;
  NODE *Node,*ThisNode;
  KeyType ThisKey = B->Key;

   /* Increase the Node Count */
  Nodecount++;
   
   /* Reset the best move */
  BestMoveRet = NO_MOVE;
   
   /* Store that this node is Complete (has no incomplete child nodes).
    * This is set to FALSE if one of the child nodes is incomplete. */
  Complete[ply] = TRUE;

   /* Reset 'capture' flag */
  IsCap[ply]=FALSE;

  /* Get the node for this (parent) position */
  ThisNode = FindNode(ThisKey,ParentNode);
  hashmove = ThisNode->move;
  ThisNode->talpha = max(alpha,ThisNode->talpha);
  ThisNode->tbeta  = min(beta,ThisNode->tbeta);


   /*   ------------====     TEST FOR DRAWN POSITIONS     ====--------------- */

   /* See if this is a repeated/drawn position.  Do not test at top ply */
  if (ply>0 && IsDrawn(B,ply,fifty,TRUE)) {
    if (FirstIteration) fprintf(stdout,"* Draw result for NODE %d\n",ThisNode->ID);
    return ((Current_Board.side==B->side) ? (DRAW_SCORE) : -(DRAW_SCORE));
  }

   /*    -----------====     PROBE ENDGAME TABLEBASES     ====------------- */
   
   /* If we're not on top ply then probe the Endgame Tablebases (if any exist) */
  if (TBON && ply>0 && ProbeEGTB(B,&best,ply)) {
    if (FirstIteration) fprintf(stdout,"* EGTB hit for NODE %d\n",ThisNode->ID);
    return best;
  }
  if (ply>0) TBHit = FALSE;


   /*     ----------====     GENERATE MOVELIST     ====------------  */

   /* Make some moves */
  if (!inchk) lastmove = GenerateMoves(B,B->side,movelist);
  else lastmove = GenerateCheckEvasions(B,B->side,movelist,inchk);

   /* Score the moves in the preliminary movelist.  Don't remove illegal moves just yet,
    * unless we're in check (needed for single-reply extension). If we're searching
    * the root then order moves according to root scores instead */
  NMoves = FilterMovelist(movelist,lastmove,B,Full,ply,hashmove,inchk);
   /* No moves - stalemate */
  BestMoveRet=NO_MOVE;
  if (NMoves == 0) {
    if (FirstIteration) fprintf(stdout,"* Stalemate result for NODE %d\n",ThisNode->ID);
    return ((Current_Board.side==B->side) ? (DRAW_SCORE) : -(DRAW_SCORE));
  }
   

   /*     -----------====     CYCLE THROUGH MOVES - INNER LOOP     ====---------- */

  for (Moveno = 0 ; Moveno < NMoves ; Moveno++) {

     /* Update the priority for this node. 
      * First nodes in the tree have highest priority, regardless of ply.
      * Deeper nodes have a slight preference over shallow nodes. */
    Priority -= PRIORITY_SCALE;

    // Reset the score to make sure that incomplete nodes don't get stored.
    score = -INFINITY;

     /* Get the highest scoring move from those left on the list.  Put it at the top. */
    SortFrom(Full,Moveno,NMoves);
    m = Full[Moveno].move;
    if (ply==0) TopPlyMoveno = Moveno;

    /* Identify the piece that's moving, and the piece on the target square */
    p   = PType(B->pieces[MFrom(m)]);
    pto = PType(B->pieces[MTo(m)]);

     /* Do the move */
    U = DoMove(B,m);

     /* Filter out illegal moves which leave us in check.
      * If we're in check before the move then this has already been done. */
    if (!inchk && InCheck(B,Opponent(B->side))) {
      UndoMove(B,m,U);
      continue;
    }
     /* This move is legal, so increase the count */
    LegalMoves++;

     /* Test to see if this move gives check */
    gchk = GivesCheck(B,m);

     /* Test for immediate CM cutoff */
    if (gchk && InCM(B,gchk)) {
      UndoMove(B,m,U);
      IncreaseCounts(Full[Moveno],ply,CMSCORE);
      BestMoveRet = m;
      if (LegalMoves==1) BestFirst++;
      SortNodes++;
      if (FirstIteration) fprintf(stdout,"* Immediate CM result for NODE %d\n",ThisNode->ID);
      return CMSCORE - ply - 1;
    }
    
     /* Test to see if this move breaks the 50 move draw chain, else increase the count */
    if (pto!=empty || IsEP(m)) IsCap[ply] = TRUE;
    else IsCap[ply] = FALSE;
    if (IsCastle(m) || IsCap[ply] || p==pawn) newfifty = 0;
    else newfifty = fifty + 1;
 
     /*  -------------====     MORE EXTENSIONS     ====------------------ */
     
    Extensions = Base_Extensions;
     
     /* Check extension (if we're giving check this move) */
    if (gchk) {
      Extensions += CHECK_EXTEND;
      CheckExtensions++;
       /* Revealed Check Extension (Not 100% accurate, but near enough)
        * Test to see if the piece giving check is not the piece moving.
        * This fails if the piece moving also gives check, and is tested first. */
      if (gchk != p)
      {Extensions += REVCHECK_EXTEND;RevCheckExtensions++;}
    }     
     
     /* Recapture Extension.  We do this in the following cases;
      * (1) We are not on the first ply (obviously)
      * (2) The last ply was a capture move
      * (3) This move captures the piece that captured last move 
      * (4) The value of this piece equals the value of the target piece */
    if (ply>0 && IsCap[ply-1] && MTo(m) == MTo(LastMove) &&
              PieceValue[pto] == PieceValue[p])
      {Extensions += RECAP_EXTEND;RecapExtensions++;}

     /* A Pawn Push Extension is used if we're moving a pawn to
      * within one rank of promotion. */
    if (p==pawn && (MTo(m)<=h7 || MTo(m)>=a2))
       {Extensions += PAWNPUSH_EXTEND; PawnPushExtensions++;}
     
     /* Add on the extensions, limiting them to one ply maximum,
      * plus reduce the depth to the next ply. */
    newdepth = depth + min(Extensions,ONEPLY) - ONEPLY;
     
   
     /* -----------------====     RECURSE TO THE NEXT PLY     ====---------------- */

    /* Check to see if we're at a leaf node for the server portion of the search. 
     * We do this by allowing the BeoServer code only to search for a certain
     * depth at the top of the game tree.  Initially this depth is only one ply,
     * but as the tree gets deeper, we adaptively subdivide leaf nodes. */

    /* See if we have any details for this new position.  If not then get a new
     * node in which to store the results of this search. */
    Node = FindNode(B->Key,ThisNode->ID);

    /* Check to see if this node has not been searched and is going to take too
     * long for one single computer to search. If so then continue to split it up.     
     * Only split down to a certain maximum depth, MAX_SPLIT.  Do not split nodes
     * previously returned CM scores (and hence are flagged as Done from the offset). */
    LeafNode = TRUE;
    if (ply<MAX_SPLIT && Node->Done == FALSE) {
      /* Only split nodes that have been searched before.  -CMSCORE is the default
       * score for an unsearched NODE.  */
      if (Node->score != -CMSCORE) {
        PreviousCost = Node->cost;
        PredictedCost = (int)((float)Node->cost * ExtendCost);
        /* Only split nodes on the first iteration (and then split those nodes
         * on every iteration, but split no new ones).  This stops us from splitting
         * child nodes with speculatively high cost estimates (because their parent
         * had a high cost estimate).  In these cases, the high cost of the parent
         * is usually due to only one of the child nodes. */
        if (Node->Split || (FirstIteration  && PredictedCost > SPLIT_NODE_COST))
          LeafNode = FALSE;
      }
      /* This node hasn't been searched before, so give it an estimated
       * cost which is equal to the cost of its parent */
      else Node->cost = PreviousCost;
    }

    /* If we are at a leaf node then we need to invoke the parallel code to
     * distribute this node, or else see if it has been searched yet. */
    if (LeafNode) {
       // Flag that this node is part of the search tree
      Node->Required = TRUE;

       // Check to see if we already have a result from this node
      if (Node->Done) {
        // We already have a result so use this
        score = - Node->score;
        // Flag that we can use this node score
        Complete[ply+1] = TRUE;
        // Write out this task to the logfile
        if (FirstIteration) fprintf(stdout,"Skipping Completed Node [%d]\n",Node->ID);
      }
       // We don't have a result here yet, so send this node to be searched
      else {
        // Debugging information
        if (Node->Awaiting == FALSE) {
          // Tree-building debug
          fprintf(stdout,"TREE_NEWBRANCH %d %d ",ThisNode->ID, Node->ID);
          PrintMove(m,FALSE,stdout);
          fprintf(stdout,"\n");
          // NODE info
//          fprintf(stdout,"- Distributing Node ID=%d ply=%d (Depth %d.%d [+%d]) [Key=%d] [Parent=%d]\n",
//                          Node->ID,ply,newdepth/ONEPLY,newdepth%ONEPLY,Extensions,
//                          (int)(B->Key % (KeyType)NODE_TABLE_SIZE),ThisNode->ID);
        }
        // Distribute this node, or check on its progress/results so far
        DistributeNode(Node,B,-ThisNode->tbeta,-ThisNode->talpha,newdepth,ply+1,
                       gchk,newfifty,Priority,m);
         // This node was either searched natively or skipped because it has
         // already been searched to the required depth
        if (Node->Done) {
          // Get the score for this node
          score = - Node->score;
          // Flag that we can use this node score
          Complete[ply+1] = TRUE;
        }
         // This node was *not* done locally.
         // It is now in the lap of the gods of networking
        else {
          // Set that we can't use this score
          Complete[ply] = FALSE;
          Complete[ply+1] = FALSE;
        }
      }
    }
     
     /* We are not at a leaf node so recurse to the next ply using negamax
      * search heuristic */
    if (!LeafNode) {
      // Split this node and setup the necessary variables
      if (Node->Split == FALSE) {
        // Tree-building debug
        fprintf(stdout,"TREE_NEWBRANCH %d %d ",ThisNode->ID, Node->ID);
        PrintMove(m,FALSE,stdout);
        fprintf(stdout,"\n");
        // Normal debug
//        fprintf(stdout,"--> Splitting Node ID=%d (depth = %d.%d [+%d]) [Pred=%d]\n",
//                        Node->ID,newdepth/ONEPLY,newdepth%ONEPLY,Extensions,PredictedCost);
        // Set the flag to record that we have already split this node
        Node->Split = TRUE;
      }

      score = - SearchParallel(B,-ThisNode->tbeta,-ThisNode->talpha,newdepth,ply+1,gchk,newfifty,
        Priority + PRIORITY_STEP,m,(int)((float)PreviousCost/ExtendCost),ThisNode->ID);

      /* Get the node for this (parent) position again incase it was corrupted
       * by reallocation of the NODE table. */
      ThisNode = FindNode(ThisKey,ParentNode);

        /* Check to see if the child node was searched completely or it was
         * a beta cut or a non-cutting CM against - in which case it's already done. */
      if (!AbortFlag && (Complete[ply+1] == TRUE || score <= ThisNode->talpha || IsCM(score)==-1)) {
        Node->score = -score;
        Node->Done = TRUE;
        Node->move = BestMoveRet;
        Complete[ply+1] = TRUE;
      }
      // This node's children have not yet been completely searched
      else {
        // Set that we can't use this score
        Complete[ply] = FALSE;
        Complete[ply+1] = FALSE;
      }
    }

     /* Undo the move */
    UndoMove(B,m,U);

     /*  ---------------====     HANDLE THE SEARCH RESULTS     ====------------- */

     /* Flag if we've completed the search for the ply=0 hash move. */
    if (ply == 0 && m == InitHashMove && Complete[ply+1] && !AbortFlag) {
      if (bTopMoveComplete == FALSE)
        fprintf(stdout,"** Previous top move complete - this ply can now be accepted!\n");
      bTopMoveComplete = TRUE;
    }

     /* Update scores */
    if (Complete[ply+1] && score>best && !AbortFlag) {
      best = score;
      bestmove = m;       
       /* Have we improved alpha? (i.e. this an interesting move) */
      if (best>ThisNode->talpha) {
        fprintf(stdout,"* [%d] Updating Alpha (%d -> %d)\n",ThisNode->ID,ThisNode->talpha,score);
        ThisNode->talpha = score;
              IncreaseCounts(Full[Moveno],ply,score);
               // Fail-high cutoff.
        if (score>=ThisNode->tbeta) {
           /* Update the hash table & return the move score */
          BestMoveRet = bestmove;
                SortNodes++;
                if (LegalMoves==1) BestFirst++;
          if (FirstIteration) fprintf(stdout,"* First Iteration Beta cut\n");
                return best;
        }
         /* Print off the move */
        if (ply==0) {
          fprintf(stdout,"Current Best = ");
          PrintMove(bestmove,FALSE,stdout);
          fprintf(stdout,"  (%d)\n",score);
        }
      }
      else if (ply==0 && LegalMoves==1) {
        fprintf(stdout,"Current Best = ");
        PrintMove(bestmove,FALSE,stdout);
        fprintf(stdout,"  (%d)\n",score);
      }
    }
     
    /* Check for time problems and new input periodically */
    if (!AbortFlag && ((Nodecount&INPUT_PERIOD)==0 || ply<4) && !bRushed) {
      if (GlobalDepth>Params.Depth && !AnalyseMode) AbortFlag = CheckTime(ply,score);
      if (!AbortFlag) {
        InputFlag = CheckUserInput();
        if (InputFlag && InputFlag != INPUT_WORK_DONE) AbortFlag = TRUE;
      }
    }

    if (AbortFlag) break;
  }

   /* Check for stalemate.  Note that we've already filtered out CMs at the top of
    * this function. */
  if (LegalMoves == 0) {
    best = ((Current_Board.side==B->side) ? (DRAW_SCORE) : -(DRAW_SCORE));
    bestmove = NO_MOVE;
  }

  /* Update the best move */
  ThisNode->move = bestmove;
  BestMoveRet = bestmove;

  /* Return the best score */
  return best;
}

/* Distribute a node for analysis in the peer node system. */
void DistributeNode(NODE *Node, Board *B,const int alpha, const int beta, int depth, int ply,
                const int inchk, int fifty, int Priority, MOVE LastMove) {
#ifdef SEARCH_NATIVELY
  long int score;
  int TimeElapsed;
#endif // SEARCH_NATIVELY
  char FEN[128];
  int TableKey = (int)(B->Key % (KeyType)NODE_TABLE_SIZE), newdepth, PredictedCost;

  /* Distribute this work unit to the SuperNode server for analysis 
   * by a PeerNode. */
  // NODE hasn't yet been sent out
  if (!(Node->Awaiting)) {
                
                // Get the FEN for this board
                BoardToFEN(B,FEN);
                
                // Get the new search depth for this node.
                // Make sure that any fractional ply extensions are rounded up.
                // Also, increase the depth slightly to benefit from the increased
                // potential of NODE splitting.
                newdepth = ((depth + ONEPLY + 1) / ONEPLY);
                
                // Predict the computational cost of this search, in relative units
                PredictedCost = (int)((float)Node->cost * ExtendCost);
                
                // Check if this is likely to be an easy node.  If so then
                // We should probably extend it by a bit.
#ifdef EXTEND_EASY_NODES
                if ((int)((float)PredictedCost * ExtendCost) < EASY_NODE) {
                        PredictedCost = (int)((float)Node->cost * ExtendCost * ExtendCost);
                        newdepth++;
                        fprintf(stdout,"Easy NODE - Depth Extended by 1 ply\n");
                }
#endif
                
                // If this node hasn't been searched deeply enough already then distribute it
                if (Node->depth < newdepth) {
                        // Distribute this node to the SuperNode server
                        fprintf(stdout,"NODE [%d][%d] \"%s\" %d %d %d %d %d\n",
                                TableKey, Node->ID, FEN, newdepth, alpha, beta, Priority, PredictedCost);
                        Node->Done = FALSE;     // Search incomplete
                        Node->depth = newdepth; // Setup the depth
                        Node->Awaiting = TRUE;  // We've sent this node and we're awaiting a reply
                }
                // Otherwise, skip it
                else {
                        fprintf(stdout,"** [%d] Skipping Node - depth already sufficient.  Score = %d\n",Node->ID, Node->score);
                        Node->Done = TRUE;
                        Node->Awaiting = FALSE;
                }
  }
  // Node has already been sent out.  Check alpha-beta values to see if they need
  // updating at all.
  else {
                if (FirstIteration) {
                        fprintf(stdout,"** Warning - NODE %d already awaiting result on first iteration! (Done=%d) (A-B = %d,%d)\n",
                                Node->ID,Node->Done,Node->alpha,Node->beta);
                }
                if (Node->alpha != alpha || Node->beta != beta) {
                        // Update the CBS with the new alpha-beta values for this NODE
                        //        fprintf(stdout,"UPDATE [%d][%d] %d %d\n", TableKey, Node->ID, alpha, beta);     
                }
  }

  // Make sure we're up to date for this NODE's a-b bounds
  Node->alpha = alpha;
  Node->beta = beta;
}

/* See if the given board exists in the node table.  If so then return it, otherwise
 * get a new (free) node. */
NODE *FindNode(KeyType Key, int ParentNode) {
  int n, TableKey = (int)(Key % (KeyType)NODE_TABLE_SIZE);
  NODETABLE *NT;

  // Find the table entry and see if this position already exists
  NT = &(NodeTable[TableKey]);
  for (n=0;n<NT->entries;n++) {
    // Check to see if this is the correct node, being careful to identify transpositions
    // by also matching the parent node ID.  The uniqueness of one NODE therefore
    // follows reverse-inductively from the uniqueness of its parent.
    if (NT->list[n].Key == Key && NT->list[n].parent == ParentNode) {
      return &(NT->list[n]);
    }
  }

  // Insert a new entry into the Node Table for this position
  if (NT->entries>0) NT->list = (NODE *)realloc(NT->list, sizeof(NODE)*(n+1));
  else NT->list = (NODE *)malloc(sizeof(NODE));
  NT->entries++;
//  fprintf(stdout,"New Node Created [ID=%d] [TableKey=%d] [Parent=%d]\n",NextNodeID,TableKey,ParentNode);

  // Setup this entry with default values
  (NT->list[n]).ID = NextNodeID++;
  (NT->list[n]).parent = ParentNode;
  (NT->list[n]).Key = Key;
  (NT->list[n]).depth = 0;
  (NT->list[n]).Done = FALSE;
  (NT->list[n]).Required = FALSE;
  (NT->list[n]).Awaiting = FALSE;
  (NT->list[n]).Split = FALSE;
  (NT->list[n]).cost = 0;
  (NT->list[n]).alpha = -CMSCORE;
  (NT->list[n]).beta = CMSCORE;
  (NT->list[n]).talpha = -CMSCORE;
  (NT->list[n]).tbeta = CMSCORE;
  (NT->list[n]).move = NO_MOVE;
  (NT->list[n]).score = -CMSCORE;

  return &(NT->list[n]);
}

/* Read in any progress from work units completed */
void ReadWorkDone(char *input) {
  char *ch;
  int ID, Key, n;
  NODE *Node;
  NODETABLE *NT;

  // Debugging
//  fprintf(stdout,"ReadWorkDone: receives %s\n", input);
  fprintf(stdout,"JOBACK %s\n", strstr(input,"["));

  // Get the node info 
  ch = strchr(input,'[');
  ID = strtol(ch+1, FALSE, 10);
  ch = strchr(input,'<');
  Key = strtol(ch+1, FALSE, 10);
  NT = &(NodeTable[Key]);
  if (NT->entries == 0) {
    fprintf(stderr,"** Error - Dubious data returned [ID=%d]\n",ID);
    fprintf(stderr,"** No NodeTable entries for this Node Key (%d)\n",Key);
    return;
  }
  for (n=0;n<NT->entries;n++) if (NT->list[n].ID == ID) break;
  if (n >= NT->entries) {
    fprintf(stderr,"** Error - Dubious data returned [ID=%d]\n",ID);
    fprintf(stderr,"** No NodeTable entries matching this work ID [%d]\n",Key);
    return;
  }
  Node = &(NT->list[n]);

  // Get the search result
  ch = strchr(input,'{');
  Node->score = strtol(ch+1, FALSE, 10);

  // Get the search cost for this node
  ch = strchr(input,'(');
  Node->cost = (short)strtol(ch+1, FALSE, 10);

  if (Node->Awaiting == FALSE) {
    fprintf(stdout,"* Ignoring result for Deleted Node %d\n",Node->ID);
  }

  // Get the final depth to which this node was searched
  
  if ((ch = strchr(input,'$'))) {
    Node->depth = (short)strtol(ch+1, FALSE, 10);
    if (Node->depth < 5 || Node->depth > 40) {
      fprintf(stdout,"* Strange search depth (%d) for Node %d\n",Node->depth,Node->ID);
    }
  }
  else fprintf(stdout,"** No depth detected in result - continuing\n");

  // Everything has been read in OK

  Node->Done = TRUE;
  Node->Awaiting = FALSE;

  // Check if this node was aborted.
  if (strstr(input,"ABORTED") != NULL) {
    fprintf(stdout,"** Detected Aborted node : ");
    // If this node is possibly interesting still then split it
    if ((Node->score + ABORT_MARGIN) > Node->alpha && (Node->score - ABORT_MARGIN) < Node->beta) {
      // Check to see if this is a hard node, or just if it was run on a slow computer
      if (Node->cost > TOUGH_NODE) {
        Node->Done  = FALSE;
        Node->Split = TRUE;
        fprintf(stdout,"* Splitting NODE for re-search\n");
      }
      else {
        Node->Done  = FALSE;
        Node->cost *= 2; // Fake a higher cost to get it done on a faster machine.
        fprintf(stdout,"* Returning NODE for re-search by a faster machine\n");
      }
    }
    else {
      fprintf(stdout,"* Accepting reduced-depth result\n");
    }
  }
}

#endif // BEOSERVER
