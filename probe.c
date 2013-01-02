/**********************************
 *    probe.c                     *
 *    Colin Frayn                 *
 *    April 2001                  *
 **********************************/

/*
   This file contains all the functions for the
   endgame tablebase access.  Based on the code written
   by Thorsten Greiner for "Amy" and the pseudo code written
   by Eugene Nalimov as part of the EGTB code distribution.
   These wrapper functions use the core EGTB access code written
   by Eugene Nalimov (included).
   Many thanks to both.
 */

#ifdef _WIN32
#include <windows.h>
#endif
#include <stdlib.h>
#include <stdio.h>

#include "common.h"
#include "probe.h"
#include "board.h"
#include "egtb.c"

/* Allocate 2 Mb for cache */
#define CACHE_SIZE (1<<21)  
/* This is the TB path set in main.c */
extern char TB_PATH[FILENAME_MAX];

extern int Flip[64];
extern long int TBProbes,TBHits;
int LargestTB=0;

/* Setup the data structures needed for the EGTB access code */
void InitialiseTB(void) {
  fprintf(stdout,"Checking for Tablebase Files\n");
  LargestTB = IInitializeTb(TB_PATH);
  if(LargestTB != 0) {
    void *TBCache = (void *)malloc(CACHE_SIZE);
    if (TBCache == NULL) {
      fprintf(stdout,"Not Enough memory for EGTBs!\n");
      LargestTB=0;
      return;
    }
    fprintf(stdout,"Including %d-man Endgame Table Bases\n", LargestTB);
    FTbSetCacheSize(TBCache, CACHE_SIZE);
  }
}

/* Setup piece lists */
int InitialiseCount(int *squares, int type, BITBOARD mask) {
  int count = 0;
  while(mask) {
    int index = FirstPiece(mask);
    RemoveFirst(mask);
     /* Annoyingly, the EGTB code uses A1=0, H8=63 ;) */
    squares[type*C_PIECES+count] = Flip[index];
    count++;
  }
  return count;
}

/* Probe the Endgame Tablebases */
int ProbeEGTB(Board *B, int *score, int ply) {
  int pcCount[10],WhiteSquares[16], BlackSquares[16];
  int iTB,colour,invert,ep,value;
  int *wp, *bp;
  INDEX index;

   /* See if we have too many pieces on the board */
  if(B->Gamestage<Endgame || Count(B->All) > LargestTB) return 0;

   /* Increase the counter */
  TBProbes++;
   
   /* Setup the piece lists */
  pcCount[0] = InitialiseCount(WhiteSquares, 0, B->WhitePawns);
  pcCount[1] = InitialiseCount(WhiteSquares, 1, B->WhiteKnights);
  pcCount[2] = InitialiseCount(WhiteSquares, 2, B->WhiteBishops);
  pcCount[3] = InitialiseCount(WhiteSquares, 3, B->WhiteRooks);
  pcCount[4] = InitialiseCount(WhiteSquares, 4, B->WhiteQueens);
  pcCount[5] = InitialiseCount(BlackSquares, 0, B->BlackPawns);
  pcCount[6] = InitialiseCount(BlackSquares, 1, B->BlackKnights);
  pcCount[7] = InitialiseCount(BlackSquares, 2, B->BlackBishops);
  pcCount[8] = InitialiseCount(BlackSquares, 3, B->BlackRooks);
  pcCount[9] = InitialiseCount(BlackSquares, 4, B->BlackQueens);

   /* See if this TB exists. iTB is the TB index */
  iTB = IDescFindFromCounters(pcCount);
  if(iTB == 0) return 0; 

   /* Remember to add on the King! */
  WhiteSquares[15] = Flip[B->WhiteKing];
  BlackSquares[15] = Flip[B->BlackKing];

   /* Setup Piece Identifiers */
  if(iTB > 0) {
    colour = B->side;
    invert = 0;
    wp = WhiteSquares;
    bp = BlackSquares;
  }
  else {
    colour = Opponent(B->side);
    invert = 1;
    wp = BlackSquares;
    bp = WhiteSquares;
    iTB = - iTB;
  }
   /* Check if we have this TB */
  if(!FRegisteredFun(iTB, colour)) return 0;
   
   /* Set up En-Passant Square */
   /* This doesn't work - not sure why */
  if (B->ep==-1) ep = XX;
  else ep = Flip[B->ep];
   /* Fudge */
  ep = XX;

   /* Probe the tables */
  index = PfnIndCalcFun(iTB, colour) (wp, bp, ep, invert);
  value = L_TbtProbeTable(iTB, colour, index);
  if(value == L_bev_broken) return 0;

   /* Alter CM scores so that they give the correct depth in comp.c */
  if(value > 0) {
    value = ply - CMSCORE + (2*value)-1;
  }
  else if(value < 0) {
    value = ply + CMSCORE + 2*(value-1);
  }

   /* Update the score */
  *score = value;

   /* Update the counter */
  TBHits++;
   /* Success! */
  return 1;
}
