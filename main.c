/**********************************
 *    main.c                      *
 *    Colin Frayn                 *
 *    March 2001                  *
 **********************************/

/*
 This file contains all the main control structure
 for the program, initialises and runs the state
 machine, and handles loading of data etc...
*/ 
 

#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>

#include "common.h"
#include "defs.h"
#include "params.h"
#include "main.h"
#include "parser.h"
#include "checks.h"
#include "comp.h"
#include "computil.h"
#include "board.h"
#include "utils.h"
#include "probe.h"
#include "rand64.h"

/*
#define INITIAL_BOARD "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq -"
*/

Board Current_Board;
int mvno,automoves,testtime=5;
CompDat Params;
BOOL XBoard=FALSE,LoadPGN=FALSE,Post=TRUE, UCI=FALSE;
BOOL Computer[2]={FALSE,FALSE},ComputerGO = FALSE,AutoMove=FALSE,Ponder=FALSE, Force=FALSE;
void *TBBuffer;
char TB_PATH[FILENAME_MAX]="TB";
char BOOK_FILE[FILENAME_MAX]="book.dat";
char PERSON_FILE[FILENAME_MAX]="default.per";
char EPD_FILE[FILENAME_MAX]="";
extern HashElt *TableW,*TableB;
extern int InputFlag, GlobalAlpha, GlobalBeta;
extern long int NHash;
#ifdef BEOSERVER
int BenchmarkSpeed;
BOOL bParallel = TRUE;
extern int TimeTaken;
#endif

/* The main() function - main control structure. */
int main(int argc, char **argv) {
  int state=S_NOP, oldmvno;
  char input[FILENAME_MAX];

   /* No buffering, please... */
  setbuf(stdout, NULL);
  setbuf(stdin, NULL);
   /* and I *really* mean it, too! */
  setvbuf(stdout, NULL, _IONBF, 0);
  setvbuf(stdin, NULL, _IONBF, 0);
  fflush(NULL);   

  GetConfigFile();
  GetEnvironmentVars();
  if (!ParseCommandLine(argc,argv)) return 0;
  CheckParams();
   
  fprintf(stdout,"Welcome to "NAME" Version "VER"!\n\n");

#ifdef BEOSERVER  
  fprintf(stdout,"**Chessbrain Server Version**\n");
#endif // BEOSERVER

  LoadOpeningBook();

  LoadPersonalityFile();
  SetupPrecalculatedData();
  Defaults();
  InitialiseTB();

#ifdef BEOSERVER  
  BenchmarkSpeed = Benchmark();
#endif // BEOSERVER

  if (strlen(EPD_FILE)>0) {
    RunTestSuite(EPD_FILE,testtime);
    return 0;
  }
  if (!XBoard) PrintBoard(Current_Board);
  if (!XBoard) Prompt();

  do {
    oldmvno = mvno;
    if (state == S_NOP) {
      (void)fgets(input,FILENAME_MAX,stdin);
      if (feof(stdin)) state = S_QUIT;
      else state=ParseInput(input);
    }
    if (state == S_SEARCH) {
      if (Comp()==-1) ComputerGO = FALSE;
      if (oldmvno != mvno && InputFlag==INPUT_NULL) state = S_CHKDRW;
      else if (InputFlag==INPUT_NULL) state = S_NOP;
      if (InputFlag==INPUT_STOP) {ParseInput("FORCE");state = S_NOP;}
    }
    if (state == S_AUTO)   {AutoMove=TRUE;(void)Comp();AutoMove=FALSE;
                            if (CheckForDraw()>0 || --automoves==0) state=S_NOP;}
    if (state == S_CHKDRW) {(void)CheckForDraw();state=S_NOP;}
    if (state != S_QUIT && state != S_AUTO && !XBoard) Prompt();
    if (Computer[Current_Board.side]==1 && ComputerGO && state != S_AUTO && !Force)
       {if (!XBoard) fprintf(stdout,"\n");state=S_SEARCH;}
    if (state == S_NOP && XBoard && Ponder && !Force) PonderPosition();
  } while (state!=S_QUIT);

  FreeOpenings();
  ResetHash();

  return 0;
}

#ifdef BEOSERVER
/* Benchmark the computer using the opening position to 9 ply.
 * Return the time taken in centiseconds. */
int Benchmark(void) {
  fprintf(stdout,"Running Standard BeoServer Benchmark\n");
  bParallel = FALSE;
  ParseInput("BOOK");
  ParseInput("RESET");
  ParseInput("ST 0");
  ParseInput("DEPTH 9");
  (void)Comp();
  ParseInput("BOOK");
  bParallel = TRUE;
  fprintf(stdout,"Benchmark Result : %d\n",TimeTaken);
  return TimeTaken;
}
#endif

/* Setup the precalculated bitboards that we're going to need
 * later on in the game */
void SetupPrecalculatedData(void) {
  BITBOARD rank=FullRank,mask,mask2;
  int i,j,x,y,rankno,filenum,diagstart,dsfile,dl,fi,dx,dy;

  /* seed the 64-bit psuedo-random number generator */
  randinit(TRUE);

  /* Rank and File Masks */
  FullBoard=0;
  for (i=0;i<8;i++) {
    FileMask[i] = 0;
    for (j=i;j<64;j+=8) FileMask[i] += (UNIT<<j);
    RankMask[i] = (rank << (8*i));
    FullBoard += RankMask[i];
  }
   
   /* Masks for the FirstPiece algorithm */
  FPMask1 = (BITBOARD)TwoFullRanks << 16;
  FPMask2 = (BITBOARD)TwoFullRanks << 32;
   
   /* Masks for the squares on a file above (or below) the specified square */
  for (i=0;i<64;i++) {
    FileUpMask[i] = FileDownMask[i] = 0;
    for (j=i-8;j>=0;j-=8) FileUpMask[i] += (UNIT << j);
    for (j=i+8;j<64;j+=8) FileDownMask[i] += (UNIT << j);
  }

   /* A mask with 1's in all the white squares, and the same for black */
  BlackMask = WhiteMask = 0;
  for (i=0;i<64;i++) {
    if (IsWhite(i)) WhiteMask += (UNIT << i);
    else            BlackMask += (UNIT << i);
  }

   /* Masks for the half ranks/files/diagonals from the square, like
    * FileUpMask & FileDownMask, but in different directions. */
  for (i=0;i<64;i++) {
    DirL[i]=DirR[i]=DirUR[i]=DirUL[i]=DirDR[i]=DirDL[i]=0;
    for (j=i-1;File(j)<File(i);j--) DirL[i] += (UNIT<<j);
    for (j=i+1;File(j)>File(i);j++) DirR[i] += (UNIT<<j);
    for (j=i-9;File(j)<File(i) && j>=0;j-=9) DirUL[i] += (UNIT<<j);
    for (j=i-7;File(j)>File(i) && j>=0;j-=7) DirUR[i] += (UNIT<<j);
    for (j=i+7;File(j)<File(i) && j<64;j+=7) DirDL[i] += (UNIT<<j);
    for (j=i+9;File(j)>File(i) && j<64;j+=9) DirDR[i] += (UNIT<<j);
  }
   
   /* Bonuses for centre proximity and avoidance */
  for (i=0;i<64;i++) {
    CentreBoard[i]=0;
    dx = (2*File(i) - 7);
    dy = (2*Rank(i) - 7);
    CentreBoard[i] = 10 - (int)sqrt((double)(dx*dx + dy*dy));
    CentreBoard2[i] = CentreBoard[i]*2;
    CornerBoard2[i] = CornerBoard[i]*2;
    CornerBoard3[i] = CornerBoard[i]*3;
  }
   
   /* Square Masks */
  for (i=0;i<64;i++) {
    Mask[i] = (UNIT<<i);
    InvMask[i] = ~Mask[i];
  }
   
   /* King Safety Mask - all squares within 2 in x or y of the current sq */
  for (i=0;i<64;i++) {
    KingSafetyMask[i] = EMPTY;
    for (x=File(i)-2;x<=File(i)+2;x++) {
      if (x<0 || x>7) continue;
      for (y=Rank(i)-2;y<=Rank(i)+2;y++) {
        if (y<0 || y>7) continue;
        if ((y*8)+x == i) continue;
        KingSafetyMask[i] |= Mask[(y*8)+x];
      }
    }
  }
   
   /* Diagonal Masks */
  for (i=0;i<64;i++) {
    DiagMaska1h8[i] = DiagMaska8h1[i] = 0;
     /* a1h8 direction first */
    diagstart = 7*(min((File(i)),7-(Rank(i)))) + i;
    for (j=0;j<DiagonalLength_a1h8[i];j++) {
      DiagMaska1h8[i] += (UNIT << (diagstart - j*7));
    }
     /* a8h1 direction next */
    diagstart = i - 9*(min((File(i)),(Rank(i))));
    for (j=0;j<DiagonalLength_a8h1[i];j++) {
      DiagMaska8h1[i] += (UNIT << (diagstart + j*9));
    }
  }

   /* Setup inverse transformation matrices */
  for (i=0;i<64;i++) {
    InvRotateL45[(RotateL45[i])]=i;
    InvRotateR45[(RotateR45[i])]=i;
  }

   /* Setup masks for diagonal sliding pieces */
  for (i=0;i<64;i++) {
    DiagonalMask_a1h8[i] = (1 << DiagonalLength_a1h8[i])-1;
    DiagonalMask_a8h1[i] = (1 << DiagonalLength_a8h1[i])-1;
  }
   
   /* Ok this one is a little bizarre.  Basically they're
    * bitboards with which I XOR the relevant rotated bitboards
    * after a castling move to get the correct resultant board */
  CastleMaskR90[0] = Mask[(RotateL90[e1])] | Mask[(RotateL90[g1])] | 
                     Mask[(RotateL90[h1])] | Mask[(RotateL90[f1])];
  CastleMaskR90[1] = Mask[(RotateL90[e1])] | Mask[(RotateL90[c1])] | 
                     Mask[(RotateL90[a1])] | Mask[(RotateL90[d1])];
  CastleMaskR90[2] = Mask[(RotateL90[e8])] | Mask[(RotateL90[g8])] | 
                     Mask[(RotateL90[h8])] | Mask[(RotateL90[f8])];
  CastleMaskR90[3] = Mask[(RotateL90[e8])] | Mask[(RotateL90[c8])] | 
                     Mask[(RotateL90[a8])] | Mask[(RotateL90[d8])];

  CastleMaskR45[0] = Mask[(InvRotateR45[e1])] | Mask[(InvRotateR45[g1])] | 
                     Mask[(InvRotateR45[h1])] | Mask[(InvRotateR45[f1])];
  CastleMaskR45[1] = Mask[(InvRotateR45[e1])] | Mask[(InvRotateR45[c1])] | 
                     Mask[(InvRotateR45[a1])] | Mask[(InvRotateR45[d1])];
  CastleMaskR45[2] = Mask[(InvRotateR45[e8])] | Mask[(InvRotateR45[g8])] | 
                     Mask[(InvRotateR45[h8])] | Mask[(InvRotateR45[f8])];
  CastleMaskR45[3] = Mask[(InvRotateR45[e8])] | Mask[(InvRotateR45[c8])] | 
                     Mask[(InvRotateR45[a8])] | Mask[(InvRotateR45[d8])];

  CastleMaskL45[0] = Mask[(InvRotateL45[e1])] | Mask[(InvRotateL45[g1])] | 
                     Mask[(InvRotateL45[h1])] | Mask[(InvRotateL45[f1])];
  CastleMaskL45[1] = Mask[(InvRotateL45[e1])] | Mask[(InvRotateL45[c1])] | 
                     Mask[(InvRotateL45[a1])] | Mask[(InvRotateL45[d1])];
  CastleMaskL45[2] = Mask[(InvRotateL45[e8])] | Mask[(InvRotateL45[g8])] | 
                     Mask[(InvRotateL45[h8])] | Mask[(InvRotateL45[f8])];
  CastleMaskL45[3] = Mask[(InvRotateL45[e8])] | Mask[(InvRotateL45[c8])] | 
                     Mask[(InvRotateL45[a8])] | Mask[(InvRotateL45[d8])];

   /* And similarly, these are just a few masks I need later on to save a few ops ;) */
  for (i=0;i<64;i++) {
    MR90[i] = Mask[(RotateL90[i])];
    IMR90[i] = InvMask[(RotateL90[i])];
    MR45[i] = Mask[(InvRotateR45[i])];
    IMR45[i] = InvMask[(InvRotateR45[i])];
    ML45[i] = Mask[(InvRotateL45[i])];
    IML45[i] = InvMask[(InvRotateL45[i])];
  }

   /* This one effectively flips the y-coord */
  for (i=0;i<64;i++) {
    Flip[i] = ((7-Rank(i))*8) + File(i);
  }

   /* Setup a random key table for the hash function.
    * The hash key is of type KeyType. */
  for (i=0;i<64;i++) {
    for (j=0;j<13;j++) RandomTable[i][j] = (KeyType)(Rand64());
  }
   /* Hash keys for castling and ep priorities */
  for (i=0;i<16;i++) CastleKey[i] = (KeyType)(Rand64());
   
   /* Pawn Moves */
  for (i=0;i<64;i++) {
    PawnAttacksBlack[i] = PawnAttacksWhite[i] = 0;
    PawnMovesBlack[i] = PawnMovesWhite[i] = 0;
    if (File(i)>0) {
      if (i<a1) PawnAttacksBlack[i] += (UNIT<<(i+7));
      if (i>7) PawnAttacksWhite[i] += (UNIT<<(i-9));
    }
    if (File(i)<7) {
      if (i<a1) PawnAttacksBlack[i] += (UNIT<<(i+9));
      if (i>7) PawnAttacksWhite[i] += (UNIT<<(i-7));
    }
    if (Rank(i)==0 || Rank(i)==7) continue;
    PawnMovesBlack[i] += (UNIT<<(i+8));
    PawnMovesWhite[i] += (UNIT<<(i-8));
    if (Rank(i)==1) PawnMovesBlack[i]+=(UNIT<<(i+16));
    if (Rank(i)==6) PawnMovesWhite[i]+=(UNIT<<(i-16));
  }
   /* Knight Moves */
  for (i=0;i<64;i++) {
    KnightMoves[i]=0;
    if (Rank(i)>0) {
      if (Rank(i)>1) {
        if (File(i)>0) KnightMoves[i] += (UNIT<<(i-17)); 
        if (File(i)<7) KnightMoves[i] += (UNIT<<(i-15)); 
      }
      if (File(i)>1) KnightMoves[i] += (UNIT<<(i-10));
      if (File(i)<6) KnightMoves[i] += (UNIT<<(i-6));
    }
    if (Rank(i)<7) {
      if (Rank(i)<6) {
        if (File(i)>0) KnightMoves[i] += (UNIT<<(i+15)); 
        if (File(i)<7) KnightMoves[i] += (UNIT<<(i+17)); 
      }
      if (File(i)>1) KnightMoves[i] += (UNIT<<(i+6));
      if (File(i)<6) KnightMoves[i] += (UNIT<<(i+10));
    }
  }
   /* King Moves */
  for (i=0;i<64;i++) {
    KingMoves[i]=0;
    if (Rank(i)>0) {
      if (File(i)>0) KingMoves[i] += (UNIT<<(i-9));
      if (File(i)<7) KingMoves[i] += (UNIT<<(i-7));
      KingMoves[i] += (UNIT<<(i-8));
    }
    if (Rank(i)<7) {
      if (File(i)>0) KingMoves[i] += (UNIT<<(i+7));
      if (File(i)<7) KingMoves[i] += (UNIT<<(i+9));
      KingMoves[i] += (UNIT<<(i+8));
    }
    if (File(i)>0) KingMoves[i] += (UNIT<<(i-1));
    if (File(i)<7) KingMoves[i] += (UNIT<<(i+1));
  }
   /* Sliding Piece Move Masks. Note that this includes being able to
    * 'capture' friendly pieces and kings.  These are filtered out 
    * later with an AND in the move generation routine.  Basically, 
    * for each square on the board, generate every possible rank using
    * 1 for occupied and 0 for empty.  This gives a list of all possible
    * occupation states from 0-255 for that rank.  Then generate the
    * possible move bitboard for each of these configurations.
    */

   /* Horizontal Sliders */
  for (filenum=0;filenum<8;filenum++) {
    for (j=0;j<256;j++) {
      mask=0;
      for (x=filenum-1;x>=0;x--) {
        mask += (UNIT<<x);
        if (j & (1<<x)) break;
      }
      for (x=filenum+1;x<8;x++) {
        mask += (UNIT<<x);
        if (j & (1<<x)) break;
      }
      for (rankno=0;rankno<8;rankno++)
        MovesRank[(rankno*8)+filenum][j] = mask<<(rankno*8);
    }
  }
   /* Vertical Sliders */
  for (rankno=0;rankno<8;rankno++) {
    for (j=0;j<256;j++) {
      mask=0;
      for (x=6-rankno;x>=0;x--) {
        mask += (UNIT<<(8*(7-x)));
        if (j & (1<<x)) break;
      }
      for (x=8-rankno;x<8;x++) {
        mask += (UNIT<<(8*(7-x)));
        if (j & (1<<x)) break;
      }
      for (filenum=0;filenum<8;filenum++)
        MovesFile[(rankno*8)+filenum][j] = mask << filenum;
    }
  }

   /* Now here is where is gets rather nasty.
    * Generate the possible moves for bishops (and queens) using
    * 45 degrees rotated bitboards, both right and left rotated for
    * the two perpendicular diagonal directions. A bit scary. */
  
   /* a1h8 Diagonal Sense first. Note all diagonals are of
    * different lengths, precalculated in common.h. */
  for (i=0;i<64;i++) {
     /* Get the far left hand square on this diagonal */
    diagstart = 7*(min((File(i)),7-(Rank(i)))) + i;
    dsfile = File(diagstart);
    dl = DiagonalLength_a1h8[i];
    fi = File(i);
     /* Loop through all possible occupations of this diagonal line */
    for (j=0 ; j < (1 << dl) ; j++) {
      mask=mask2=0;
       /* Calculate possible target squares */
      for (x=(fi-dsfile)-1;x>=0;x--) {
        mask += (UNIT<<x);
        if (j & (1<<x)) break;
      }
      for (x=(fi-dsfile)+1;x<dl;x++) {
        mask += (UNIT<<x);
        if (j & (1<<x)) break;
      }
       /* Rotate the target line back onto the required diagonal */
      for (x=0;x<dl;x++)
        mask2 += ( ((mask >> x) & 1) << (diagstart-(7*x)) ) ;
      Movesa1h8[i][j] = mask2;
    }
  }
   
   /* a8h1 Diagonal Sense */
  for (i=0;i<64;i++) {
     /* Get the far left hand square on this diagonal */
    diagstart = i - 9*(min((File(i)),(Rank(i))));
    dsfile = File(diagstart);
    dl = DiagonalLength_a8h1[i];
    fi = File(i);
     /* Loop through all possible occupations of this diagonal line */
    for (j=0 ; j < (1 << dl) ; j++) {
      mask=mask2=0;
       /* Calculate possible target squares */
      for (x=(fi-dsfile)-1;x>=0;x--) {
        mask += (UNIT<<x);
        if (j & (1<<x)) break;
      }
      for (x=(fi-dsfile)+1;x<dl;x++) {
        mask += (UNIT<<x);
        if (j & (1<<x)) break;
      }
       /* Rotate target squares back */
      for (x=0;x<dl;x++)
        mask2 += ( ((mask >> x) & 1) << (diagstart+(9*x)) ) ;
      Movesa8h1[i][j] = mask2;
    }
  }

   /* Finally, make sliding mask templates, or equivalent to
    * all possible queen moves on an open board from this position */
  for (i=0;i<64;i++) {
    RookMask[i]   = MovesRank[i][empty] | MovesFile[i][empty];
    BishopMask[i] = Movesa1h8[i][empty] | Movesa8h1[i][empty];
    QueenMask[i]  = RookMask[i] | BishopMask[i];
                     
  }
   /* Fill the array for piece values */
  PieceValue100[pawn] = PAWN_SCORE;
  PieceValue100[rook] = ROOK_SCORE;
  PieceValue100[knight] = KNIGHT_SCORE;
  PieceValue100[bishop] = BISHOP_SCORE;
  PieceValue100[queen] = QUEEN_SCORE;
   
   /* Distances between squares */
  for (i=0;i<64;i++) {
    for (j=0;j<64;j++) {
      Distance[i][j] = max(abs(Rank(j)-Rank(i)),abs(File(j)-File(i)));
    }
  }

   /* Masks for sections of the board */
   /* Above Mask = All squares from this Rank upwards (inclusive) 
    * Below Mask = All squares from this Rank downwards (inclusive) 
    * Left Mask  = All squares from this Rank leftwards (inclusive) 
    * Right Mask = All squares from this Rank rightwards (inclusive) */
  for (i=0;i<8;i++) {
    AboveMask[i] = EMPTY;
    BelowMask[i] = EMPTY;
    LeftMask[i] = EMPTY;
    RightMask[i] = EMPTY;
    for (j=0;j<i;j++) {
      AboveMask[i] += RankMask[j];
      LeftMask[i] += FileMask[j];
    }
    for (j=7;j>i;j--) {
      BelowMask[i] += RankMask[j];
      RightMask[i] += FileMask[j];
    }
  }

   /* The four board quartiles */
  WhiteKingSide = BelowMask[3] & RightMask[3];
  WhiteQueenSide = BelowMask[3] & LeftMask[4];
  BlackKingSide = AboveMask[4] & RightMask[3];
  BlackQueenSide = AboveMask[4] & LeftMask[4];
   
   /* Get the Gamestage lookup */
  for (i=0;i<256;i++) Gamestage[i] = GetGamestage(i);

   /* Doubled Pawns score based on number of pawns on the board */
  for (i=0;i<17;i++) DoubledPawns[i] = (DoubledPawns[i]*DOUBLED_PAWNS)/10;

   /* Squares surrounding the king */
  for (i=0;i<64;i++) {
    j=0;
    for (y = Rank(i)-2; y <= Rank(i)+2; y++) {
      if (y<0) continue;
      if (y>7) break;
      for (x = File(i)-2; x <= File(i)+2; x++) {
        if (x<0) continue;
        if (x>7) break;
        if (x==File(i) && y==Rank(i)) continue; 
        if (abs(File(i)-x) + abs(Rank(i)-y) == 4) continue;
        KingArea[i][j++] = (y*8) + x;
      }
      KingArea[i][j] = -1;
    }
  }

   /* Double knight moves
    * This is a bit complicated */
  for (i=0;i<64;i++) {
    mask = KnightMoves[i];
    DoubleKnightMove[i] = mask;
    while (mask) {
      j = FirstPiece(mask);
      DoubleKnightMove[i] |= KnightMoves[j];
      RemoveFirst(mask);
    }
  }
   /* Square control weightings based on game stage and rank */
  for (j=0;j<6;j++) {
    for (i=0;i<64;i++) {
      x = 12;
      if (j==2) x=8;
      if (j==3) x=4;
      if (Rank(i)==7) x-=2;
      if (Rank(i)==6) x--;
      if (Rank(i)==4 || Rank(i)==0) x++;
      if (Rank(i)==3 || Rank(i)==1) x+=2;
      if (Rank(i)==2) x+=3;
      if (File(i)==0 || File(i)==7) x--;
      if (File(i)==2 || File(i)==5) x++;
      if (File(i)==3 || File(i)==4) x+=2;
      if (j>=4) x=0;
      SpaceWon[j][i] = (SPACE_WON*x)/12;
      SpaceDefended[j][i] = (SPACE_DEFENDED*x)/12;
    }
  }
   /* A board with +1 if a square is white, and -1 if it is black */
  for (i=0;i<64;i++) {
    if (IsWhite(i)) SquareColour[i]=1;
    else SquareColour[i]=-1;
  }
   /* Pawn Positional advantages based on gamestage */
  for (j=0;j<6;j++) {
    for (i=0;i<64;i++) {
      PawnPosition[j][i] = PawnPositionI[i];
       /* Scale down pawn advancement in the opening */
      if (j==0 && Rank(i)<Rank4) PawnPosition[j][i] = (PawnPositionI[i] / 5);
      if (j==1 && Rank(i)<Rank4) PawnPosition[j][i] = (PawnPositionI[i] / 2);
      PawnPositionPenalty[j][i] = PawnPosition[j][i]/2;
    }
  }
    
   /* Penalty for a side attack based on gamestage */
  SideAttack[5] = SideAttack[4] = 0;
  SideAttack[3] = SIDE_ATTACK;
  SideAttack[2] = SIDE_ATTACK*2;
  SideAttack[1] = SIDE_ATTACK*2;
  SideAttack[0] = SIDE_ATTACK*3;

  /* Corner masks for testing rook positions */
  ULCorner = Mask[a8] + Mask[b8] + Mask[c8];
  ULCorner += ULCorner << 8;
  URCorner = Mask[f8] + Mask[g8] + Mask[h8];
  URCorner += URCorner << 8;
  LLCorner = ULCorner << 48;
  LRCorner = URCorner << 48;

   /* Masks for the squares on a rank left or right of the specified square */
  for (i=0;i<64;i++) {
    RankLeftMask[i] = RankRightMask[i] = 0;
    for (j=i-1;File(j)<File(i);j--) RankLeftMask[i] += (UNIT<<j);
    for (j=i+1;File(j)>File(i);j++) RankRightMask[i] += (UNIT<<j);
  }
   /* Masks for supporting pieces (usually pawns) on either flank */
  for (i=0;i<64;i++) {
    FlankMaskUp[i] = FlankMaskDown[i] = SideMask[i] = EMPTY;
    if (File(i)>0) FlankMaskUp[i] |= (FileUpMask[i-1] | Mask[i-1]);
    if (File(i)<7) FlankMaskUp[i] |= (FileUpMask[i+1] | Mask[i+1]);
    if (File(i)>0) FlankMaskDown[i] |= (FileDownMask[i-1] | Mask[i-1]);
    if (File(i)<7) FlankMaskDown[i] |= (FileDownMask[i+1] | Mask[i+1]);
    if (File(i)>0) SideMask[i] |= Mask[i-1];
    if (File(i)<7) SideMask[i] |= Mask[i+1];
    FlankMask[i] = FlankMaskDown[i] | FlankMaskUp[i];
  }
   /* Masks for pawn pairs seperated by at most 1 rank */
  for (i=0;i<64;i++) {
    PairMaskUp[i] = PairMaskDown[i] = EMPTY;
    if (File(i)>0) {
      PairMaskUp[i] |= Mask[i-1];
      if (Rank(i)>0) PairMaskUp[i] |= Mask[i-9];
      PairMaskDown[i] |= Mask[i-1];
      if (Rank(i)<7) PairMaskDown[i] |= Mask[i+7];
    }
    if (File(i)<7) {
      PairMaskUp[i] |= Mask[i+1];
      if (Rank(i)>0) PairMaskUp[i] |= Mask[i-7];
      PairMaskDown[i] |= Mask[i+1];
      if (Rank(i)<7) PairMaskDown[i] |= Mask[i+9];
    }
    PairMask[i] = PairMaskDown[i] | PairMaskUp[i];
  }

   /* Set up offside mask.  This contains a file mask for
    * all files NOT within 2 of the specified rank */
  for (i=0;i<8;i++) {
    OffsideMask[i] = EMPTY;
    for (j=0;j<8;j++) {
      if (abs(i-j)>2) OffsideMask[i] |= FileMask[j];
    }
  }

   /* Passed Pawn scoring based on board square. */
  for (i=0;i<64;i++) {
    PassedPawn[i] = (PassedPawnI[Rank(i)] * FileMod[File(i)]) / 10;
  }

   /* Backward pawns are worse in the centre */
  for (i=0;i<8;i++) {
    Backward[0][i] = 0;
    Backward[1][i] = BACKWARD_PAWN_1 / 2;
    Backward[2][i] = BACKWARD_PAWN_1;
    Backward[3][i] = (BACKWARD_PAWN_1 + BACKWARD_PAWN_2)/2;
    Backward[4][i] = BACKWARD_PAWN_2;
    Backward[5][i] = (BACKWARD_PAWN_2 + BACKWARD_PAWN_3)/2;
    Backward[6][i] = BACKWARD_PAWN_3;
    for (j=1;j<7;j++) {
      Backward[j][i] = (Backward[j][i] * (18 - FileMod[i])) / 10; 
    }
  }

   /* Masks for the three squares in front of (behind) passed pawns.
    * "Front" is taken as meaning in front of WHITE pawns */
  for (i=0; i<64; i++) {
    FrontMask[i] = PawnAttacksWhite[i];
    if (Rank(i) != Rank8) FrontMask[i] |= Mask[i-8];
    BackMask[i]  = PawnAttacksBlack[i];
    if (Rank(i) != Rank1) BackMask[i]  |= Mask[i+8];
  }
   /* Mask for the centre of the board minus files A and H (used for rook pawn tests) */
  EdgeMask = FileMask[FileA]|FileMask[FileH];
  CentreMask = ~EdgeMask;

   /* Penalty for trapping a rook as a function of gamestage */
  RookTrapped[Opening]  = ROOK_TRAPPED / 4;
  RookTrapped[EarlyMid] = ROOK_TRAPPED / 2;
  RookTrapped[Middle]   =(ROOK_TRAPPED * 2) / 3;
  RookTrapped[LateMid]  = ROOK_TRAPPED;
  RookTrapped[Endgame]  =(ROOK_TRAPPED * 3) / 2;
  RookTrapped[LateEnd]  = ROOK_TRAPPED * 2;

  /* Masks for proximate pawn shields */
  for (i=0;i<64;i++) {
    PawnShieldMaskWhite[i] = (FlankMaskUp[i]|FileUpMask[i]) &
                             BelowMask[Rank7] & KingSafetyMask[i];
    PawnShieldMaskBlack[i] = (FlankMaskDown[i]|FileDownMask[i]) &
                             AboveMask[Rank2] & KingSafetyMask[i];
  }

  /* Knight attacks on promotion race */
  for (i=0;i<64;i++) {
    mask = FileUpMask[i] | Mask[i];
    KnightAttackPromotionW[i] = EMPTY;
    while (mask) {
      j = FirstPiece(mask);
      KnightAttackPromotionW[i] |= KnightMoves[j];
      if (abs(j-i) > 8) KnightAttackPromotionW[i] |= DoubleKnightMove[j];
      RemoveFirst(mask);
    }
    mask = FileDownMask[i] | Mask[i];
    KnightAttackPromotionB[i] = EMPTY;
    while (mask) {
      j = FirstPiece(mask);
      KnightAttackPromotionB[i] |= KnightMoves[j];
      if (abs(j-i) > 8) KnightAttackPromotionB[i] |= DoubleKnightMove[j];
      RemoveFirst(mask);
    }
  }

   /* King safety based on the number of pawns left on the board */
  for (i=0;i<=16;i++) {
    KingSafetyPP[i][3] = KING_SAFETY;
    if (i<12) KingSafetyPP[i][3] += (KING_SAFETY * (12-i)) / 3;
    KingSafetyPP[i][4] = KingSafetyPP[i][3] / 2;
    KingSafetyPP[i][5] = KingSafetyPP[i][3] / 3;
    KingSafetyPP[i][6] = KingSafetyPP[i][3] / 4;
    KingSafetyPP[i][2] = (KingSafetyPP[i][3] * 3) / 2;
    KingSafetyPP[i][1] = (KingSafetyPP[i][2] * 3) / 2;
    KingSafetyPP[i][0] = (KingSafetyPP[i][1] * 3) / 2;

  }
   /* Phew */
}

/* Show the prompt */
void Prompt(void) {
  int ch=0;
  fprintf(stdout,"[%d]",(mvno/2)+1);
  if (Current_Board.side==WHITE) fprintf(stdout,"W");
  if (Current_Board.side==BLACK) fprintf(stdout,"B");
  if ( (ch = InCheck(&Current_Board,Current_Board.side)) ) {
    if (InCM(&Current_Board,ch)) fprintf(stdout,"#");
    else fprintf(stdout,"+");
  }
  else fprintf(stdout," ");
  fprintf(stdout,"> ");
}

void Defaults(void) {
   /* Reset the board to the opening layout */
  ResetBoard(&Current_Board);
#ifdef INITIAL_BOARD
  SetBoard(&Current_Board,INITIAL_BOARD);
#endif
  fprintf(stdout,"\n\n");
   
   /* Setup the computer parameters */
  Params.Depth = 5;                     /* Minimum Search Depth in ply.  Overrides 'MoveTime' */
  Params.Time = Params.OTime = 30000;   /* Total Clock Time = 5 minutes in centiseconds */
  Params.MoveTime = 10.0;               /* Maximum time in seconds.  'Depth' overrides this */
  Params.Test = FALSE;                  /* Not running a test suite */
  Params.Mps = 0;                       /* Moves per session.  0 = all */
  Params.Inc = 0;                       /* Time increase per move */
  automoves = 0;
  NHash = 0;
  TableW = TableB = NULL;
  Randomise();
  GlobalAlpha = -CMSCORE;
  GlobalBeta = CMSCORE;
}

/* Load in the opening book. */
void LoadOpeningBook(void) {
  int i,nm=0,sc=0;
  FILE *fp;
  char FEN[100],moves[200],ch;
  MOVE m;
  Openpos *O;
  
  NPos=0;
  
  if ((fp=fopen(BOOK_FILE,"r"))==NULL) {
    fprintf(stdout,"Could not find Opening Book %s!\n",BOOK_FILE);
    return;
  }
  fprintf(stdout,"Loading Opening Book %s ... ",BOOK_FILE);
  
  /* Step through the file, one line at a time */
  do {
    /* Read in the data */
    if (!fgets(FEN,100,fp)) break;
    for (i=0;i<(int)strlen(FEN);i++) if (FEN[i]<' ') {
      FEN[i] = 0;
      break;
    }
    if (strstr(FEN,"#END#")) break;
    if (!fgets(moves,200,fp)) break;
     
    NPos++;
    if (NPos>1) Openings = (Openpos *)realloc(Openings,sizeof(Openpos)*NPos);
    else Openings = (Openpos *)malloc(sizeof(Openpos));
    assert(Openings != NULL);
    O = (Openings+(NPos-1));
    O->FEN = (char *)malloc(sizeof(char)*(strlen(FEN)+1));
    assert(O->FEN != NULL);
    strncpy(O->FEN,FEN,strlen(FEN));
    O->FEN[strlen(FEN)]=0;
    
    /* Load in the suggested moves */
    i=-1;
    nm=0;
    do {
      do {ch=moves[++i];} while (ch==' ');
      if (ch != '\n') {
        m = (MOVE)moves[i]-97;
        m += (56-(MOVE)moves[++i])<<3;
        m += ((MOVE)moves[++i]-97)<<6;
        m += (56-(MOVE)moves[++i])<<9;
        
        ch=moves[++i];
        /* Check for a promotion move */
        if (ch!='{') {
          switch(toupper(ch)) {
          case 'Q': m += promote_q; break;
          case 'R': m += promote_r; break;
          case 'N': m += promote_n; break;
          case 'B': m += promote_b; break;
          }
          i++;
        }
        i++;
        
        sc=0;
        /* Get the score/weighting for this move */
        while ((ch=moves[i]) != '}') {
          sc *= 10;
          sc += (int)ch - 48;
          i++;
        }
        
        nm++;
        /* Update the opening data */
        if (nm>1) {
          O->m  = (MOVE *)realloc(O->m,sizeof(MOVE)*nm);
          O->sc = (int *)realloc(O->sc,sizeof(int)*nm);
        }
        else {
          O->m  = (MOVE *)malloc(sizeof(MOVE));
          O->sc = (int *)malloc(sizeof(int));
        }
        assert(O->m != NULL);
        assert(O->sc != NULL);
        O->m[nm-1]=m;
        O->sc[nm-1]=sc;
        ch = moves[++i];
      }
    } while (ch != '\n');
    
    /* Set the number of moves */
    O->nmoves=nm;
  } while (!feof(fp));
  
  fclose(fp);   
  fprintf(stdout,"OK\n[%d Position",NPos);
  if (NPos!=1) fprintf(stdout,"s");
  fprintf(stdout,"]\n");
}

/* Frees the memory allocated to the opening book */
void FreeOpenings(void) {
  int n;
  for (n=0;n<NPos;n++) {
    free(Openings[n].FEN);
    free(Openings[n].m);
    free(Openings[n].sc);
  }
  free (Openings);
}

/* Parse the command line input (if any) */
int ParseCommandLine(int argc,char **argv) {
  int n,i;
  char *arg,*loc;
   
  if (argc<2) return 1;
  fprintf(stdout,"%d Argument(s) Obtained\n",argc-1);
  for (n=1;n<argc;n++) {
    arg = argv[n];
    for (i=0;i<(int)strlen(arg);i++) {arg[i] = toupper(arg[i]);if (arg[i]=='=') break;}
    fprintf(stdout,"Argument %d: %s\n",n,arg);
    if (strstr(arg,"HELP") || strchr(arg,'?')) {PrintArgs();return 0;}
    if ((loc=strstr(arg,"HASH"))) Params.HashSize = (int)strtol(loc+4,NULL,10);
    if ((loc=strstr(arg,"WSKILL"))) Params.WSkill = (int)strtol(loc+6,NULL,10);
    if ((loc=strstr(arg,"BSKILL"))) Params.BSkill = (int)strtol(loc+6,NULL,10);
    if ((loc=strstr(arg,"TBLOC="))) strncpy(TB_PATH,loc+6,sizeof(TB_PATH));
    if ((loc=strstr(arg,"BOOK="))) strncpy(BOOK_FILE,loc+5,sizeof(BOOK_FILE));
    if ((loc=strstr(arg,"PERSON="))) strncpy(PERSON_FILE,loc+7,sizeof(PERSON_FILE));
    if ((loc=strstr(arg,"EPDTEST="))) strncpy(EPD_FILE,loc+8,sizeof(EPD_FILE));
    if ((loc=strstr(arg,"ST"))) testtime = (int)strtol(loc+2,NULL,10);
  }
  return 1;
}

/* Print command line arguments */
void PrintArgs(void) {
  fprintf(stdout,"Command Line Arguments:\n\n");
  fprintf(stdout,"?, help            - Print this file\n");
  fprintf(stdout,"hash<num>          - Set the hash table size (2^n kb)\n");
  fprintf(stdout,"tbloc=<directory>  - Set the tablebase location\n");
  fprintf(stdout,"book=<filename>    - Set the opening book\n");
  fprintf(stdout,"person=<filename>  - Set the personality file\n");
  fprintf(stdout,"wskill<num>        - Set the skill level for white (1 to 10)\n");
  fprintf(stdout,"bskill<num>        - Set the skill level for black (1 to 10)\n");
  fprintf(stdout,"epdtest=<file>     - Run an EPD test and quit immediately\n");
  fprintf(stdout,"st<num>            - Set the time per move for an EPD test (default 5s)\n");
}

/* Get any environment variables that are set
 * Does anyone actually use these any more? */
void GetEnvironmentVars(void) {
  char *where;

  if ((where = getenv("EGTBPATH"))) 
    strncpy(TB_PATH, where, sizeof(TB_PATH));

  if ((where = getenv("BOOKFILE"))) 
    strncpy(BOOK_FILE, where, sizeof(BOOK_FILE));

  if ((where = getenv("PERSONALITYFILE"))) 
    strncpy(PERSON_FILE, where, sizeof(PERSON_FILE));
}

/* Get the setup from the config file, if it exists */
void GetConfigFile(void) {
  FILE *fp;
  char ch;
   
   /* Set up some default values in case config file is not found */
  Params.WSkill = 10;                   /* [1-10] Higher is stronger */
  Params.BSkill = 10;                   /* [1-10] Higher is stronger */
  Params.HashSize = 14;                 /* Hash Table Size is 2^HashSize kb */
  Params.Resign = TRUE;                 /* Do we resign? */

   /* Open the config file */
  if ((fp=fopen("beowulf.cfg","r"))==NULL) {
    fprintf(stdout,"Could not Load Config File beowulf.cfg.  Continuing...\n");
    return;
  }
   /* Browse the config file for the relevant data */
  do {
    fscanf(fp,"%c",&ch);
    switch(ch) {
     case '#' : 
       do {
         fscanf(fp,"%c",&ch);
       } while (ch != '\n' && !feof(fp)); break;
     case 'H' : fscanf(fp,"ASH %d\n",&Params.HashSize); break;
     case 'T' : fscanf(fp,"BLOC %s\n",TB_PATH); break;
     case 'O' : fscanf(fp,"PBOOK %s\n",BOOK_FILE); break;
     case 'P' : fscanf(fp,"ERSON %s\n",PERSON_FILE); break;
     case 'W' : fscanf(fp,"SKILL %d\n",&Params.WSkill); break;
     case 'B' : fscanf(fp,"SKILL %d\n",&Params.BSkill); break;
     case 'R' : fscanf(fp,"ESIGN O%c",&ch);
                if (ch == 'N') Params.Resign = TRUE;
                else {Params.Resign = FALSE;fscanf(fp,"F");}
                fscanf(fp,"\n");
     default  : break;
    }
  } while (!feof(fp));
  fclose(fp);
}

/* Check the parameters are legal */
void CheckParams(void) {
  if (Params.HashSize<0) Params.HashSize=0;
  if (Params.HashSize>20) Params.HashSize=20;
  if (Params.WSkill>10) Params.WSkill=10;
  if (Params.WSkill<1) Params.WSkill=1;
  if (Params.BSkill>10) Params.BSkill=10;
  if (Params.BSkill<1) Params.BSkill=1;
}

/* Load in the personality file */
void LoadPersonalityFile(void) {
  FILE *fp;
   
  init_personality();

  if ((fp = fopen(PERSON_FILE,"r")) != NULL) {
    fprintf(stdout,"Loading Personality File %s ... ",PERSON_FILE);
    (void)read_person(fp);
    fclose(fp);
    fprintf(stdout,"OK\n");
  }
  else {
    fprintf(stdout,"Could not find Personality File %s.  Continuing...\n",PERSON_FILE);
  }
}

/* Sorts out what stage of the game the board is in.  This is now
 * used for a lookup table instead. */
int GetGamestage(int tpts) {
  if (tpts>70) return Opening;
  if (tpts>62) return EarlyMid;
  if (tpts>54) return Middle;
  if (tpts>46) return LateMid;
  if (tpts>22) return Endgame;
  return LateEnd;
}


