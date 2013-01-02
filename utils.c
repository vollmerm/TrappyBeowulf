/**********************************
 *    utils.c                     *
 *    Colin Frayn                 *
 *    March 2001                  *
 **********************************/

/*
  This file contains all the miscellaneous functions
  that don't really fit elsewhere
*/

#ifdef _WIN32
#include <windows.h>
#include <conio.h>
#endif
#include <stdlib.h>
#include <ctype.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

#include "common.h"
#include "utils.h"
#include "board.h"
#include "parser.h"
#include "comp.h"
#include "computil.h"
#include "moves.h"
#include "checks.h"
#ifndef _WIN32
# include <sys/types.h>
# include <sys/time.h>
# include <unistd.h>
#endif //!_WIN32

extern CompDat Params;
extern int mvno,NPos,GlobalDepth,TimeTaken,TimeToSolution,Gamestage[256];
extern longlong Nodecount;
extern int PreviousScore;
extern char PV_text[1000];
extern Board Current_Board;
extern BOOL CMFound,XBoard,LoadPGN,BookON;
extern MOVE MoveHistory[1000];
extern Undo UndoHistory[1000];
extern Openpos *Openings;

/* Converts a char into a piece value */
int CharToPiece(const char P) {
  switch(P) {
    case('P') : return wpawn;
    case('R') : return wrook;
    case('N') : return wknight;
    case('B') : return wbishop;
    case('Q') : return wqueen;
    case('K') : return wking;
    case('p') : return bpawn;
    case('r') : return brook;
    case('n') : return bknight;
    case('b') : return bbishop;
    case('q') : return bqueen;
    case('k') : return bking;
  }
  return empty;
}

/* Converts a square held in 2 chars into a square ID */
int GetSquare(const char x,const char y) {
  int sq;
  
  sq  = (int)(toupper(x)) - 65;
  sq += 8*(56 - (int)y);
   
  return sq;
}

/* Run an EPD Test Suite */
void RunTestSuite(char EPD_FILE[FILENAME_MAX], int limit) {
  char Suite[FILENAME_MAX],FileTemp[FILENAME_MAX],temp[10]="",Problem[FILENAME_MAX];
  char answer[10][20],FEN[100]="",ID[100];
  Board OldBoard = Current_Board;
  int time_limit=limit,oldmvno=mvno,oldside=Current_Board.side,score=0,total=0,n;
  int TotalTTS=0, NMissed=0;
  MOVE Bestmove;
  FILE *fp,*out=NULL,*missed=NULL;
  BOOL OldBook = BookON;
  
  if (strlen(EPD_FILE)==0) {
     /* Get directory listing */
    fprintf(stdout,"Directory of EPD Files\n\n");
    (void)system(DIR_COMMAND"*.epd");
    fprintf(stdout,"\nPlease Choose : ");
     /* Get chosen
	filename */
    (void)fgets(Suite,50,stdin);
    Suite[strlen(Suite)-1]=0;
    if (!strstr(Suite,".epd")) strcat(Suite,".epd");
  }
  else strncpy(Suite,EPD_FILE,sizeof(Suite));
  
   /* Try to open chosen file */
  if ((fp = fopen(Suite,"r"))==NULL) {
    fprintf(stdout,"Can't Open File %s!\n",Suite);
    return;
  }
  fprintf(stdout,"%s Loaded OK!\n",Suite);
   
   /* Get output file */
  if (strlen(EPD_FILE)==0) {  
    fprintf(stdout,"Output Log File ('.' for none) : ");
    (void)fgets(FileTemp,50,stdin);
    FileTemp[strlen(FileTemp)-1]=0;
    if (FileTemp[0]=='.') out=NULL;
     /* Try to open chosen file */
    else {
      if ((out = fopen(FileTemp,"w"))==NULL) {
        fprintf(stdout,"Can't Open Output File %s!\n",FileTemp);
        out=NULL;
      }
      fprintf(stdout,"%s Opened OK!\n",FileTemp);
    }
  }

   /* Set up the 'missed positions' file which, for those who don't speak American,
    * contains a list of all positions that Beowulf failed to solve in this suite. */
  if (strlen(EPD_FILE)==0) {  
    fprintf(stdout,"Missed Positions File ('.' for none) : ");
    (void)fgets(FileTemp,50,stdin);
    FileTemp[strlen(FileTemp)-1]=0;
    if (FileTemp[0]=='.') missed=NULL;
     /* Try to open chosen file */
    else {
      if ((missed = fopen(FileTemp,"w"))==NULL) {
        fprintf(stdout,"Can't Open Missed Positions File %s!\n",FileTemp);
        missed=NULL;
      }
      if (missed) fprintf(stdout,"%s Opened OK!\n",FileTemp);
    }
  }

   /* Get time per position */
  if (time_limit==0) {     
    do {
      fprintf(stdout,"Set Time Limit [1-3600s] : ");
      (void)fgets(temp,10,stdin);
      time_limit = (int)strtol(temp,NULL,10);
    } while (time_limit<1 || time_limit>3600);
  }

   /* Setup the computer parameters */
  Params.MoveTime = (float)time_limit;
  Params.Depth = 2;
  Params.Test = TRUE;
  BookON = FALSE;    

   /* Load in the positions, one at a time */
  while (!feof(fp)) {
     /* Load in the problem */
    if (!fgets(Problem, (int)sizeof(Problem), fp)) continue;
     /* Parse the EPD string */
    if (!ParseEPD(Problem,FEN,answer,ID)) continue;

     /* Set the board to the new string */
    SetBoard(&Current_Board,FEN,TRUE);
     /* Print the new board */
    fprintf(stdout,"\n\nPosition %d - ",total+1);
    if (Current_Board.side==WHITE) fprintf(stdout,"White to play\n");
    else                           fprintf(stdout,"Black to play\n");
    if (total>9)    fprintf(stdout,"-");
    if (total>99)   fprintf(stdout,"-");
    if (total>999)  fprintf(stdout,"-");
    if (total>9999) fprintf(stdout,"-");
    fprintf(stdout,"--------------------------\n");
    fprintf(stdout,"ID = %s\nFEN = %s\n",ID,FEN);
    PrintBoard(Current_Board);
     /* Analyse this position */
    Bestmove = Comp();
     /* Print to logfile */
    if (out) {
      fprintf(out,"%s acd %d; acn %.0f; acs %.2f; bm ",FEN,GlobalDepth,(double)Nodecount,(float)TimeTaken/100.0);
      n=-1;
      while (strlen(answer[++n])>0) {if (n>0) fprintf(out," ");fprintf(out,"%s",answer[n]);}
      fprintf(out,"; ce %d; id %s; pv %s",PreviousScore,ID,PV_text);
    }    
    /* Check if this move is correct */
    if (CorrectMove(Bestmove,answer)) {
      fprintf(stdout," ** Correct **\n");
      if (out) fprintf(out,"; c0 \"Correct\"");
      fprintf(stdout,"Time To Solution = %.2f sec\n",(float)(TimeToSolution)/100.0);
      if (out) fprintf(out,"; c1 \"tts = %.2f\"",(float)(TimeToSolution)/100.0);
      score++;
      TotalTTS += TimeToSolution;
    }
    else {
      if (CMFound) {
        fprintf(stdout," ** Alternative CM Found **\n");
        if (out) fprintf(out,"; c0 \"Alternative Mate\"");
        if (TimeToSolution > 0) fprintf(stdout,"Time To Solution = %.2f sec\n",(float)(TimeToSolution)/100.0);
        else fprintf(stdout,"Time To Solution < 0.01 sec\n");
        if (out) fprintf(out,"; c1 \"tts = %.2f\"",(float)(TimeToSolution)/100.0);
        score++;
        TotalTTS += TimeToSolution;
      }
      else {
        fprintf(stdout," !! Incorrect !!\n");
        if (out) fprintf(out,"; c0 \"Incorrect\"");
        TotalTTS += TimeTaken;
        NMissed++;
         /* Write this position to the 'missed positions' file */
        if (missed) fprintf(missed, "%s",Problem);
      }
    }
    if (out) fprintf(out,"\n");
    fprintf(stdout,"Correct So Far : %d/%d\n",score,total+1);
    total++;
  }
  
  /* Close the file handles */
  fclose(fp);
  if (missed) fclose(missed);

   
   /* Print out the results */
  fprintf(stdout,"\n\nTest Completed.\nResults;\n");
  fprintf(stdout,"Correct : %d/%d\n",score,total);
  fprintf(stdout,"Average Time To Solution = %.2f sec\n",(float)(TotalTTS)/((float)total*100.0));
  if (out) {
    fprintf(out,"Test Summary;\n");
    fprintf(out,"Correct : %d/%d\n",score,total);
    fprintf(out,"Average Time To Solution = %.2f sec\n",(float)(TotalTTS)/((float)total*100.0));
    fprintf(out,"---END LOG---\n");
    fclose(out);
  }   

   /* Reset the board to how it was */
  Current_Board = OldBoard;
  Current_Board.side = oldside; mvno = oldmvno;
  Params.Test = FALSE;
  BookON = OldBook;
}

/* EPD Parser.  Pretty simple, but it does the job */
BOOL ParseEPD(char *line,char FEN[100],char answer[10][20], char ID[100]) {
  char *ans,*id;
  int blankcount=0,i=0,nans=0,l;
   
  if (strlen(line)<20) return FALSE;

  while (!isalnum((int)line[i])) i++;
  
  do {
    while (line[i]!=' ') i++;
    blankcount++;
    i++;
  } while (blankcount<4);
  strcpy(FEN,"");
  strncpy(FEN,line,i);
  FEN[i]=0;
   
  ans = strstr(line,"bm ");
  if (!ans) return FALSE;
  ans+=2;
   
  do {
    l=0;
    ans++;
    do {
      answer[nans][l++]= *ans++;
    } while (*ans != ' ' && *ans != ';');
    answer[nans][l]=0;
    nans++;
  } while (*ans != ';');
   
  for (i=nans;i<10;i++) strcpy(answer[i],"");

   /* Get the position ID */
  id = strstr(line,"id ");
  if (id) {
    id += 3;
    l=0;
    while (*id!=';' && *id!=0) ID[l++]=*id++;
    ID[l]=0;
  }
  else strcpy(ID,"");
   
  return TRUE;
}
 
 /* See if the move 'm' is in the list 'answer' */ 
BOOL CorrectMove(MOVE m, char answer[10][20]) {
  int n=-1; 

  fprintf(stdout,"--Answer");
  if (strlen(answer[1])>0) fprintf(stdout,"s");
  fprintf(stdout,": ");
  while (strlen(answer[++n])>0) fprintf(stdout,"%s ",answer[n]);
  fprintf(stdout,"  ");
   
  n=-1;
  while (strlen(answer[++n])>0) {
    if (SameMove(m,answer[n])) return TRUE; 
  }
  return FALSE;
}
 
/* Test to see if the two moves are the same.  Assume that this is played on
 * the current board position */
BOOL SameMove(MOVE m, char move[20]) {
  int length=strlen(move);
  int mf = MFrom(m),mt=MTo(m);
   
   /* Obviously incorrect moves */
  if (m==NO_MOVE || length<2 || length>7) return FALSE;
      
   /* Check for a castling move */
  if (IsCastle(m) && mt<mf && !strcmp(move,"0-0-0")) return TRUE;
  if (IsCastle(m) && mt<mf && !strcmp(move,"O-O-O")) return TRUE;
  if (IsCastle(m) && mt>mf && !strcmp(move,"0-0"))   return TRUE;
  if (IsCastle(m) && mt>mf && !strcmp(move,"O-O"))   return TRUE;

   /* Remove trailing '+' and '#' characters */
  if (move[length-1] == '+' || move[length-1]=='#') length--;
   
  LoadPGN =  TRUE;
  if (m == ParseMove(move,Current_Board,FALSE)) return TRUE;
  LoadPGN = FALSE;

  return FALSE;
}

/* Check if the current position is drawn.  Also tests for check and checkmate. */
int CheckForDraw(void) {
  Board B = Current_Board;
  int n = mvno-1,fifty=0,count=0,nmoves,inchk,vars,bvars,tpts=max(B.BPts,B.WPts);
  unsigned longlong oldkey;
  MOVE m;
   
  GenerateHashKey(&B);
  oldkey = B.Key;
  vars = B.castle;
  if (B.side==WHITE) vars+=16;

  inchk = InCheck(&B,B.side);
  nmoves = (int)CountMoves(&B,1,1);
  if (nmoves==0) {
    if (inchk) {Result(5);return 5;}
    else {Result(4);return 4;}
  }
  else if (inchk) Result(6);

   /* Test for lack of force */
  if (tpts < 4 && !(B.WhitePawns | B.BlackPawns)) {Result(3);return 3;}
   
  while (n>=0) {
    m = MoveHistory[n];
    if (UndoHistory[n].capt) break;
    if (IsPromote(m) || IsEP(m)) break;
    if (PType(B.pieces[MTo(m)])==pawn) break;     
    GenerateHashKey(&B);
    bvars = B.castle;
    if (B.side==WHITE) bvars+=16;
    if (oldkey == B.Key && bvars==vars) count++;
    if (count==3) {Result(1);return 1;}
    fifty++;
    if (fifty>100) {Result(2);return 2;}
    UndoMove(&B, MoveHistory[n], UndoHistory[n]);
    n--;
  }
  return 0;
}

/* Print off a notice declaring a draw */
void Result(int type) {
  if (!XBoard) {
    if (type==1) fprintf(stdout,"You May Claim Draw by Repetition!\n");
    if (type==2) fprintf(stdout,"You May Claim Draw by the Fifty Move Rule!\n");
    if (type==3) fprintf(stdout,"You May Claim Draw by Lack of Force!\n");
    if (type==4) fprintf(stdout,"Draw by Stalemate!\n");
    if (type==5) fprintf(stdout,"Checkmate!\n");
    if (type==6) fprintf(stdout,"Check!\n");
  }
  else {
    if (type==1) fprintf(stdout,"1/2-1/2 {Draw by Repetition}\n");
    if (type==2) fprintf(stdout,"1/2-1/2 {Draw by Fifty Move Rule}\n");
    if (type==3) fprintf(stdout,"1/2-1/2 {Draw by Lack of Force}\n");
    if (type==4) fprintf(stdout,"1/2-1/2 {Stalemate}\n");
    if (type==5 && Current_Board.side==WHITE) fprintf(stdout,"0-1 {Black Mates}\n");
    if (type==5 && Current_Board.side==BLACK) fprintf(stdout,"1-0 {White Mates}\n");
  }
}

/* Check to see if this position is stored in the opening book, and if so
 * then return the suggested move.  Else return NO_MOVE. Val holds the percentage
 * weighting for the chosen move */
MOVE CheckOpening(Board *B, int *val) {
  char FEN[100];
  int n,i,weight=0,tweight=0;
  Openpos *O;
  
   /* Don't bother searching opening book for positions in developed games */
  if (Gamestage[B->WPts + B->BPts] > Middle) return NO_MOVE;
   /* Get the FEN for the current board */
  BoardToFEN(B,FEN);
   /* Cycle through the positions in the opening book */
  for (n=0;n<NPos;n++) {
    O = (Openings+n);
     
     /* See if this one matches */
    if (!strcmp(FEN,O->FEN)) {
      for (i=0;i<O->nmoves;i++) tweight += O->sc[i];
      if (tweight==0) return NO_MOVE;
      weight = Random(tweight);
       /* Get a move randomly, biased by move weights */
      for (i=0;i<O->nmoves;i++) {
      	weight -= O->sc[i];
	      if (weight<0) {*val = O->sc[i]*100/tweight;return O->m[i];}
      }
      return NO_MOVE;
    }
  }
  return NO_MOVE;
}

/* Convert a board position to a FEN notation line */
void BoardToFEN(Board *B, char *FEN) {
  int x,y,l=0,i=0,sq;
  char row[8],ch[3];
  
  strcpy(FEN,"");
  for (y=0;y<8;y++) {
    i=l=0;
    strcpy(row,"");
    for (x=0;x<8;x++) {
      sq = (y*8)+x;
      if (B->pieces[sq]==0) l++;
      else {
	if (l>0) {row[i]=(char)(l+48);i++;}
	l=0;
	switch (B->pieces[sq]) {
	 case (wpawn)  : row[i]='P'; break;
	 case (wrook)  : row[i]='R'; break;
	 case (wknight): row[i]='N'; break;
	 case (wbishop): row[i]='B'; break;
	 case (wqueen) : row[i]='Q'; break;
	 case (wking)  : row[i]='K'; break;
	 case (bpawn)  : row[i]='p'; break;
	 case (brook)  : row[i]='r'; break;
	 case (bknight): row[i]='n'; break;
	 case (bbishop): row[i]='b'; break;
	 case (bqueen) : row[i]='q'; break;
	 case (bking)  : row[i]='k'; break;
	}
	i++;
      }
    }
    if (l>0) {row[i]=(char)(l+48);i++;}
    strncat(FEN,row,i);
    if (y<7) strcat(FEN,"/");
  }
   
  if (B->side==WHITE) strcat(FEN," w ");
  else                strcat(FEN," b ");
   
  if (B->castle&1) strcat(FEN,"K");
  if (B->castle&2) strcat(FEN,"Q");
  if (B->castle&4) strcat(FEN,"k");
  if (B->castle&8) strcat(FEN,"q");
  if (B->castle==0) strcat(FEN,"-");
   
  if (B->ep==-1) strcat(FEN," -");
  else {
    strcat(FEN," ");
    ch[0] = (char)((B->ep)&7) + 97;
    ch[1] = (char)(56 - ((B->ep>>3)&7));
    ch[2]=0;
    strcat(FEN,ch);
  }
}

/* Get a hint for a move in the current position from the hash table */
void GetHint(void) {
  HashElt *Entry;
  MOVE BookMove;
  int dummy;
   
   /* Check the Opening Book First */
  BookMove = CheckOpening(&Current_Board,&dummy);
  if (BookMove!=NO_MOVE) {
     /* Check some values for the book move, i.e. EP and castle */
    BookMove = CheckMove(BookMove);
    if (XBoard) fprintf(stdout,"Hint: ");
    else fprintf(stdout,"I suggest you play ");
    PrintMove(BookMove,FALSE,stdout);
    fprintf(stdout,"\n");
    return;
  }
   
  GenerateHashKey(&Current_Board);
  Entry = HashProbe(&Current_Board);
   
  if (Entry) {
    if (Entry->move != NO_MOVE) {
      if (XBoard) fprintf(stdout,"Hint: ");
      else fprintf(stdout,"I suggest you play ");
      PrintMove(Entry->move,FALSE,stdout);
      fprintf(stdout,"\n");
    }
  }
  else if (!XBoard) fprintf(stdout,"No Suggested Move Available\n");
}

/* Bioskey checks for input waiting in stdin.
 * Credit here goes to Dr Oliver Brausch for the code from his
 * program Olithink.  Some rewriting has been done by CMF, but not much
 * as I have absolutely no idea whatsoever what this does.
 * I think the original code is from Crafty by Prof. Robert Hyatt. */

#ifndef _WIN32
/* Non-windows version */
int Bioskey(void) {
  fd_set readfds;
  struct timeval timeout;
   
  FD_ZERO(&readfds);
  FD_SET(fileno(stdin), &readfds);
   /* Set to timeout immediately */
  timeout.tv_sec = 0;
  timeout.tv_usec = 0;
  select(16, &readfds, 0, 0, &timeout);
   
  return (FD_ISSET(fileno(stdin), &readfds));
}

#else
/* Windows-version */
int Bioskey(void) {
  static int init = 0, pipe;
  static HANDLE inh;
  DWORD dw;
   /* If we're running under XBoard then we can't use _kbhit() as the input commands
    * are sent to us directly over the internal pipe */
  if (XBoard) {
#if defined(FILE_CNT)
    if (stdin->_cnt > 0) return stdin->_cnt;
#endif
    if (!init) {
      init = 1;
      inh = GetStdHandle(STD_INPUT_HANDLE);
      pipe = !GetConsoleMode(inh, &dw);
      if (!pipe) {
        SetConsoleMode(inh, dw & ~(ENABLE_MOUSE_INPUT | ENABLE_WINDOW_INPUT));
        FlushConsoleInputBuffer(inh);
      }
    }
    if (pipe) {
      if (!PeekNamedPipe(inh, NULL, 0, NULL, &dw, NULL)) return 1;
      return dw;
    }
    else {
      GetNumberOfConsoleInputEvents(inh, &dw);
      return dw <= 1 ? 0 : dw;
    }
  }
  else return _kbhit();
}
#endif

/* Study a pgn file for adding to the opening book.
 * Parses each game and plays through it until the first
 * unknown position or move is encountered.  If the side
 * to move eventually wins then it learns that position
 * and the best move. */
void StudyFile(void) {
  char PGNFile[FILENAME_MAX],temp[10]="",FEN[100],AcceptDraws, Verbose, Weight;
  Board B;
  MOVE *moves,move,bookmove;
  int maxdepth,nmoves,elo,minelo,minlength,n,opno,m,winner,longest=0;
  int NumGames=0, NewPositions=0, NewMoves=0, breakout=0,from,to;
  Openpos *O;
  FILE *fp;
  
  /* Get directory listing */
  fprintf(stdout,"Directory of PGN Files\n\n");
  (void)system(DIR_COMMAND"*.pgn");
  fprintf(stdout,"\nPlease Choose : ");
  /* Get chosen filename */
  (void)fgets(PGNFile,50,stdin);
  PGNFile[strlen(PGNFile)-1]=0;
  if (!strstr(PGNFile,".pgn")) strcat(PGNFile,".pgn");
  
  /* Try to open chosen file */
  if ((fp = fopen(PGNFile,"r"))==NULL) {
    fprintf(stdout,"Can't Open File %s!\n",PGNFile);
    return;
  }
  fprintf(stdout,"%s Loaded OK!\n",PGNFile);
  
  do {
    fprintf(stdout,"Maximum Number of Ply to Examine [>=1] : ");
    (void)fgets(temp,sizeof(temp),stdin);
    maxdepth = (int)strtol(temp,NULL,10);
  } while (maxdepth<1);
  
  do {
    fprintf(stdout,"Minimum ELO of Winning Player [>=0] : ");
    (void)fgets(temp,sizeof(temp),stdin);
    minelo = (int)strtol(temp,NULL,10);
  } while (minelo<0);
  
  do {
    fprintf(stdout,"Minimum Game Length (ply) [>=1] : ");
    (void)fgets(temp,sizeof(temp),stdin);
    minlength = (int)strtol(temp,NULL,10);
  } while (minlength<1);
  
  do {
    fprintf(stdout,"Accept Drawn Games? [Y/N] : ");
    (void)fgets(temp,sizeof(temp),stdin);
    AcceptDraws = toupper(temp[0]);
  } while (AcceptDraws != 'N' && AcceptDraws != 'Y');
  
  do {
    fprintf(stdout,"Verbose Output? [Y/N] : ");
    (void)fgets(temp,sizeof(temp),stdin);
    Verbose = toupper(temp[0]);
  } while (Verbose != 'N' && Verbose != 'Y');

  do {
    fprintf(stdout,"Weight Moves by ELO? [Y/N] : ");
    (void)fgets(temp,sizeof(temp),stdin);
    Weight = toupper(temp[0]);
  } while (Weight!= 'N' && Weight != 'Y');
  
  if (Verbose == 'N') fprintf(stdout,"Studying ");  

  /* Load in the games, one at a time, and study them */
  do {

    /* Load in the next game */
    moves = ImportPGNGame(fp,&winner,&nmoves,&elo, NULL, NULL);

    // Dud game?
    if (moves == NULL || nmoves==0) {
      fprintf(stdout,"x");
      break;
    }

    breakout = 0;
    NumGames++;

     /* Print game details */
    if (Verbose == 'Y') {
      fprintf(stdout,"\nLoaded Game Number %d  :  %d Moves, Winner = ",NumGames,nmoves);
      switch(winner) {
        case (WHITE) : fprintf(stdout,"White  "); break;
        case (BLACK) : fprintf(stdout,"Black  "); break;
        default      : fprintf(stdout,"Drawn  "); break;
      }
      if (winner != NEUTRAL) {
        fprintf(stdout,"Winning ELO = ");
        if (elo > 0) fprintf(stdout,"%d",elo);
        else fprintf(stdout,"Unknown");
      }
      fprintf(stdout,"\n");
      if (elo < minelo) fprintf(stdout,"Winner's ELO (%d) too low - Skipping!\n",elo);
      if (nmoves < minlength) fprintf(stdout,"Game too short (%d moves) - Skipping!\n",nmoves);
      if (AcceptDraws == 'N' &&  winner == NEUTRAL) fprintf(stdout,"Game was a draw - Skipping!\n");
    }
     /* Print (tidy) progress */
    else {
      if (NumGames % 1000 == 0) fprintf(stdout,"[%d]",NumGames);
      else if (NumGames % 100 == 0) fprintf(stdout,"x");
      else fprintf(stdout,".");
    }

     /* Check to see if we can reject this game */
    if (elo < minelo) {free(moves); continue;}
    if (nmoves < minlength) {free(moves); continue;}
    if (AcceptDraws == 'N' &&  winner == NEUTRAL) {free(moves); continue;}
    
    ResetBoard(&B);
    /* Step through the moves */
    for (n=0;n<nmoves;n++) {
      // Set up this move
      if (Verbose == 'Y') fprintf(stdout,"%d ",n+1);
      move = moves[n];
      assert(move != NO_MOVE);
      if (n >= maxdepth) break;
      if (n>longest) longest = n;
      
      /* Skip moves for the losing side */
      if (B.side != winner && winner != NEUTRAL) {
        (void)DoMove(&B, move);
        continue;
      }
      
      /* Get the FEN for the current board */
      BoardToFEN(&B,FEN);
      
      /* Cycle through the positions in the opening book */
      for (opno=0;opno<NPos;opno++) {
        O = (Openings+opno);
        
        /* See if this opening book entry matches the current board position */
        if (!strcmp(FEN,O->FEN)) {
          assert(O->nmoves > 0);
          
          /* It matches!  Now check the moves to see if the move played is
           * already stored. */
          for (m=0;m<O->nmoves;m++) {
            bookmove = O->m[m];
            from = MFrom(bookmove); to = MTo(bookmove);
            /* Check if this is an en-passant move */
            if (!IsEP(bookmove) && PType(B.pieces[from])==pawn && File(to)!=File(from) && B.pieces[to]==empty)
              bookmove |= SPECIAL_FLAG;
            /* Check if this is a castling move */
            if (!IsCastle(bookmove)) {
              if (PType(B.pieces[from])==king && File(from)==FileE && File(to)==FileC)
                bookmove |= SPECIAL_FLAG;
              if (PType(B.pieces[from])==king && File(from)==FileE && File(to)==FileG)
                bookmove |= SPECIAL_FLAG;
            }
            /* If we've found this exact move - just increase its weighting by an amount
            * varying with the ELO of whoever played it.  If the score is currently
            * zero then leave it alone (this is a move that we want to be aware of,
            * but never actually play). */
            if (bookmove == move && O->sc[m]>0) {
              if (Weight == 'Y') O->sc[m] += ELOBonus(elo);
              else O->sc[m]++;
            }
            if (bookmove == move) break;
          }
          /* This is a new move - add it to the list */
          if (m == O->nmoves) {
            (O->nmoves)++;
            O->m  = (MOVE *)realloc(O->m,sizeof(MOVE)*(m+1));
            O->sc = (int *)realloc(O->sc,sizeof(int)*(m+1));
            assert(O->m != NULL);
            assert(O->sc != NULL);
            O->m[m] = move;
            O->sc[m] = ELOBonus(elo);
            if (Verbose == 'Y') {
              fprintf(stdout,"\nExisting Position %s\n",FEN);
              fprintf(stdout,"Added New Move ");	      
              PrintMove(move,FALSE,stdout);
            }
            /* Stop parsing this game */
            breakout = 1;
            NewMoves++;
          }
          break;
        }
      }

      /* This position was not found - add it! */
      if (opno == NPos) {
        NPos++;	
        if (NPos>1) Openings = (Openpos *)realloc(Openings,sizeof(Openpos)*NPos);
        else Openings = (Openpos *)malloc(sizeof(Openpos));
        assert(Openings != NULL);
        O = (Openings+opno);
        O->FEN = (char *)malloc(sizeof(char)*(strlen(FEN)+1));
        assert(O->FEN != NULL);
        strncpy(O->FEN,FEN,strlen(FEN));
        O->FEN[strlen(FEN)]=0;
        O->nmoves = 1;
        O->m = (MOVE *)malloc(sizeof(MOVE));
        O->sc = (int *)malloc(sizeof(int));
        assert(O->m != NULL);
        assert(O->sc != NULL);
        O->m[0] = move;
        O->sc[0] = ELOBonus(elo);
        if (Verbose == 'Y') {
          fprintf(stdout,"\nLearning New Position %s\n",FEN);
          fprintf(stdout,"Best Move ");
          PrintMove(move,FALSE,stdout);
        }
        /* Stop parsing this game */
        breakout = 1;
        NewPositions++;
      }
      if (breakout) break;
      (void)DoMove(&B,move);
    }
    free(moves);
    if (Verbose == 'Y') fprintf(stdout,"\n");
  } while (!feof(fp));
  
  /* Close the input file handle */
  fclose(fp);
  
  /* Print out the results */
  fprintf(stdout,"\nFile %s Studied\n",PGNFile);
  fprintf(stdout,"Total Games         : %d\n",NumGames);
  fprintf(stdout,"Total New Positions : %d\n",NewPositions);
  fprintf(stdout,"Total New Moves     : %d\n",NewMoves);
  fprintf(stdout,"Longest Line        : %d\n\n",longest);
  fprintf(stdout,"Type 'savebook' to write out the new positions\n\n");
}

/* Does what it says.  Assumes that the game is 1000 moves long or less.
 * I think that's a safe assumption ;) */
MOVE *ImportPGNGame(FILE *fp, int *winner, int *nmoves, int *elo,
                    char *WhitePlayer, char *BlackPlayer) {
  MOVE *movelist = NULL,templist[1000],move;
  int n,cno=0,welo=0,belo=0,breakout=0,bracket_count=0;
  char ch,chh=0,*location,movech[10],temp[1024],*tempch;
  Board B;

  *nmoves = 0;
  *winner = NEUTRAL; /* Assume draw */
  *elo = 0; /* Assume no good players */
   /* Work out who won this game and skip the bits at the top.
    * Also pick out the ELO ratings if available */

  do {
    (void)fscanf(fp,"%c",&ch);
    if (ch!='1' && ch!='\n' && !feof(fp)) {
      fgets(temp,1024,fp);
      if (!temp) continue;
      for (tempch = temp ; *tempch ; tempch++) *tempch = toupper(*tempch);
      location=strstr(temp,"RESULT");
      if (location!=NULL) {
        location=strchr(temp,'"');
        if (location[1]=='0') *winner = BLACK;
        else if (location[1]=='1') {
          if (location[2]=='-') *winner = WHITE;
        }
      }
      location=strstr(temp,"WHITEELO");
      if (location!=NULL) {
        location=strchr(temp,'"');
        welo = (int)strtol((location+1),NULL,10);
      }
      location=strstr(temp,"BLACKELO");
      if (location!=NULL) {
        location=strchr(temp,'"');
        belo = (int)strtol((location+1),NULL,10);
      }
      location=strstr(temp,"WHITE ");
      if (location!=NULL) {
        location=strchr(temp,'"');
        if (WhitePlayer) {
          strcpy(WhitePlayer,(location+1));
          location = strchr(WhitePlayer,'\"');
          if (location) *location = 0;
        }
      }
      location=strstr(temp,"BLACK ");
      if (location!=NULL) {
        location=strchr(temp,'"');
        if (BlackPlayer) {
          strcpy(BlackPlayer,(location+1));
          location = strchr(BlackPlayer,'\"');
          if (location) *location = 0;
        }
      }
    }
    if (ch=='1') (void)fscanf(fp,"%c",&chh);
  } while ((ch != '1' || chh != '.') && !feof(fp));

  if (*winner == WHITE) *elo = welo;
  if (*winner == BLACK) *elo = belo;
  if (*winner == NEUTRAL) *elo = min(welo,belo);

  if (feof(fp)) return NULL;
  (void)fscanf(fp," ");

  ResetBoard(&B);
 
  ch = 0;
  do {
    if (ch == 0) {memset(temp,0,sizeof(temp)); (void)fgets(temp,1024,fp); cno=0;}
    ch = temp[cno++];
     /* Closing bracket outside a comment - not a good thing! */
    if (ch=='}' || ch==')') {
      fprintf(stdout,"Bad Bracket Formation in PGN!\n");
      return NULL;
    }
     /* We have a (nested) comment - read in and ignore it */
    if (ch=='{' || ch=='(') {
      bracket_count=1;
      do {
        if (ch == 0) {memset(temp,0,sizeof(temp)); (void)fgets(temp,1024,fp); cno=0;}
        ch = temp[cno++];
        if (ch=='{' || ch=='(') bracket_count++;
        if (ch=='}' || ch==')') bracket_count--;
      } while (bracket_count>0);
      continue;
    }
     /* End of the game? */
    if (ch=='#' || ch=='[') breakout=1;
     /* We're not in a comment, so try to read in a move */
    else {
       /* This isn't white space */
      if (ch!=' ' && ch!='\n') {
        movech[0]=ch;
        n=1;
        do {
          ch = temp[cno++];
          if (isalnum(ch) || ch=='-' || ch=='=') movech[n++]=ch;
        } while (isalnum(ch) || ch=='-' || ch=='=' || ch=='?' || ch=='!');
        movech[n] = 0;
        
        if (ch!='.') {
          LoadPGN =  TRUE;
          move = ParseMove(movech,B,FALSE);
          LoadPGN = FALSE;
          if (move != NO_MOVE) {
            (void)DoMove(&B, move);
            templist[(*nmoves)++] = move;
          }
          else if ((int)movech[0] > 60) {
            PrintBoard(B);
            PrintMove(move,0,stderr);
            fprintf(stderr,"\nIllegal Move {No. %d}\"%s\"\n",B.side,movech);
            return 0;
          }
        }
      }
    }
  } while (breakout==0 && !feof(fp));

  if (*nmoves==0) return NULL;
  movelist = (MOVE *)malloc(sizeof(MOVE)*(*nmoves));
  assert(movelist != NULL);
  for (n=0;n<(*nmoves);n++) movelist[n] = templist[n];
  return movelist;
}


/* Convert a player's ELO into a bonus weighting for the opening book. */
int ELOBonus(int elo) {
  if (elo<2400) return 1;
  return ((elo-2200)/100);
}

/* Save the opening book in its current state */
void SaveOpeningBook(void) {
  char Outfile[FILENAME_MAX];
  Openpos *O;
  int n,m;
  FILE *fp;
   
   /* Get directory listing */
  fprintf(stdout,"Directory of .dat Files\n\n");
  (void)system(DIR_COMMAND"*.dat");
  fprintf(stdout,"\nOutput Book File Name ('.' to cancel) : ");
  (void)fgets(Outfile,FILENAME_MAX,stdin);
  Outfile[strlen(Outfile)-1]=0;
  if (Outfile[0]=='.') return;
   /* Try to open chosen file */

  if ((fp = fopen(Outfile,"w"))==NULL) {
    fprintf(stdout,"Can't Open Output File %s!\n",Outfile);
    return;
  }
   
   /* Write out the data */
  for (n=0;n<NPos;n++) {
    O = (Openings + n);
    fprintf(fp,"%s\n",O->FEN);
    for (m=0;m<O->nmoves;m++) {
      if (m>0) fprintf(fp," ");
      if (IsCastle(O->m[m])) 
	fprintf(fp,"%c%d%c%d",File(MFrom(O->m[m])) + 97,
                              8 - Rank(MFrom(O->m[m])),
                              File(MTo(O->m[m])) + 97,
                              8 - Rank(MTo(O->m[m])));
      else PrintMove(O->m[m],FALSE,fp);
      fprintf(fp,"{%d}",O->sc[m]);
    }
    fprintf(fp,"\n");
  }
  fprintf(fp,"#END#");
   
  fclose(fp);
   
  fprintf(stdout,"Book Saved OK!\n");
}

/* Annotate a pgn file. */
void AnnotateFile(void) {
  char PGNFile[FILENAME_MAX],temp[10]="",FEN[100], SideC,FileTemp[FILENAME_MAX];
  char PlayerName[FILENAME_MAX],WhitePlayer[FILENAME_MAX], BlackPlayer[FILENAME_MAX];
  char *ch, *Current, MoveTxt[10];
#ifdef _WIN32
  char DateStr[16]="";
#endif
  Board Backup, Backup2;
  MOVE *moves,move,Bestmove, OldBest;
  int maxtime,nmoves,elo,n,winner,op, firstmove, lastmove;
  int NumGames=0, val=0, i, tweight=0, oldscore, margin, linelength=0;
  Openpos *O;
  FILE *fp,*logfile, *outfile;
  BOOL BookMove;
  
  /* Get directory listing */
  fprintf(stdout,"Directory of PGN Files\n\n");
  (void)system(DIR_COMMAND"*.pgn");
  fprintf(stdout,"\nPlease Choose : ");

  /* Get chosen filename */
  (void)fgets(PGNFile,50,stdin);
  PGNFile[strlen(PGNFile)-1]=0;
  if (!strstr(PGNFile,".pgn")) strcat(PGNFile,".pgn");
  
  /* Try to open chosen input file */
  if ((fp = fopen(PGNFile,"r"))==NULL) {
    fprintf(stdout,"Can't Open File %s!\n",PGNFile);
    return;
  }
  fprintf(stdout,"%s Loaded OK!\n",PGNFile);
  
  do {
    fprintf(stdout,"Maximum Thinking Time Per Move (seconds) : ");
    (void)fgets(temp,sizeof(temp),stdin);
    maxtime = (int)strtol(temp,NULL,10);
  } while (maxtime<1);

  do {
    fprintf(stdout,"Annotate For Which Side [(B)lack (W)hite (A)ll (N)ame] : ");
    (void)fgets(temp,sizeof(temp),stdin);
    SideC = toupper(temp[0]);
  } while (SideC!= 'B' && SideC != 'W' && SideC != 'A' && SideC != 'N');

   /* Input the name of the player for whom to annotate */
  if (SideC == 'N') {
    fprintf(stdout,"Name of Player to Annotate : ");
    (void)fgets(PlayerName,sizeof(PlayerName),stdin);
    n=0;
    do {
      PlayerName[n] = toupper(PlayerName[n]);
      if (PlayerName[n] == '\n') PlayerName[n] = 0;
    } while (PlayerName[n++]);
  }
  
  /* Get the move range */
  do {
    fprintf(stdout,"Annotate Which Moves? (e.g. '5' (start at 5) or '6-20') : ");
    (void)fgets(temp,sizeof(temp),stdin);
    firstmove = (int)strtol(temp,NULL,10);
    ch = strchr(temp,'-');
    if (ch) {
      lastmove = (int)strtol((ch+1),NULL,10);
    }
    else lastmove = 10000;
  } while (firstmove<1 || lastmove < firstmove);

  /* Get the score margin for annotation */
  do {
    fprintf(stdout,"Minimum Score Margin for Annotation (centipawns) : ");
    (void)fgets(temp,sizeof(temp),stdin);
    margin = (int)strtol(temp,NULL,10);
  } while (margin<0 || margin>1000);

   /* Set up the output logfile */
  fprintf(stdout,"Output Log File ('.' for none) : ");
  (void)fgets(FileTemp,50,stdin);
  FileTemp[strlen(FileTemp)-1]=0;
  if (FileTemp[0]=='.') logfile=NULL;
   /* Try to open chosen file */
  else {
    if ((logfile = fopen(FileTemp,"a"))==NULL) {
      fprintf(stdout,"Can't Open Log File %s!\n",FileTemp);
      logfile=NULL;
    }
    if (logfile) {
      fprintf(stdout,"%s Opened OK!\n",FileTemp);
      fprintf(logfile,"Annotation by Beowulf V"VER"\n");
      fprintf(logfile,"Time per Position = %d Second(s)\n",maxtime);
    }
    else {
      fprintf(stdout,"Can't Open Log File %s!\n",FileTemp);
    }
  }

   /* Set up the output PGN file */
  fprintf(stdout,"Output PGN File ('.' for none) : ");
  (void)fgets(FileTemp,50,stdin);
  FileTemp[strlen(FileTemp)-1]=0;
  if (FileTemp[0]=='.') outfile=NULL;
   /* Try to open chosen file */
  else {
    if ((outfile = fopen(FileTemp,"a"))==NULL) {
      fprintf(stdout,"Can't Open Output File %s!\n",FileTemp);
      outfile=NULL;
    }
    if (outfile) {
      fprintf(stdout,"%s Opened OK!\n",FileTemp);
    }
    else {
      fprintf(stdout,"Can't Open Output File %s!\n",FileTemp);
    }
  }

   /* Setup the computer parameters */
  Params.MoveTime = (float)maxtime;
  Params.Depth = 2;
  Params.Test = FALSE;

  /* Backup the current board */
  Backup = Current_Board;

  /* Load in the games, one at a time, and study them */
  do {
    /* Load in the next game */
    moves = ImportPGNGame(fp,&winner,&nmoves,&elo,WhitePlayer,BlackPlayer);

    if (moves == NULL || nmoves==0) break;
    NumGames++;
     /* Print game details */
    fprintf(stdout,"\nLoaded Game Number %d  :  %d Moves, Winner = ",NumGames,nmoves);
    switch(winner) {
      case (WHITE) : fprintf(stdout,"White  "); break;
      case (BLACK) : fprintf(stdout,"Black  "); break;
      default      : fprintf(stdout,"Drawn  "); break;
    }
    fprintf(stdout,"\n");
    if (logfile) fprintf(logfile,"\nLoaded Game Number %d  :  %d Moves\n",NumGames,nmoves);
    if (outfile) {
      fprintf(outfile, "[Event \"Annotated Game\"]\n");
#ifdef _WIN32
      _strdate(DateStr);
      fprintf(outfile, "[Date \"%s\"]\n",DateStr);
#endif
      fprintf(outfile, "[Number \"%d\"]\n",NumGames);
      fprintf(outfile, "[White \"%s\"]\n",WhitePlayer);
      fprintf(outfile, "[Black \"%s\"]\n",BlackPlayer);
      if (winner == WHITE) fprintf(outfile, "[Result \"1-0\"]\n");
      if (winner == BLACK) fprintf(outfile, "[Result \"0-1\"]\n");
      if (winner == NEUTRAL) fprintf(outfile, "[Result \"1/2-1/2\"]\n");
      fprintf(outfile,"[Annotator \"Beowulf V"VER"\"]\n");
      fprintf(outfile,"{Annotating for ");
      if (SideC == 'W') fprintf(outfile,"White Only}\n");
      if (SideC == 'B') fprintf(outfile,"Black Only}\n");
      if (SideC == 'A') fprintf(outfile,"Black and White}\n");
      if (SideC == 'N') fprintf(outfile,"Player \"%s\"}\n",PlayerName);
      fprintf(outfile,"{Time per Position = %d Second(s)}\n\n",maxtime);
      linelength=0;
    }

    /* Step through the moves */
    ResetBoard(&Current_Board);
    for (n=0;n<nmoves;n++) {
      move = moves[n];
      assert(move != NO_MOVE);

       /* Print out the move played in the game */
      fprintf(stdout,"\n[%d%c] Move Played: ",(n/2)+1,(n&1)?'B':'W');
      (void)MoveToText(MoveTxt, move, &Current_Board);
      fprintf(stdout,"%s\n",MoveTxt);
      if (logfile) {
        fprintf(logfile,"\n[%d%c] Move Played: ",(n/2)+1,(n&1)?'B':'W');
        fprintf(logfile,"%s\n",MoveTxt);
      }
      if (outfile) {
        if ((n&1) == 0) {
          fprintf(outfile,"%d. ",(n/2)+1);
          linelength += 3;
          if (n>19) linelength++;
          if (n>199) linelength++;
        }
        (void)MoveToText(MoveTxt, move, &Current_Board);
        fprintf(outfile, "%s ", MoveTxt);
        linelength += 1 + (int)strlen(MoveTxt);
        if (linelength > 50) {fprintf(outfile,"\n"); linelength = 0;}
      }

      /* Skip moves for the side we're not studying, if any */
      if (Current_Board.side==WHITE) Current = WhitePlayer;
      else Current = BlackPlayer;

      if ((SideC=='B' && Current_Board.side==WHITE) ||
          (SideC=='W' && Current_Board.side==BLACK) ||
          (SideC=='N' && !strstr(Current,PlayerName)) ||
          (n<(firstmove*2)-2 || n>(lastmove*2)-1)) {
        fprintf(stdout,"Skipping Move by ");
        if (Current_Board.side == BLACK) fprintf(stdout,"Black\n");
        if (Current_Board.side == WHITE) fprintf(stdout,"White\n");
        if (logfile) {
          fprintf(logfile,"Skipping Move by ");
          if (Current_Board.side == BLACK) fprintf(logfile,"Black\n");
          if (Current_Board.side == WHITE) fprintf(logfile,"White\n");
        }
        (void)DoMove(&Current_Board, move);
        continue;
      }
      
      /* Get the FEN for the current board */
      BoardToFEN(&Current_Board,FEN);
      Bestmove=NO_MOVE;
      
      /* Check for a book move */
      for (op=0;op<NPos;op++) {
        O = (Openings+op);        
        /* See if this book entry matches */
        if (!strcmp(FEN,O->FEN)) {
          tweight=0;
          Bestmove=-2;
          for (i=0;i<O->nmoves;i++) tweight += O->sc[i];
          for (i=0;i<O->nmoves;i++) {
            if (O->m[i] == move) {val = O->sc[i]*100/tweight;Bestmove = O->m[i];}
          }
        }
        if (Bestmove!=NO_MOVE) break;
      }

      /* Analyse this position */
      if (Bestmove==NO_MOVE || Bestmove==-2) {
        Bestmove = Comp();
        BookMove=FALSE;
      }
      else {
        fprintf(stdout,"Move Found in Opening Book.  Weighting %d%%\n",val);
        if (logfile) fprintf(logfile,"Move Found in Opening Book.  Weighting %d%%\n",val);
        BookMove=TRUE;
        PreviousScore = 0;
      }

      /* Print to logfile */
      if (logfile && !BookMove) {
        fprintf(logfile,"%s acd %d; acn %.0f; acs %.2f; ",FEN,GlobalDepth,(double)Nodecount,(float)TimeTaken/100.0);
        fprintf(logfile,"ce %d; pv %s\n",PreviousScore,PV_text);
        if (move != Bestmove) fprintf(logfile,"!!  Beowulf Disagrees with the Move Played\n");
        else fprintf(logfile,"**  Beowulf Agrees with the Move Played\n");
      }

      if (move != Bestmove) {
        fprintf(stdout,"\n!!  Beowulf Disagrees! Analysing the Move Played in the Game\n\n");
        if (logfile) fprintf(logfile,"Analysing the Move Played in the Game\n");
        oldscore = PreviousScore;

        /* Go to the next move */
        Backup2 = Current_Board;
        OldBest = Bestmove;
        (void)DoMove(&Current_Board,move);
        /* Analyse this position */
        Bestmove = Comp();

        /* Print out the results */
        if (logfile) {
          fprintf(logfile,"acd %d; acn %.0f; acs %.2f; ",GlobalDepth,(double)Nodecount,(float)TimeTaken/100.0);
          PreviousScore = -PreviousScore;
          fprintf(logfile,"ce %d\n",PreviousScore);
          if (oldscore>PreviousScore) fprintf(logfile,"Estimated Loss : %d centipawn(s)\n",oldscore-PreviousScore);
          else if (oldscore<PreviousScore) fprintf(logfile,"Beowulf Changes its Mind - The Game Move is Better\n");
          else fprintf(logfile,"Beowulf Reckons the Game Move is Equivalent\n");
        }
        if (outfile && oldscore>PreviousScore+margin) {
          fprintf(outfile, "{");
          (void)MoveToText(MoveTxt, move, &Backup2);
          fprintf(outfile, "%s %.2f, ",MoveTxt, (float)PreviousScore/100.0);
          linelength += 8 + (int)strlen(MoveTxt);
          if (linelength > 50) {fprintf(outfile,"\n"); linelength = 0;}
          (void)MoveToText(MoveTxt, OldBest, &Backup2);
          fprintf(outfile, "%s %.2f} ",MoveTxt, (float)oldscore/100.0);
          linelength += 7 + (int)strlen(MoveTxt);
          if (linelength > 50) {fprintf(outfile,"\n"); linelength = 0;}
        }
      }
      else {
        fprintf(stdout,"\n**  Beowulf Agrees with the Move Played in the Game\n\n");
        /* Go to the next move */
        (void)DoMove(&Current_Board,move);
      }
    }
    free(moves);
    if (logfile) fprintf(logfile,"\n----------------\nEnd Of Game\n----------------\n\n");
    if (outfile) {
      if (winner == WHITE) fprintf(outfile, " 1-0\n\n");
      if (winner == BLACK) fprintf(outfile, " 0-1\n\n");
      if (winner == NEUTRAL) fprintf(outfile, " 1/2-1/2\n\n");
    }
  } while (!feof(fp));
  
  /* Close the file handles */
  fclose(fp);
  if (logfile) fclose(logfile);
  if (outfile) fclose(outfile);
  Current_Board = Backup;
  
  /* Print out the results */
  fprintf(stdout,"\nFile %s Studied\n",PGNFile);
  fprintf(stdout,"Total Games Annotated : %d\n",NumGames);
}
