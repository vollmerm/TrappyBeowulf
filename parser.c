/**********************************
 *    parser.c                    *
 *    Colin Frayn                 *
 *    March 2001                  *
 **********************************/

/*
  This file contains all the functions for parsing the input
  string from the player.
*/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <signal.h>
#ifdef _WIN32
# define strncasecmp strnicmp
#endif

#include "common.h"
#include "parser.h"
#include "main.h"
#include "moves.h"
#include "board.h"
#include "utils.h"
#include "checks.h"
#include "eval.h"
#include "computil.h"

extern int mvno,automoves, GlobalAlpha, GlobalBeta;
extern Board Current_Board;
extern CompDat Params;
char words[10][FILENAME_MAX];
Undo UndoHistory[1000];
extern BOOL XBoard,Post,Computer[2],ComputerGO,Ponder, LoadPGN, Force, UCI;
BOOL BookON=TRUE,AnalyseMode=FALSE, TBON = TRUE;
MOVE MoveHistory[1000];
extern BITBOARD FileMask[8],InvMask[64];
extern HashElt *TableB,*TableW;
extern long int NHash;

#ifdef BEOSERVER
int PeerNodes;
#endif // BEOSERVER

/* This is the main control function which breaks down the input
 * into its constituent parts and sends it off to specialised functions
 * in order to interpret correctly what is required. */
int ParseInput(const char *inp) {
  int nwrd,n,side = Current_Board.side;
  long int l;
  MOVE tempmove;
  char first[FILENAME_MAX],temp[FILENAME_MAX];

   /* Count words in input string */
  nwrd = CountWords(inp);
   
   /* Check for hitting 'return' with no input */
  if (nwrd==0) {PrintBoard(Current_Board);return S_NOP;}
   /* Check for excessive length input */
  if (nwrd>10) {Error(0);return S_NOP;}

   /* Break input string up into seperate words */
  GetWords(inp,nwrd);
   
   /* Get first word and convert to upper case */
  strcpy(first,words[0]);
  for (n=0;n<(int)strlen(first);n++) first[n]=toupper(first[n]);
       
                        /* --== Check for legal commands ==-- */
  
   /* -- Single commands -- */

  if (nwrd==1) {
     /* Quit the game */
    if (!strcmp(first,"QUIT") || !strcmp(first,"RESIGN") || !strcmp(first,"Q")) return S_QUIT;
     /* Display the board */
    if (!strcmp(first,"BOARD") || !strcmp(first,"B")) {PrintBoard(Current_Board);return S_NOP;}
     /* Display the FEN code for the Current Board */
    if (!strcmp(first,"DFEN") || !strcmp(first,"FEN")) {
      BoardToFEN(&Current_Board,temp);
      fprintf(stdout,"FEN is : %s\n",temp);
      return S_NOP;
    }
     /* Reset the Board */
    if (!strcmp(first,"NEW") || !strcmp(first,"RESET")) {ResetBoard(&Current_Board);
       if (!XBoard) PrintBoard(Current_Board);
       else {Computer[BLACK]=TRUE;Computer[WHITE]=FALSE;ComputerGO=TRUE;}
       Force = FALSE;
        /* Free old memory */
       ResetHash();
       return S_NOP;}
     /* Set the computer thinking */
    if (!strcmp(first,"COMP")) return S_SEARCH;
     /* Display the Command Help List */
    if (!strcmp(first,"HELP")) {PrintHelp();return S_NOP;}
     /* Toggle the Opening Book ON/OFF */
    if (!strcmp(first,"BOOK") || !strcmp(first,"BK")) {
      if (BookON) {BookON=FALSE;fprintf(stdout,"Opening Book OFF!\n");}
      else        {BookON=TRUE; fprintf(stdout,"Opening Book ON!\n");}
      return S_NOP;
    }
     /* Turn tablebases ON/OFF */
    if (!strcmp(first,"TABLEBASES") || !strcmp(first,"TB")) {
      if (TBON) {TBON=FALSE;fprintf(stdout,"Endgame Tablebases OFF!\n");}
      else      {TBON=TRUE; fprintf(stdout,"Endgame Tablebases ON!\n");}
      return S_NOP;
    }
     /* Study a PGN file for improving the opening book */
    if (!strcmp(first,"STUDY")) {StudyFile();return S_NOP;}
     /* Annotate a PGN file */
    if (!strcmp(first,"ANNOTATE")) {AnnotateFile();return S_NOP;}
     /* Study a file for improving the opening book */
    if (!strcmp(first,"SAVEBOOK")) {SaveOpeningBook();return S_NOP;}
     /* Run an EPD test suite */
    if (!strcmp(first,"TEST")) {RunTestSuite("",0);return S_NOP;}
     /* Set the computer off! */
    if (!strcmp(first,"GO")) {ComputerGO = TRUE;Computer[side]=TRUE;Computer[Opponent(side)]=FALSE;Force = FALSE;return S_NOP;}
     /* Set Xboard mode */
    if (!strcmp(first,"XBOARD")) {XBoard=TRUE;Params.MoveTime=0.0;Params.Depth=2;ComputerGO=TRUE;
#ifdef SIG_IGN
                                  (void)signal(SIGINT, SIG_IGN);
#endif
                                  Computer[BLACK]=TRUE;Post=FALSE; fprintf(stdout,"\n");return S_NOP;}
     /* Set UCI mode */
    if (!strcmp(first,"UCI")) {
      XBoard=TRUE;
      UCI=TRUE;
      Params.MoveTime=0.0;
      Params.Depth=2;
      ComputerGO=FALSE;
#ifdef SIG_IGN
                                  (void)signal(SIGINT, SIG_IGN);
#endif
      Computer[BLACK]=FALSE;
      Computer[WHITE]=FALSE;
      Force=TRUE;
      Post=FALSE;
      fprintf(stdout,"id name Beowulf v"VER"\n");
      fprintf(stdout,"id author Colin Frayn\n");
//      fprintf(stdout,"option name NalimovPath type string name c:\\\n");
//      fprintf(stdout,"option name NalimovCache type spin default 1 min 1 max 32\n");
      fprintf(stdout,"uciok\n");
      return S_NOP;
    }
     /* UCI synchronising */
    if (UCI && !strcmp(first,"ISREADY")) {fprintf(stdout,"readyok\n");return S_NOP;}
     /* Castling */
    if (side==WHITE && !strcmp(first,"E1G1") && Current_Board.WhiteKing==e1) return Check_Castle(&Current_Board,OO);
    if (side==WHITE && !strcmp(first,"E1C1") && Current_Board.WhiteKing==e1) return Check_Castle(&Current_Board,OOO);
    if (side==BLACK && !strcmp(first,"E8G8") && Current_Board.BlackKing==e8) return Check_Castle(&Current_Board,OO);
    if (side==BLACK && !strcmp(first,"E8C8") && Current_Board.BlackKing==e8) return Check_Castle(&Current_Board,OOO);
    if (!strcmp(first,"0-0")) return Check_Castle(&Current_Board,OO);
    if (!strcmp(first,"0-0-0")) return Check_Castle(&Current_Board,OOO);
    if (!strcmp(first,"O-O")) return Check_Castle(&Current_Board,OO);
    if (!strcmp(first,"O-O-O")) return Check_Castle(&Current_Board,OOO);
     /* Evaluate the current position */
    if (!strcmp(first,"EVAL") || !strcmp(first,"SCORE") || !strcmp(first,"ANALY")) {Analyse(Current_Board);return S_NOP;}
     /* Undo the last move */
    if (!strcmp(first,"UNDO") || !strcmp(first,"RETAKE") || !strcmp(first,"BACK") || !strcmp(first,"U")) {
      if (mvno<1) {Error(3); return S_NOP;}
      mvno--;
      UndoMove(&Current_Board,MoveHistory[mvno],UndoHistory[mvno]);
      PrintBoard(Current_Board);
      return S_NOP;
    }
     /* Redo the last move */
    if (!strcmp(first,"REDO") || !strcmp(first,"FORWARD") || !strcmp(first,"R")) {
      if (mvno==999 || MoveHistory[mvno]==NO_MOVE) {Error(4); return S_NOP;}
      UndoHistory[mvno] = DoMove(&Current_Board,MoveHistory[mvno]);
      PrintBoard(Current_Board);
      mvno++;
      return S_NOP;
    }
     /* Post thinking to XBoard */
    if (!strcmp(first,"POST")) {Post=TRUE;return S_NOP;}
     /* Do not Post thinking to XBoard */
    if (!strcmp(first,"NOPOST")) {Post=FALSE;return S_NOP;}
     /* Set computer to play white (now obsolete) */
    if (!strcmp(first,"WHITE")) {Computer[WHITE]=FALSE;Computer[BLACK]=TRUE;ComputerGO=FALSE;Current_Board.side=WHITE;return S_NOP;}
     /* Set computer to play black (now obsolete) */
    if (!strcmp(first,"BLACK")) {Computer[WHITE]=TRUE;Computer[BLACK]=FALSE;ComputerGO=FALSE;Current_Board.side=BLACK;return S_NOP;}
     /* Set computer to cease playing */
    if (!strcmp(first,"FORCE") || !strcmp(first,"STOP") || !strcmp(first,"COFF")) {Computer[WHITE]=FALSE;Computer[BLACK]=FALSE;Force = TRUE;return S_NOP;}
     /* Get a hint for a move to play */
    if (!strcmp(first,"HINT")) {GetHint();return S_NOP;}
     /* Enter Analyse Mode */
    if (!strcmp(first,"ANALYZE")) {AnalyseMode = TRUE;fprintf(stdout,"Analyse mode: 'exit' to stop\n");return S_SEARCH;}
     /* Ponder Mode */
    if (!strcmp(first,"EASY")) {Ponder=FALSE;return S_NOP;}
    if (!strcmp(first,"HARD")) {Ponder=TRUE;return S_NOP;}
    if (!strcmp(first,"PONDER")) {Ponder=Not(Ponder); 
                                  if (Ponder) fprintf(stdout,"Pondering Enabled\n");
                                  else        fprintf(stdout,"Pondering Disabled\n");
                                  return S_NOP;}
     /* Display current skill levels */
    if (!strcmp(first,"SKILL")) {fprintf(stdout,"Skill Levels : White %d  Black %d\n",Params.WSkill,Params.BSkill);return S_NOP;}
     /* Enter EDIT mode */
    if (!strcmp(first,"EDIT")) {EditMode(); return S_NOP;}
     /* Leave Analyse mode */
    if (!strcmp(first,"EXIT")) {AnalyseMode = FALSE; return S_NOP;}
     /* Ignore extra NODE results */
    if (!strcmp(first,"NODE")) {fprintf(stdout,"Returned work unit ignored\n");return S_NOP;}
  }
     
   /* -- One parameter commands -- */

   /* Run a movegen test */
  if (!strcmp(first,"MOVEGEN") || !strcmp(first,"PERFT")) {
    float TimeResult;
    if (nwrd!=2) {fprintf(stdout,"Usage : %s <ply>\n",first);return S_NOP;}
    fprintf(stdout,"Progress : ");
    SetStartTime();
    l = CountMoves(&Current_Board,(int)strtol(words[1],NULL,10),0);
    TimeResult = (float)GetElapsedTime() / 100.0f;
    fprintf(stdout,"\nTotal: %ld Moves\n%.2f Seconds\n\n",l,TimeResult);
    return S_NOP;
  }
   /* Set the default search depth */
  if (!strcmp(first,"DEPTH") || !strcmp(first,"SD")) {
    if (nwrd!=2) {fprintf(stdout,"Usage : %s <ply>\n",first);return S_NOP;}
    l = strtol(words[1],NULL,10);
    if (l<2) {fprintf(stdout,"Depth Must be At Least 2 ply!\n");return S_NOP;}
    fprintf(stdout,"Default Search Depth set to %ld Ply\n",l);
    Params.Depth=l;
    return S_NOP;    
  }
   /* Set the default maximum search time */
  if (!strcmp(first,"ST") || !strcmp(first,"LIMIT")) {
    if (nwrd!=2) {fprintf(stdout,"Usage : %s <time/seconds>\n",first);return S_NOP;}
    l = strtol(words[1],NULL,10);
    Params.MoveTime=(float)l;
    fprintf(stdout,"Default Search Time set to %ld seconds\n",l);
    return S_NOP;    
  }
   /* Set the clock for this side */
  if (!strcmp(first,"TIME")) {
    if (nwrd!=2) {fprintf(stdout,"Usage : TIME <time/centisecs>\n");return S_NOP;}
    l = strtol(words[1],NULL,10);
    Params.Time=l;
     /* Also change the default move time.  If we're in xboard mode then this will
      * get reset anyway, and if not then we can use the 'time' command instead of
      * the 'st' command to do exactly the same thing! */
    Params.MoveTime = (float)l;
    return S_NOP;    
  }
   /* Set the clock for opponent */
  if (!strcmp(first,"OTIM")) {
    if (nwrd!=2) {fprintf(stdout,"Usage : OTIM <time/centisecs>\n");return S_NOP;}
    l = strtol(words[1],NULL,10);
    Params.OTime=l;
    return S_NOP;    
  } 
   /* Set the hash table size */
  if (!strcmp(first,"HASH")) {
    if (nwrd!=2) {fprintf(stdout,"Usage : HASH <0-20>\n");return S_NOP;}
    l = strtol(words[1],NULL,10);
    if (l<0 || l>20) {fprintf(stdout,"Size must be between 0-20!\n");return S_NOP;}
    Params.HashSize = l;
    l = (1<<l);
    fprintf(stdout,"Default Hash Size set to %ld kb\n",l);
     /* Free old memory */
    ResetHash();
    return S_NOP;    
  }
   /* Set the computer skill level */
  if (!strcmp(first,"SKILL")) {
    if (nwrd!=2) {fprintf(stdout,"Usage : %s <skill>\n",first);return S_NOP;}
    l = strtol(words[1],NULL,10);
    if (l<1 || l>10) {fprintf(stdout,"Skill Must Be Between 1-10!\n");return S_NOP;}
    if (side==WHITE) {fprintf(stdout,"Skill Level for White Set to %ld\n",l);Params.WSkill=l;}
    else             {fprintf(stdout,"Skill Level for Black Set to %ld\n",l);Params.BSkill=l;}
    return S_NOP;    
  }
   /* Computer Auto Moves */
  if (!strcmp(first,"AUTO")) {
    if (nwrd!=2) {fprintf(stdout,"Usage : %s <moves>\n",first);return S_NOP;}
    l = strtol(words[1],NULL,10);
    if (l<1) {fprintf(stdout,"Number of Moves Must Be > 0!\n");return S_NOP;}
    fprintf(stdout,"Doing Automoves for %ld ply\n",l);
    automoves=l;
    return S_AUTO;
  }
   /* Politely Reply to Name setup */
  if (!strcmp(first,"NAME")) {fprintf(stdout,"tellics say Hello %s, I'm Beowulf v"VER"\n",words[1]);return S_NOP;}
   /* Set the gloabl alpha parameter */
  if (!strcmp(first,"ALPHA")) {
    if (nwrd!=2) {fprintf(stdout,"Usage : %s <value>\n",first);return S_NOP;}
    l = strtol(words[1],NULL,10);
    fprintf(stdout,"Global Alpha set to %ld\n",l);
    GlobalAlpha = (int)l;
    return S_NOP;    
  }
   /* Set the global beta parameter */
  if (!strcmp(first,"BETA")) {
    if (nwrd!=2) {fprintf(stdout,"Usage : %s <value>\n",first);return S_NOP;}
    l = strtol(words[1],NULL,10);
    fprintf(stdout,"Global Beta set to %ld\n",l);
    GlobalBeta = (int)l;
    return S_NOP;    
  }

#ifdef BEOSERVER
   /* Set the number of nodes in a parallel search */
  if (!strcmp(first,"PEERNODES")) {
    if (nwrd!=2) {fprintf(stdout,"Usage : %s <value>\n",first);return S_NOP;}
    l = strtol(words[1],NULL,10);
    fprintf(stdout,"PeerNode Count set to %ld\n",l);
    PeerNodes = (int)l;
    return S_NOP;    
  }
#endif // BEOSERVER
  
   /* -- Multiple parameter commands -- */
   
   /* Set the board to a certain FEN position */
  if (!strcmp(first,"SETBOARD") || !strcmp(first,"SET")) {
    if (nwrd<2) {fprintf(stdout,"Usage : SETBOARD <FEN>\n");return S_NOP;}
    strcpy(temp,words[1]); strcat(temp," ");
    strcat(temp,words[2]); strcat(temp," ");
    strcat(temp,words[3]); strcat(temp," ");
    strcat(temp,words[4]);
    SetBoard(&Current_Board,temp,TRUE);
    return S_NOP;
  }
   /* Goto a certain move */
  if (!strcmp(first,"GOTO")) {
    if (nwrd!=3) {fprintf(stdout,"Usage : GOTO <mvno> <W/B>\n");return S_NOP;}
    GotoMove(&Current_Board,words[1],words[2]);
    return S_NOP;
  }
   /* Display the movelist */
  if (!strcmp(first,"MOVES") || !strcmp(first,"MOVELIST")) {
    if (nwrd!=3) {fprintf(stdout,"Usage : %s <from> <to>\n",first);return S_NOP;}
    MoveList(words[1],words[2]);return S_NOP;
  }
   /* Accept an XBoard style result comand */
  if (!strcmp(first,"RESULT")) {Computer[0] = Computer[1] = FALSE; ComputerGO = FALSE; return S_NOP;}
   
   /* Set the XBoard time control */
  if (!strcmp(first,"LEVEL")) {
    if (nwrd!=4) {fprintf(stdout,"Usage : Level <Moves> <Time> <Inc>\n");return S_NOP;}
    SetTimeControl(words[1], words[2], words[3]);
    return S_NOP;
  }

   /* More Unsupported XBoard Commands */
  if (!strcmp(first,"DRAW") || !strcmp(first,"COMPUTER")) return S_NOP;
  if (!strcmp(first,"RANDOM") || !strcmp(first,"RATING")) return S_NOP;
   
   /* UCI Options */
  if (UCI && !strcmp(first,"SETOPTION")) {
    if (!strcmp(words[1],"name")) {
    }
  }
  /* Set a position in UCI */
  if (UCI && !strcmp(first,"POSITION")) {
    if (!strcmp(words[1],"startpos")) {
      ResetBoard(&Current_Board);
    }
    else {
      strcpy(temp,words[1]); strcat(temp," ");
      strcat(temp,words[2]); strcat(temp," ");
      strcat(temp,words[3]); strcat(temp," ");
      strcat(temp,words[4]);
      SetBoard(&Current_Board,temp,TRUE);
    }

  }

   /* -- Standard Move -- */
  if ((tempmove=ParseMove(words[0],Current_Board,TRUE)) != NO_MOVE) {
     /* Do the move */  
    UndoHistory[mvno] = DoMove(&Current_Board,tempmove);
    MoveHistory[mvno] = tempmove;
    mvno++;
     /* Reset new movelist entry */
    MoveHistory[mvno] = NO_MOVE;      
    return S_CHKDRW;
  }

   /* Default */
  return S_NOP;
}

 /* Counts the words in a given string */
int CountWords(const char *inp) {
  int n=0,nwrd=0,l=0;
   
  for (n=0;n<(int)strlen(inp);n++) {
    if (inp[n]!=' ' && inp[n]!='\n' && l==0) {nwrd++;l++;}
    if (inp[n]==' ') l=0;
  }
  return nwrd;
}

 /* Breaks a string down into constituent words */
void GetWords(const char *inp, const int nwrd) {
  int n=-1,l=0,wno=0;

  for (wno=0;wno<nwrd;wno++) {
    memset(words[wno],0,sizeof(words[wno]));
    while (inp[++n]==' ');
    while (inp[n]!=' ' && inp[n]!='\n') {
      words[wno][l]=inp[n];
      l++;
      n++;
    }
    l=0;
  }
}

/* Display an Error message */
void Error(const int e) {
  if (e==0) fprintf(stdout,"Error - Illegal Input!\n");
  if (e==1) fprintf(stdout,"Error - Unrecognised Command!\n");
  if (e==2) fprintf(stdout,"Error - Illegal Move!\n");
  if (e==3) fprintf(stdout,"Error - Already on First Move!\n");
  if (e==4) fprintf(stdout,"Error - Already on Last Move!\n");
}

/* Parse a single word input and check to see if it is a legal move.  If it
 * is then return the move in from-to format. Warning - this is extremely
 * fiddly to achieve correctly, and I've probably got it wrong. */
MOVE ParseMove(char *move_to_parse, Board B, BOOL giveErrors) {
  int n,bestfit=NO_MOVE,ambiguous=0,ok=0,sq,length;
  int p=0,from=-1,to=-1,fx=-1,fy=-1,pf,mf,mt,mp,moveno=0,promote=-1;
  MOVE movelist[MAX_MOVES],*lastmove,*m;
  char movestr[FILENAME_MAX];
  BITBOARD temp;
  Undo U;
   
   /* Already on last move */
  if (mvno==999) return NO_MOVE;
   
   /* Get first word and convert to upper case */
  strncpy(movestr,move_to_parse,sizeof(movestr));
  length = (int)strlen(movestr);
  for (n=0;n<length;n++) movestr[n]=toupper(movestr[n]);

   /* Test for castling moves */
  if (strstr(movestr,"O-O-O") || strstr(movestr,"0-0-0")) {
    if (B.side == WHITE) return (MOVE)(e1 + (c1 << 6) + SPECIAL_FLAG);
    else                 return (MOVE)(e8 + (c8 << 6) + SPECIAL_FLAG);
  }
  if (strstr(movestr,"O-O") || strstr(movestr,"0-0")) {
    if (B.side == WHITE) return (MOVE)(e1 + (g1 << 6) + SPECIAL_FLAG);
    else                 return (MOVE)(e8 + (g8 << 6) + SPECIAL_FLAG);
  }
  if (strstr(movestr,"E1G1") && B.pieces[e1]==wking) return (MOVE)(e1 + (g1 << 6) + SPECIAL_FLAG);
  if (strstr(movestr,"E8G8") && B.pieces[e8]==bking) return (MOVE)(e8 + (g8 << 6) + SPECIAL_FLAG);
  if (strstr(movestr,"E1C1") && B.pieces[e1]==wking) return (MOVE)(e1 + (c1 << 6) + SPECIAL_FLAG);
  if (strstr(movestr,"E8C8") && B.pieces[e8]==bking) return (MOVE)(e8 + (c8 << 6) + SPECIAL_FLAG);
   
   /* Remove trailing '+' and '#' characters */
  if (strchr(movestr,'+') || strchr(movestr,'#')) length--;
   
   /* Input is wrong length */
  if (length<2 || length>6) return NO_MOVE;
  
   /* See what we can guess about this move */
  if (length==2) {
     /* Must be a pawn push of the form 'e4' */
    p=pawn;
    to=GetSquare(movestr[0],movestr[1]);
    if (movestr[0]<'A' || movestr[0]>'H') return NO_MOVE;
    if (movestr[1]<'1' || movestr[1]>'8') return NO_MOVE;
    fx=File(to);
  }
  if (length==3) {
     /* Piece move of the form 'Nf3' */
    if (movestr[2] >= '1' && movestr[2] <= '8') {
      if (movestr[1]<'A' || movestr[1]>'H') return NO_MOVE;
      if (movestr[2]<'1' || movestr[2]>'8') return NO_MOVE;
      p = CharToPiece(movestr[0]);
      if (p == empty) return -1;
      to = GetSquare(movestr[1],movestr[2]);
    }
     /* Pawn Promotion of the form 'e8q' */
    else {
      if (movestr[0]<'A' || movestr[0]>'H') return NO_MOVE;
      if (movestr[1]<'1' || movestr[1]>'8') return NO_MOVE;
      promote = -1;
      if (movestr[2]=='Q') promote=1;
      if (movestr[2]=='R') promote=2;
      if (movestr[2]=='N') promote=3;
      if (movestr[2]=='B') promote=4;
      if (promote == -1) return NO_MOVE;
      p=1;
      to = GetSquare(movestr[0],movestr[1]);
      fx = File(to);
    }
  }
  if (length==4) {
    to = GetSquare(movestr[2],movestr[3]);
     /* Test for a capture move of the form 'exd5' or 'Nxg6' */
    if (movestr[1]=='X' || movestr[1]=='*') {
      p = CharToPiece(movestr[0]);
       /* If this is a pawn move then just set the fx */
      if (p==empty) {p = 1;fx = (int)movestr[0]-65;}
       /* Remove the old bishop vs. file-b ambiguity */
      if (p==bishop) {p=-1;ambiguous=1;}
       /* This is the only one that gives real problems with XBoard as it is */
      if (p==-1 && XBoard) {p=1;fx=FileB;ambiguous=0;}
       /* If loading PGN file then we have the benefit of case sensitivity */
      if (ambiguous==1 && LoadPGN) {
        ambiguous=0;
        if (move_to_parse[0]=='b') {p=1;fx=1;}
        else p=4;
      }
    }
     /* Pawn promotion of the form 'e8=q' */
    else if (movestr[2]=='=') { 
      if ((int)movestr[0]<'A' || (int)movestr[0]>'H') return NO_MOVE;
      if ((int)movestr[1]<'1' || (int)movestr[1]>'8') return NO_MOVE;
      promote=-1;
      if (movestr[3]=='Q') promote=1;
      if (movestr[3]=='R') promote=2;
      if (movestr[3]=='N') promote=3;
      if (movestr[3]=='B') promote=4;
      if (promote==-1) return NO_MOVE;
      p=pawn;
      to = GetSquare(movestr[0],movestr[1]);
      fx = File(to);     
    }
     /* This must be a move of the form 'e2e4' or 'Ngf3' or 'Raf1' 
      * Note the unavoidable inherent ambiguity with bishops - B1a3 vs. b1a3 for example
      * in the position 7k/8/8/2B5/8/8/8/1NB4K !  The problem is that I want
      * to be able to parse the move in caps so these look identical.  I'm just going to
      * play safe to avoid these (admittedly extremely rare) cases. B is the only letter
      * which is both a piece and file. */
    else {
      p = CharToPiece(movestr[0]);
       /* Second char is a letter */
      if ((int)movestr[1]>60) fx = (int)movestr[1]-65;
       /* Second char is a number */
      else {
        /* Piece (OR coordinates where the file is 'b').  Set only the 'from' rank. */
        if (p!=empty) fy = 56 - (int)movestr[1];
        else from = GetSquare(movestr[0],movestr[1]);
        /* Careful with bishops and coordinate moves. P=bishop when we're moving a
         * bishop, i.e. B1a3, or a piece on file b, i.e. b8d7 */
        if (!XBoard && p==bishop) ambiguous=1;
        if (XBoard && p==bishop) {p=-1;fx=FileB;}
        if (p==empty || p==bishop) p=-1;
         /* If loading PGN file then we have the benefit of case sensitivity */
        if (ambiguous==1 && LoadPGN) {
          ambiguous=0;
          if (move_to_parse[0]=='b') {p=1;fx=1;}
          else {
            p=4;
            if (movestr[1]>='A') fx = (int)(movestr[1]-'A');
            else fy = 8 - (int)(movestr[1] - '1');
          }
        }
      }
    }
  }
  if (length==5) {
     /* Capture move of the form 'Ngxf3'. Same problem with before with
      * moves like this in coordinate notation i.e. 'b1xa3'. */
    if (movestr[2]=='X' || movestr[2]=='*') {
      p = CharToPiece(movestr[0]);
      to = GetSquare(movestr[3],movestr[4]);
       /* Second char is a letter */
      if ((int)movestr[1]>60) fx = (int)movestr[1]-65;
       /* Second char is a number */
      else {
        /* Piece (OR coordinates where the file is 'b').  Set only the 'from' rank. */
        if (p!=empty) fy = 56 - (int)movestr[1];
        else from = GetSquare(movestr[0],movestr[1]);
        /* Careful with bishops and coordinate moves */
        if (p==bishop) ambiguous=1;
        if (p==empty || p==bishop) p=-1;
         /* If loading PGN file then we have the benefit of case sensitivity */
        if (ambiguous==1 && LoadPGN) {
          ambiguous=0;
          if (move_to_parse[0]=='b') {p=1;fx=1;}
          else {
            p=4;
            if (movestr[1]>='A') fx = (int)(movestr[1]-'A');
            else fy = 8 - (int)(movestr[1] - '1');
          }
        }
      }
    }
    /* Capture and promotion of the form 'dxe8q' */
    else if (movestr[1]=='X' || movestr[1]=='*') {
      fx = (int)movestr[0]-65;
      p = pawn;
      to = GetSquare(movestr[2],movestr[3]);
      if (movestr[4]=='Q') promote=1;
      if (movestr[4]=='R') promote=2;
      if (movestr[4]=='N') promote=3;
      if (movestr[4]=='B') promote=4;
    }
     /* Promotion of the form a7a8q */
    else {
      p = pawn;
      from = GetSquare(movestr[0],movestr[1]);
      to = GetSquare(movestr[2],movestr[3]);
      if (movestr[4]=='Q') promote=1;
      if (movestr[4]=='R') promote=2;
      if (movestr[4]=='N') promote=3;
      if (movestr[4]=='B') promote=4;      
    }
  }
  if (length==6) {
     /* Capture and promotion of the form 'dxe8=q' */
    if (movestr[1]=='X' || movestr[1]=='*') {
      fx = (int)movestr[0]-65;
      p = pawn;
      to = GetSquare(movestr[2],movestr[3]);
      if (movestr[5]=='Q') promote=1;
      if (movestr[5]=='R') promote=2;
      if (movestr[5]=='N') promote=3;
      if (movestr[5]=='B') promote=4;
    }
     /* Capture and promotion of the form 'a7xb8q' */
    else if (movestr[2]=='X' || movestr[2]=='*') {
      p = pawn;
      from = GetSquare(movestr[0],movestr[1]);
      to = GetSquare(movestr[3],movestr[4]);
      if (movestr[5]=='Q') promote=1;
      if (movestr[5]=='R') promote=2;
      if (movestr[5]=='N') promote=3;
      if (movestr[5]=='B') promote=4;      
    }
  }


   /* --- PHEW! --- */
   
   /* Generate a movelist for the current position */ 
  lastmove = GenerateMoves(&B, B.side, movelist);
  
  m = movelist;
  while (m<lastmove) {

     /* We've already tested for castling moves - skip these */
    if (IsCastle(*m)) {m++;continue;}

     /* Perform the move */
    U=DoMove(&B,*m);

     /* Ensure that this move didn't leave us in check */
    if (!InCheck(&B,Opponent(B.side))) {
      mf = MFrom(*m);
      mt = MTo(*m);
      pf   = B.pieces[mt];
      mp = IsPromote(*m);
      if (mp) pf = pawn;

       /* Check this move for suitability */
      ok=0;
      if (ambiguous==0) {
        if ((p==-1 || PType(pf)==p) &&
          (from==-1 || from==mf) &&
          (to==-1 || to==mt) &&
          (fx==-1 || fx==File(mf)) &&
          (fy==-1 || fy==Rank(mf)) &&
          (promote==-1 || promote==mp)) ok=1;
      }
      else {
        if (((PType(pf)==bishop && (fy==-1 || Rank(mf)==fy)) || (PType(pf)==pawn && File(mf)==1)) && 
          (fy==-1 || fy==Rank(mf)) && mp==0 &&
          (to==-1 || to==mt)) ok=1;
        else if (Rank(mf)==fy && File(mf)==FileB && (to==-1 || to==mt) &&
          (promote==-1 || promote==mp)) ok=1;
      }
      
      /* If we think we have an ambiguous move with a bishop on specified rank
       * moving, test to see if this is possible.  This failed in the following position;
       * r1b2rk1/2q1bppp/p2p1n2/npp1p3/3PP3/2P2N1P/PPB2PP1/RNBQR1K1 w - -
       * with the move b1d2 (to check if the ambiguity in ranks is a problem i.e.
       * when B1d2 is distinguishing between a bishop on c1 or c3, I just check to see if
       * there is another bishop on the same file).  I think the best way around this is
       * simply to alter the parsing for XBoard, as I have done.  I also added in another
       * check to make sure the second bishop can actually move to the specified square,
       * but even this is not infallible. */
      if (ok==1 && ambiguous==1 && PType(pf)==bishop && Rank(mf)==fy && File(mf)!=FileB) {
        if (B.side==BLACK) temp=((B.WhiteBishops&FileMask[File(mf)])&InvMask[mf]);
        else               temp=((B.BlackBishops&FileMask[File(mf)])&InvMask[mf]);
        if (temp) {
          sq = FirstPiece(temp);
          if (abs(Rank(sq)-Rank(to)) != abs(File(sq)-File(to))) ok=0;
        }
        else ok=0;
      }
      
      if (ok==1) {
        if (bestfit>-1) {if (giveErrors==TRUE) fprintf(stdout,"Error - Ambiguous Move!\n");bestfit=-2;}
        else bestfit = moveno;
      }
    }
    /* Take the move back */
    UndoMove(&B,*m,U);
    
    moveno++;
    m++;
  }
     
   /* No move fits - Probably an illegal move */
  if (bestfit == -1) {if (giveErrors==TRUE) Error(2);return NO_MOVE;}
   /* Ambiguous move.  Already given a warning message so just quit the parser */
  if (bestfit == -2) return NO_MOVE;

   /* Return move successfully */
  return movelist[bestfit];
}

/* Set the board to a specific move number in the current game */
void GotoMove(Board *B, const char *moveno, const char *side) {
  int tmvno,a;  

  for (a=0;a<(int)strlen(words[2]);a++) words[2][a]=toupper(words[2][a]);

  tmvno = (int)strtol(moveno,NULL,10)*2;
          
  if      (!strcmp(side,"B") || !strcmp(side,"BLACK")) tmvno--;
  else if (!strcmp(side,"W") || !strcmp(side,"WHITE")) tmvno -= 2;
  else    {fprintf(stdout,"Error - Illegal Side!\n");return;}
   
  if (tmvno<0 || tmvno>999) {fprintf(stdout,"Error - Illegal Move Number!\n");return;}
   
  for (a=mvno;a<tmvno;a++)
     if (MoveHistory[a]==NO_MOVE) {fprintf(stdout,"Error - Illegal Move Number!\n");return;}
   
  if (mvno<tmvno) {
    while (mvno<tmvno) {
      UndoHistory[mvno] = DoMove(B,MoveHistory[mvno]);
      mvno++;
    }
  }
  else {
    while (mvno>tmvno) {
      mvno--;
      UndoMove(B,MoveHistory[mvno],UndoHistory[mvno]);
    }
  }
   /* Redisplay the new board */
  PrintBoard(*B);
}

/* Display the movelist */
void MoveList(const char *from,const char *to) {
  int mf,mt,m,a;
   
   /* Get start and finish moves */
  mf = (int)strtol(from,NULL,10);
  mt = (int)strtol(to,NULL,10);
   
   /* Test for legality */
  if (mf<1 || mf>499)  {fprintf(stdout,"Error - Illegal Starting Move!\n");return;}
  if (mt>499 || mt<mf) {fprintf(stdout,"Error - Illegal Ending Move!\n");return;}
   
   /* Test that these moves are within the current game */
  for (a=0;a<(mf*2)-1;a++) 
     if (MoveHistory[a]==NO_MOVE) {fprintf(stdout,"Error - Illegal Starting Move!\n");return;}

   /* Print out the moves */
  for (m=mf;m<=mt;m++) {
    fprintf(stdout,"\n%d.  ",m);
    if (m<100) fprintf(stdout," ");
    if (m<10)  fprintf(stdout," ");
    PrintMove(MoveHistory[(m-1)*2],TRUE,stdout);
    fprintf(stdout,"   ");
    PrintMove(MoveHistory[(m*2)-1],TRUE,stdout);
    if (MoveHistory[(m*2)-1]==NO_MOVE || MoveHistory[(m-1)*2]==NO_MOVE) break;
  }
  fprintf(stdout,"\n\n");
}

/* Print off the command help */
void PrintHelp(void) {
  fprintf(stdout,"Command Help\n------------\n\n");
  fprintf(stdout,"Board, B               Show the Current Board Position\n");
  fprintf(stdout,"FEN, DFEN              Show the FEN code for the current board position\n");
  fprintf(stdout,"New, Reset             Start a new game\n");
  fprintf(stdout,"Comp                   Fully search the current position\n");
  fprintf(stdout,"Help                   Display this screen!\n");
  fprintf(stdout,"Book                   Toggle Opening Book ON/OFF\n");
  fprintf(stdout,"Tablebases, TB         Toggle Endgame Tablebases ON/OFF\n");
  fprintf(stdout,"Test                   Run an EPD test suite\n");
  fprintf(stdout,"XBoard                 Enter XBoard Mode\n");
  fprintf(stdout,"Eval, Analyse          Return Analysis of Current Position\n");
  fprintf(stdout,"Undo, U, Retake, Back  Retake the last move\n");
  fprintf(stdout,"Redo, R, Forward       Redo the current move\n");
  fprintf(stdout,"Post, Nopost           Post thinking ON/OFF\n");
  fprintf(stdout,"White, Black           Set the computer to play the specified side\n");
  fprintf(stdout,"Force, Stop, Coff      Turn the automatic computer play off\n");
  fprintf(stdout,"Movegen, Perft         Test the move generator\n");
  fprintf(stdout,"Depth, SD              Set the default (minimum) search depth\n");
  fprintf(stdout,"Limit, ST              Set the default (maximum) search time\n");
  fprintf(stdout,"Time, Otim             Set the time left on your clock/Opponent's clock\n");
  fprintf(stdout,"Skill                  Set the computer skill (1-10)\n");
  fprintf(stdout,"Hash                   Set up the hash table size\n");
  fprintf(stdout,"Ponder                 Toggle Pondering ON/OFF\n");
  fprintf(stdout,"Setboard, set          Set the board to a FEN position\n");
  fprintf(stdout,"Study                  Study a PGN file for new openings\n");
  fprintf(stdout,"Savebook               Save the current opening book\n");
  fprintf(stdout,"Annotate               Annotate a PGN game or match\n");
  fprintf(stdout,"Goto                   Skip to another place in the move list\n");
  fprintf(stdout,"Moves                  Print off a movelist\n");
  fprintf(stdout,"Quit, Exit, Resign     Quit the program\n\n");
  fprintf(stdout,"e2e4, e4, Nf3 etc.     Play a move.  Most formats can be parsed\n");
  fprintf(stdout,"\n(Plus specific XBoard-only commands)\n");
  fprintf(stdout,"All Commands are case insensitive, including move parsing\n\n");
}

/* Edit mode */
void EditMode(void) {
  int side=1,sq,len;
  char input[FILENAME_MAX],FEN[FILENAME_MAX];
  Board *B = &Current_Board;

  do {
    (void)fgets(input,FILENAME_MAX,stdin);
    len = strlen(input);
    input[len-1]=0;
    if (!strcmp(input,"#")) {
      if (B->side == WHITE) SetBoard(B,"8/8/8/8/8/8/8/8 w - -",TRUE);
      else                  SetBoard(B,"8/8/8/8/8/8/8/8 b - -",TRUE);
    }
    if (!strcmp(input,"c") || !strcmp(input,"C")) side = -side;
    if (strlen(input)==3) {
      sq = GetSquare(input[1],input[2]);
      if (sq<0 || sq>63) {fprintf(stdout,"Illegal Location!\n");break;}
      switch(toupper(input[0])) {
        case 'P': B->pieces[sq] = pawn*side; break;
        case 'R': B->pieces[sq] = rook*side; break;
        case 'N': B->pieces[sq] = knight*side; break;
        case 'B': B->pieces[sq] = bishop*side; break;
        case 'Q': B->pieces[sq] = queen*side; break;
        case 'K': B->pieces[sq] = king*side; break;
        default : fprintf(stdout,"Illegal Piece!\n"); break;
      }
    }
  } while (input[0] != '.');
  BoardToFEN(B,FEN);
  SetBoard(B,FEN,TRUE);
}

/* Set the time control level */
void SetTimeControl(char *nmv, char *tim, char *inc) {
  /* We ignore the time control value (*tim) for now.
   * We can get this from XBoard's periodic updates.
   * This is especially relieving as it has a potentially
   * weird format which would be tricky to parse. */

  /* Sort out the number of moves per time control. */
  Params.Mps = (int)strtol(nmv, NULL, 10);

  /* Sort out the time increment per move in seconds */
  Params.Inc = (int)strtol(inc, NULL, 10);
}

