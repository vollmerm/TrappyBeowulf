/**********************************
 *    common.h                    *
 *    Colin Frayn                 *
 *    March 2001                  *
 **********************************/

/*
 This file includes all the necessary macros, defines etc...
 for most of the source '.c' files
*/

#ifndef COMMON_H
#define COMMON_H

/* Game details */
#define NAME "Beowulf" 
#define VER  "2.4a"

/* Piece Identifiers */
#define pawn    (1)
#define rook    (2)
#define knight  (3)
#define bishop  (4)
#define queen   (5)
#define king    (6)  
#define empty   (0)

#define wpawn   (pawn)
#define wrook   (rook)
#define wknight (knight)
#define wbishop (bishop)
#define wqueen  (queen)
#define wking   (king)

#define bpawn   (-pawn)
#define brook   (-rook)
#define bknight (-knight)
#define bbishop (-bishop)
#define bqueen  (-queen)
#define bking   (-king)  

/* Piece colours */
#define WHITE   (0)
#define BLACK   (1)
#define NEUTRAL (2)
#define Opponent(x)  (1-(x))

/* Castling flags */
#define KingSide  (1)
#define QueenSide (2)
#define OO        (KingSide)
#define OOO       (QueenSide)

/* A few random numbers :) */
#define CMSCORE           (32767)
#define INFINITY          (CMSCORE)
#define THEORETICAL_WIN   (10000)
#define T_WIN_BOUND       (5000)
#define MAX_MOVES         (128)
#define MAX_STALENESS     (3)

/* Maximum PV length */
#define MAX_PV  (200)

/* Just in case... */
#ifndef NULL
#define NULL ((void *)0)
#endif

/* Directory Listing Commands */
#ifdef _WIN32
# define DIR_COMMAND "dir "
#else
# define DIR_COMMAND "ls "
#endif

/* Hash types. */
#define HASH_EXACT  (0)
#define HASH_LOWER  (1)
#define HASH_UPPER  (2)
#define HASH_NULL   (3)

/* Fractional ply lengths */
#define ONEPLY            (8)
#define TWOPLY            (ONEPLY*2)
#define THREEPLY          (ONEPLY*3)
#define FOURPLY           (ONEPLY*4)
#define HALFPLY           (ONEPLY/2)
#define ONEANDAHALFPLY    (ONEPLY+HALFPLY)

/* Many Simple Macros */
#define File(x)           (x & 7)
#define Rank(x)           (x >> 3)
#define PType(x)          (((x) >= 0) ? (x) : -(x))
#define IsWhite(x)        (( ((x)>>3) + (x) + 1 ) & 1)
#define Not(x)            (((x)==(TRUE)) ? (FALSE) : (TRUE))
#define Clip(x)           (((x) < 0) ? (0) : (x))

/* One bizarre macro.  Basically I store the depth for a CM by subtracting
 * 1 from its score each ply.  This macro ensures that moves within a 
 * reasonable score of a CM are counted as such */
#define CMBOUNDS          (1000)
#define CMHIGH            ((CMSCORE) - (CMBOUNDS))
#define CMLOW             (-CMHIGH)
#define IsCM(a)           ((a) > (CMHIGH) ? (1) : ((a) < (CMLOW) ? (-1) : (0)))

/* Simple piece configurations for bitboards */
#define FullRank     (255)
#define TwoFullRanks (65535)

/* More Basic macros */
#ifndef max /*  Returns the maximum of two integers */
# define max(a,b)   (((a) > (b)) ? (a) : (b))
#endif
#ifndef min /*  Returns the minimum of two integers */
# define min(a,b)   (((a) < (b)) ? (a) : (b))
#endif

/* Generate an integer random number */
#define Random(a) ((a) == (0) ? (0) : (int)((double)rand() / ((double)RAND_MAX + 1) * (a)))
#define Randomise()  (srand(time(NULL)))

 /* Time Compatability */
#include <time.h>

#ifndef CLOCKS_PER_SEC
# ifdef CLK_TCK
#  define CLOCKS_PER_SEC CLK_TCK
# else
#  define CLOCKS_PER_SEC 1000000
# endif
#endif
#define CLOCKS_PER_CENTISEC   ((CLOCKS_PER_SEC)/100)

/* Game States */
#define S_QUIT   (-1)
#define S_NOP     (0)
#define S_SEARCH  (1)
#define S_CHKDRW  (2)
#define S_AUTO    (3)

/* Game stages */
#define Opening  (0)
#define EarlyMid (1)
#define Middle   (2)
#define LateMid  (3)
#define Endgame  (4)
#define LateEnd  (5)

/* This is naughty, but what the heck. Good practice is to avoid
 * defining FALSE, but just to test for !TRUE */
#ifndef TRUE
# define TRUE    (1)
#endif
#ifndef FALSE
# define FALSE   (0)
#endif

/* FEN for the start position.  Why not? ;) */
#define Startpos "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq -"

/* Enumerated Ranks and Files */
#define FileA   (0)
#define FileB   (1)
#define FileC   (2)
#define FileD   (3)
#define FileE   (4)
#define FileF   (5)
#define FileG   (6)
#define FileH   (7)
#define Rank1   (7)
#define Rank2   (6)
#define Rank3   (5)
#define Rank4   (4)
#define Rank5   (3)
#define Rank6   (2)
#define Rank7   (1)
#define Rank8   (0)

/* Enumerated board squares */
#define a8 (0)
#define b8 (1)
#define c8 (2)
#define d8 (3)
#define e8 (4)
#define f8 (5)
#define g8 (6)
#define h8 (7)
#define a7 (8)
#define b7 (9)
#define c7 (10)
#define d7 (11)
#define e7 (12)
#define f7 (13)
#define g7 (14)
#define h7 (15)
#define a6 (16)
#define b6 (17)
#define c6 (18)
#define d6 (19)
#define e6 (20)
#define f6 (21)
#define g6 (22)
#define h6 (23)
#define a5 (24)
#define b5 (25)
#define c5 (26)
#define d5 (27)
#define e5 (28)
#define f5 (29)
#define g5 (30)
#define h5 (31)
#define a4 (32)
#define b4 (33)
#define c4 (34)
#define d4 (35)
#define e4 (36)
#define f4 (37)
#define g4 (38)
#define h4 (39)
#define a3 (40)
#define b3 (41)
#define c3 (42)
#define d3 (43)
#define e3 (44)
#define f3 (45)
#define g3 (46)
#define h3 (47)
#define a2 (48)
#define b2 (49)
#define c2 (50)
#define d2 (51)
#define e2 (52)
#define f2 (53)
#define g2 (54)
#define h2 (55)
#define a1 (56)
#define b1 (57)
#define c1 (58)
#define d1 (59)
#define e1 (60)
#define f1 (61)
#define g1 (62)
#define h1 (63)

/* A few precalculated bits and pieces that we need for the move generation */
#define to_c8 (MOVE)(c8<<6)
#define to_c1 (MOVE)(c1<<6)
#define to_g8 (MOVE)(g8<<6)
#define to_g1 (MOVE)(g1<<6)

#define promote_q  (MOVE)(1<<12)
#define promote_r  (MOVE)(2<<12)
#define promote_n  (MOVE)(3<<12)
#define promote_b  (MOVE)(4<<12)

/* UI Input check return values */
#define INPUT_NULL      (0)
#define INPUT_STOP      (1)
#define INPUT_MOVE      (2)
#define INPUT_UNDO      (3)
#define INPUT_NEW       (4)
#define INPUT_RESIGN    (5)
#define INPUT_MOVE_NOW  (6)
#define INPUT_WORK_DONE (7)

/* Datatypes */
#ifdef _MSC_VER
#define longlong __int64
#else
#define longlong long long int
#endif
typedef unsigned longlong BITBOARD;
typedef unsigned longlong u64;
typedef unsigned char u8;
#ifndef BOOL
typedef int BOOL;
#endif
typedef unsigned longlong KeyType;


/*     -----===  Move Storage Classes and Macros  ===-----   */


/* MOVE is now stored as the following;  (16 bits)
 * bits 1-6      : From Square
 * bits 7-12     : To Square
 * bits 13-15    : Promotion piece {0-4} = {None,Q,R,N,B}
 * bit  16       : Special Move?  (i.e. EP capture or castling)
 *                    (Clearly cannot simultaneously be castle & ep) 
 */

typedef short int MOVE;

/* Macros */
#define IsCastle(x)       (((x) & SPECIAL_FLAG) && (MFrom(x)<8 || MFrom(x)>55))
#define IsEP(x)           (((x) & SPECIAL_FLAG) && (MFrom(x)>7 && MFrom(x)<56))
#define IsPromote(x)      (((x) >> 12) & 7)
#define MFrom(x)          ((x)&63)
#define MTo(x)            (((x)>>6)&63)

#define NO_MOVE       (MOVE)(-1)
#define SPECIAL_FLAG  (MOVE)(1<<15)

/* Store the undo information for a move */
typedef struct Undo {
  char ep;         /* The old E-P square, or -1 */
  char capt;       /* The piece captured in the move, or 0 */
  char castle;     /* The old castle permissions setting */
  BITBOARD R90,R45,L45; /* The old rotated bitboards - saves recreating them */
  KeyType Key;     /* The hash key for the board */
}Undo;


/*    -----===  Board Structure Storage Class  ===-----  */


 /* Removes the indicated piece from the board. 
  * The XOR is a little naughty, and fails if the piece doesn't exist! */
#define Remove(a,b)  ((a) = (a^(UNIT<<b)))
 /* Remove the first piece */
#define RemoveFirst(a)   ((a) = ((a) & ((a)-1)))
  /* Miscellaneous Board defines */
#define UNIT   ((BITBOARD)1)
#define EMPTY  ((BITBOARD)0)

/* A Structure to Store the Current State of the Board */
typedef struct Board {
  BITBOARD All;  /* Every piece on the board */
  BITBOARD R90,R45,L45; /* Every piece on the board rotated */
  BITBOARD BlackPieces, WhitePieces;
  BITBOARD BlackPawns, WhitePawns;
  BITBOARD BlackRooks, WhiteRooks;
  BITBOARD BlackKnights, WhiteKnights;
  BITBOARD BlackBishops, WhiteBishops;
  BITBOARD BlackQueens, WhiteQueens;
  int WhiteKing,BlackKing;  /* King square positions */
  signed char pieces[64];  /* Array of pieces.  Much nicer :) */
  int ep; /* En-passant square */
  int castle; /* Bitfield : WKS,WQS,BKS,BQS in that order bits 1-4 */
  int side; /* Side to move */
  int BPts,WPts; /* Points on {1,5,3,3,9} scale */

   /* The following are not continuously defined,
    * and usually need to bet set up when required */
  int WAttacks[144],BAttacks[144];   /* Boards keeping a tally of how much each
                                      * square is attacked by pieces of each colour. 
                                      * Uses only the middle 8*8 squares of the 12*12 arrays.
                                      * This is for optimisation reasons. */
  int Gamestage;  /* Stage of the game (0=Opening, 5=Late endgame) */
  KeyType Key;    /* Hash Key for this board position */
  int wmaj, bmaj; /* Count the major pieces (queen = 2, rook = 1) */
  int wmin, bmin; /* Count the minor pieces (bishop, knight = 1) */
}Board;


/*     -----== Hash Table Storage Class ==-----   */



/* Use full 64-bit hash key info to avoid clashes?  Undefine to switch off. */
#define SAFE_HASH

/* Required types for the hash keys */
#ifdef SAFE_HASH
typedef unsigned longlong StoreKey;
#else
typedef unsigned long int StoreKey;
#endif

/* An element in the hash table.
 * sizeof(HashElt) =  11 or 15 bytes, depending on SAFE_HASH */
typedef struct HashElt {
  MOVE move;        /* Suggested best move, or NO_MOVE */
  StoreKey Key;     /* If SAFE_HASH is set then this is the full 64-bit key.
                     * Otherwise it is the most significant 32 bits of hash key */
  short int depth;  /* Depth to which this element was searched, or 0 */
  short int score;  /* Score for this position */
  char flags;       /* Bits 1-6 for staleness, 7-8 for hash type (as defined above) */
}HashElt;

#define GetHashType(x)     (((x)>>6)&3)
#define GetStaleness(x)    ((x)&63)
#define ResetStaleness(x)  ((x) &= 192)
#define SetHashType(x,y)   ((x) = (char)(((x)&63)|((y)<<6)))
#ifdef SAFE_HASH
# define PackToStoreKey(x)  (StoreKey)(x)
#else
# define PackToStoreKey(x)  (StoreKey)((x) >> 32)
#endif


/*   ---== Other Miscellaneous Storage Classes ==--- */


/* Storage class for a full move plus its details */
typedef struct FullMove {
 MOVE move;        /* The move details */
 long int score;   /* The move ordering score for this move */
 char gchk;        /* Does this move give check? */
}FullMove;

/* An opening book entry */
typedef struct Openpos {
  char *FEN;      /* FEN for this position */
  char nmoves;    /* number of moves in opening book for this position */
  MOVE *m;        /* List of suggested moves */
  int *sc;        /* Weightings for suggested moves */
}Openpos;

/* Store the necessary computer bits and pieces */
typedef struct CompDat {
  int Depth;         /* Minimum depth to search */
  int Time,OTime;    /* Time left on the clocks */
  float MoveTime;    /* Default time per move in sec. '0' = just search minimum depth */
  int Mps, Inc;      /* Moves per time control, time increase per move */
  int HashSize;      /* Hash Table size = 2^HashSize kb */
  BOOL Test;         /* Running a timed test suite? Y/N */
  int WSkill,BSkill; /* Computer Skill levels 1-10 */
  BOOL Resign;       /* Will the computer resign? */
}CompDat;

#endif /* COMMON_H */
