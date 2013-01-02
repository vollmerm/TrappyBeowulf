
/**********************************
 *    moves.c                     *
 *    Colin Frayn                 *
 *    March 2001                  *
 **********************************/

/*
  This file contains all the functions for generating list of
  possible moves from positions. 
*/

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

#include "common.h"
#include "moves.h"
#include "checks.h"
#include "board.h"

extern int PieceValue[7],PromotePiece[5];
extern int DiagShifts_a1h8[64],DiagShifts_a8h1[64];
extern int DiagonalMask_a1h8[64],DiagonalMask_a8h1[64];
extern BITBOARD Mask[64],InvMask[64],FileMask[8],RankMask[8],QueenMask[64];
extern BITBOARD KnightMoves[64],KingMoves[64],MovesRank[64][256],MovesFile[64][256];
extern BITBOARD Movesa1h8[64][256],Movesa8h1[64][256],PawnAttacksBlack[64],PawnAttacksWhite[64];
extern BITBOARD CastleMaskR90[4],CastleMaskR45[4],CastleMaskL45[4];
extern BITBOARD IMR90[64],MR90[64],IMR45[64],IML45[64],MR45[64],ML45[64];
extern int mvno;
extern MOVE MoveHistory[1000];
extern Undo UndoHistory[1000];
extern KeyType RandomTable[64][13],CastleKey[16];

/* Generate all pseudolegal moves for specified side from
 * the specified board position.  Throw them in a list and then
 * forget about them. */
MOVE *GenerateMoves(const Board *B, const int side, MOVE *movelist) {
  int to,from=-1,i;
  MOVE tempmove;
  BITBOARD tsq=0,temp=0,mask;

  /* Generate moves for white pawns first */
  switch (side) {
  case WHITE :
    
    /* Promotions */
    tsq = ((B->WhitePawns >> 8) & FullRank) & ~(B->All);
    while (tsq) {
      to = FirstPiece(tsq);
      from = to + 8;
      tempmove = from + (to<<6);
      for (i=1;i<5;i++) *movelist++ = (tempmove + (i<<12));
      RemoveFirst(tsq);
    }
    /* Captures (including En-Passant) */
    tsq = (((B->WhitePawns & ~(FileMask[FileH])) >> 7) | ((B->WhitePawns & ~(FileMask[FileA])) >> 9));
    if (B->ep>=0) tsq &= (B->BlackPieces | Mask[B->ep]);
    else tsq &= B->BlackPieces;
    while (tsq) {
      to = FirstPiece(tsq);
      if (File(to)>0) {
        from = to + 7;
        if (B->WhitePawns & Mask[from]) {	
          tempmove = from + (to<<6);
          if (Rank(to)==0) for (i=1;i<5;i++) *movelist++ = (tempmove + (i<<12));
          else {
            /* Check for en-passant move */
            if (!(B->BlackPieces & Mask[to])) tempmove |= SPECIAL_FLAG;
            /* Store move */
            *movelist++ = tempmove;
          }
        }
      }
      if (File(to)<7) {
        from = to + 9;
        if (B->WhitePawns & Mask[from]) {
          tempmove = from + (to<<6);
          if (Rank(to)==0) for (i=1;i<5;i++) *movelist++ = (tempmove + (i<<12));
          else {
            /* Check for en-passant move */
            if (!(B->BlackPieces & Mask[to])) tempmove |= SPECIAL_FLAG;
            /* Store move */
            *movelist++ = tempmove;
          }
        }
      }
      RemoveFirst(tsq);
    }
    /* Pushes */
    /* One rank pushes */
    tsq = (B->WhitePawns >> 8) & ~(B->All);
    tsq &= ~RankMask[Rank8];
    /* Two rank initial pushes */
    temp = (((B->WhitePawns & RankMask[Rank2]) >> 16) & ~(B->All));
    /* Ensure that the 'step-over' square is empty too! */
    temp &= ~(B->All >> 8);
    /* Add them on */
    tsq |= temp;
    while (tsq) {
      to = FirstPiece(tsq);
      if (B->WhitePawns & Mask[to+8]) from = to + 8;
      else from = to + 16;
      *movelist++ = from + (to<<6);
      RemoveFirst(tsq);
    }
    
    break;
    
    /* ... or black pawns */
  case BLACK:
    
    /* Promotions */
    tsq = ((B->BlackPawns << 8) & RankMask[Rank1]) & ~(B->All);
    while (tsq) {
      to = FirstPiece(tsq);
      from = to - 8;
      tempmove = from + (to<<6);
      for (i=1;i<5;i++) *movelist++ = (tempmove + (i<<12));
      RemoveFirst(tsq);
    }
    /* Captures (including En-Passant) */
    tsq = (((B->BlackPawns & ~(FileMask[FileH])) << 9) | ((B->BlackPawns & ~(FileMask[FileA])) << 7));
    if (B->ep>=0) tsq &= (B->WhitePieces | Mask[B->ep]);
    else tsq &= B->WhitePieces;
    while (tsq) {
      to = FirstPiece(tsq);
      if (File(to)>0) {
        from = to - 9;
        if (B->BlackPawns & Mask[from]) {	
          tempmove = from + (to<<6);
          if (Rank(to)==7) for (i=1;i<5;i++) *movelist++ = (tempmove + (i<<12));
          else {
            /* Check for en-passant move */
            if (to==B->ep) tempmove |= SPECIAL_FLAG;
            /* Store move */
            *movelist++ = tempmove;
          }
        }
      }
      if (File(to)<7) {
        from = to - 7;
        if (B->BlackPawns & Mask[from]) {
          tempmove = from + (to<<6);
          if (Rank(to)==7) for (i=1;i<5;i++) *movelist++ = (tempmove + (i<<12));
          else {
            /* Check for en-passant move */
            if (to==B->ep) tempmove |= SPECIAL_FLAG;
            /* Store move */
            *movelist++ = tempmove;
          }
        }
      }
      RemoveFirst(tsq);
    }
    /* Pushes */
    /* One rank pushes */
    tsq = (B->BlackPawns << 8) & ~(B->All);
    tsq &= ~RankMask[Rank1];
    /* Two rank initial pushes */
    temp = (((B->BlackPawns & RankMask[Rank7]) << 16) & ~(B->All));
    /* Ensure that the 'step-over' square is empty too! */
    temp &= ~(B->All << 8);
    /* Add them on */
    tsq |= temp;
    while (tsq) {
      to = FirstPiece(tsq);
      if (B->BlackPawns & Mask[to-8]) from = to - 8;
      else from = to - 16;
      *movelist++ = from + (to<<6);
      RemoveFirst(tsq);
    }
    break;
  }

   /* Now Generate King Moves */  
  switch(side) {
   case WHITE:
    from = B->WhiteKing;
    tsq = KingMoves[from] & ~(B->WhitePieces);
    break;
   case BLACK:
    from = B->BlackKing;
    tsq = KingMoves[from] & ~(B->BlackPieces);
    break;
  }
   /* tsq holds a board of all possible target squares */
  while (tsq) {
    to = FirstPiece(tsq);
    *movelist++ = from + (to<<6); 
    RemoveFirst(tsq);
  }

   /* Now Generate Knight Moves */
  switch (side) {
   case WHITE: tsq = B->WhiteKnights; break;
   case BLACK: tsq = B->BlackKnights; break;
  }
   /* tsq holds a board of all possible knights */
  while (tsq) {
    from = FirstPiece(tsq);
    switch(side) {
     case WHITE: temp = KnightMoves[from] & ~(B->WhitePieces); break;
     case BLACK: temp = KnightMoves[from] & ~(B->BlackPieces); break;
    }
     /* temp holds a board of all possible target squares for this knight */
    while (temp) {
      to = FirstPiece(temp);
      *movelist++ = from + (to<<6);
      RemoveFirst(temp);
    }
    RemoveFirst(tsq);
  }

   /* Now Generate Rook Moves */
  switch (side) {
   case WHITE: tsq = B->WhiteRooks; break;
   case BLACK: tsq = B->BlackRooks; break;
  }
   /* tsq now holds a board of all rooks */
  while (tsq) {
    from = FirstPiece(tsq);
     /* First generate horizontal moves */
    mask = (B->All >> (Rank(from)<<3)) & FullRank;
    temp = MovesRank[from][mask];
     /* Next generate vertical moves */
    mask = (B->R90 >> (File(from)<<3)) & FullRank;
    temp |= MovesFile[from][mask];
    switch(side) {
     case WHITE: temp &= ~(B->WhitePieces); break;
     case BLACK: temp &= ~(B->BlackPieces); break;
    }
     /* temp holds a board of all possible target squares */
    while (temp) {
      to = FirstPiece(temp);
      *movelist++ = from + (to<<6); 
      RemoveFirst(temp);
    }
    RemoveFirst(tsq);
  }

   /* Now Generate Bishop Moves
    * See above for comments, or look at my webpage
    * http://www.ast.cam.ac.uk/~cmf/chess/theory.html */
  switch (side) {
   case WHITE: tsq = B->WhiteBishops; break;
   case BLACK: tsq = B->BlackBishops; break;
  }
  while (tsq) {
    from = FirstPiece(tsq);
    mask = ((B->R45 >> DiagShifts_a1h8[from]) & DiagonalMask_a1h8[from]);
    temp = Movesa1h8[from][mask];
    mask = ((B->L45 >> DiagShifts_a8h1[from]) & DiagonalMask_a8h1[from]);
    temp |= Movesa8h1[from][mask];
    switch(side) {
     case WHITE: temp &= ~(B->WhitePieces); break;
     case BLACK: temp &= ~(B->BlackPieces); break;
    }
    while (temp) {
      to = FirstPiece(temp);
      *movelist++ = from + (to<<6); 
      RemoveFirst(temp);
    }
    RemoveFirst(tsq);
  }

   /* Now Generate Queen Moves */
  switch (side) {
   case WHITE: tsq = B->WhiteQueens; break;
   case BLACK: tsq = B->BlackQueens; break;
  }
  while (tsq) {
    from = FirstPiece(tsq);
    mask = ((B->R45 >> DiagShifts_a1h8[from]) & DiagonalMask_a1h8[from]);
    temp = Movesa1h8[from][mask];
    mask = ((B->L45 >> DiagShifts_a8h1[from]) & DiagonalMask_a8h1[from]);
    temp |= Movesa8h1[from][mask];
    mask = (B->All & RankMask[(Rank(from))]) >> (Rank(from)<<3);
    temp |= MovesRank[from][mask];
    mask = (B->R90 & RankMask[(File(from))]) >> (File(from)<<3);
    temp |= MovesFile[from][mask];
    switch(side) {
     case WHITE: temp &= ~(B->WhitePieces); break;
     case BLACK: temp &= ~(B->BlackPieces); break;
    }
    while (temp) {
      to = FirstPiece(temp);
      *movelist++ = from + (to<<6); 
      RemoveFirst(temp);
    }
    RemoveFirst(tsq);
  }
     
   /* Generate Castling Moves */
  switch (side) {
   case WHITE:
    tempmove = e1 | SPECIAL_FLAG;
     /* O-O */
    if ((B->castle & 1) && B->pieces[h1]==wrook) {
      mask=96;
      if (!(B->All & (mask<<56))) *movelist++ = tempmove + to_g1;
    }
     /* O-O-O */
    if ((B->castle & 2) && B->pieces[a1]==wrook) {
      mask=14;
      if (!(B->All & (mask<<56))) *movelist++ = tempmove + to_c1;
    }
    break;
   case BLACK:
    tempmove = e8 | SPECIAL_FLAG;
     /* O-O */
    if ((B->castle & 4) && B->pieces[h8]==brook) {
      if (!(B->All & 96)) *movelist++ = tempmove + to_g8;
    }
     /* O-O-O */
    if ((B->castle & 8) && B->pieces[a8]==brook) {
      if (!(B->All & 14)) *movelist++ = tempmove + to_c8;
    }
    break;
  }
   
   /* Return the last element so we know where we got up to in the move 
    * list i.e. how many moves there are to check */
  return movelist;
}

/* Generate all pseudolegal capture moves for specified side
 * from the specified board position.  This is useful for the
 * quiescence search */
MOVE *GenerateCaptures(const Board *B, const int side, MOVE *movelist) {
  int to,from,i;
  MOVE tempmove;
  BITBOARD tsq=0,temp,mask;
  
  /* Generate moves for white pawns first */
  if (side==WHITE) {
    
    /* Captures (including En-Passant) */
    tsq = (((B->WhitePawns & ~(FileMask[FileH])) >> 7) | ((B->WhitePawns & ~(FileMask[FileA])) >> 9));
    if (B->ep>0) tsq &= (B->BlackPieces | Mask[B->ep]);
    else tsq &= B->BlackPieces;
    while (tsq) {
      to = FirstPiece(tsq);
      if (File(to)>0) {
        from = to + 7;
        if (B->WhitePawns & Mask[from]) {	
          tempmove = from + (to<<6);
          if (Rank(to)==0) for (i=1;i<5;i++) *movelist++ = (tempmove + (i<<12));
          else {
            /* Check for en-passant move */
            if (to==B->ep) tempmove |= SPECIAL_FLAG;
            /* Store move */
            *movelist++ = tempmove;
          }
        }
      }
      if (File(to)<7) {
        from = to + 9;
        if (B->WhitePawns & Mask[from]) {
          tempmove = from + (to<<6);
          if (Rank(to)==0) for (i=1;i<5;i++) *movelist++ = (tempmove + (i<<12));
          else {
            /* Check for en-passant move */
            if (to==B->ep) tempmove |= SPECIAL_FLAG;
            /* Store move */
            *movelist++ = tempmove;
          }
        }
      }
      RemoveFirst(tsq);
    }
  }
  
  /* ... or black pawns */
  if (side==BLACK) {
    
    /* Captures (including En-Passant) */
    tsq = (((B->BlackPawns & ~(FileMask[FileH])) << 9) | ((B->BlackPawns & ~(FileMask[FileA])) << 7));
    if (B->ep>0) tsq &= (B->WhitePieces | Mask[B->ep]);
    else tsq &= B->WhitePieces;
    while (tsq) {
      to = FirstPiece(tsq);
      if (File(to)>0) {
        from = to - 9;
        if (B->BlackPawns & Mask[from]) {	
          tempmove = from + (to<<6);
          if (Rank(to)==7) for (i=1;i<5;i++) *movelist++ = (tempmove + (i<<12));
          else {
            /* Check for en-passant move */
            if (to==B->ep) tempmove |= SPECIAL_FLAG;
            /* Store move */
            *movelist++ = tempmove;
          }
        }
      }
      if (File(to)<7) {
        from = to - 7;
        if (B->BlackPawns & Mask[from]) {
          tempmove = from + (to<<6);
          if (Rank(to)==7) for (i=1;i<5;i++) *movelist++ = (tempmove + (i<<12));
          else {
            /* Check for en-passant move */
            if (to==B->ep) tempmove |= SPECIAL_FLAG;
            /* Store move */
            *movelist++ = tempmove;
          }
        }
      }
      RemoveFirst(tsq);
    }
  }
  
  /* Now Generate King Moves */  
  if (side==WHITE) {
    from = B->WhiteKing;
    tsq = (KingMoves[from] & B->BlackPieces);
  }
  else {
    from = B->BlackKing;
    tsq = (KingMoves[from] & B->WhitePieces);
  }
  while (tsq) {
    to = FirstPiece(tsq);
    *movelist++ = from + (to<<6); 
    RemoveFirst(tsq);
  }
  
  /* Now Generate Knight Moves */
  if (side==WHITE) tsq = B->WhiteKnights;
  else             tsq = B->BlackKnights;
  while (tsq) {
    from = FirstPiece(tsq);
    if (side==WHITE) temp = KnightMoves[from] & B->BlackPieces;
    else             temp = KnightMoves[from] & B->WhitePieces;
    while (temp) {
      to = FirstPiece(temp);
      *movelist++ = from + (to<<6);
      RemoveFirst(temp);
    }
    RemoveFirst(tsq);
  }
  
  /* Now Generate Rook Moves */
  if (side==WHITE) tsq = B->WhiteRooks;
  else tsq = B->BlackRooks;
  while (tsq) {
    from = FirstPiece(tsq);
    mask = (B->All >> (Rank(from)<<3)) & FullRank;
    temp = MovesRank[from][mask];
    mask = (B->R90 >> (File(from)<<3)) & FullRank;
    temp |= MovesFile[from][mask];
    if (side==WHITE) temp &= B->BlackPieces;
    else             temp &= B->WhitePieces;
    while (temp) {
      to = FirstPiece(temp);
      *movelist++ = from + (to<<6); 
      RemoveFirst(temp);
    }
    RemoveFirst(tsq);
  }
  
  /* Now Generate Bishop Moves */
  if (side==WHITE) tsq = B->WhiteBishops;
  else tsq = B->BlackBishops;
  while (tsq) {
    from = FirstPiece(tsq);
    mask = ((B->R45 >> DiagShifts_a1h8[from]) & DiagonalMask_a1h8[from]);
    temp = Movesa1h8[from][mask];
    mask = ((B->L45 >> DiagShifts_a8h1[from]) & DiagonalMask_a8h1[from]);
    temp |= Movesa8h1[from][mask];
    if (side==WHITE) temp &= B->BlackPieces;
    else             temp &= B->WhitePieces;
    while (temp) {
      to = FirstPiece(temp);
      *movelist++ = from + (to<<6); 
      RemoveFirst(temp);
    }
    RemoveFirst(tsq);
  }
  
  /* Now Generate Queen Moves */
  if (side==WHITE) tsq = B->WhiteQueens;
  else tsq = B->BlackQueens;
  while (tsq) {
    from = FirstPiece(tsq);
    mask = ((B->R45 >> DiagShifts_a1h8[from]) & DiagonalMask_a1h8[from]);
    temp = Movesa1h8[from][mask];
    mask = ((B->L45 >> DiagShifts_a8h1[from]) & DiagonalMask_a8h1[from]);
    temp |= Movesa8h1[from][mask];
    mask = (B->All & RankMask[(Rank(from))]) >> (Rank(from)<<3);
    temp |= MovesRank[from][mask];
    mask = (B->R90 & RankMask[(File(from))]) >> (File(from)<<3);
    temp |= MovesFile[from][mask];
    if (side==WHITE) temp &= B->BlackPieces;
    else             temp &= B->WhitePieces;
    while (temp) {
      to = FirstPiece(temp);
      *movelist++ = from + (to<<6); 
      RemoveFirst(temp);
    }
    RemoveFirst(tsq);
  }
  
  /* Return the last element so we know where we got up to in the move 
    * list i.e. how many moves there are to check */
  return movelist;
}

/* Generate all pseudolegal moves for specified side blocking
 * sliding attacks to the specified square from the specified
 * board position. Ignore captures and king moves. This is used
 * for escaping checks. */
MOVE *GenerateBlockingMoves(const Board *B, const int side, const int target, MOVE *movelist) {
  int to,from,i,ty = Rank(target);
  MOVE tempmove;
  BITBOARD tsq=0,temp,mask,TargetMask = QueenMask[target],PieceTargetMask;

  PieceTargetMask = TargetMask & ~(B->All);
   
   /* Generate moves for white pawns first */
  if (side==WHITE) {
     
     /* Promotions */
    if (ty==0) tsq = ((B->WhitePawns >> 8) & FullRank) & ~(B->All);
    else tsq=0;
    while (tsq) {
      to = FirstPiece(tsq);
      from = to + 8;
      tempmove = from + (to<<6);
      for (i=1;i<5;i++) *movelist++ = (tempmove + (i<<12));
      RemoveFirst(tsq);
    }
     /* Pushes */
     /* One rank pushes */
    tsq = (B->WhitePawns >> 8) & ~(B->All);
    tsq &= ~RankMask[Rank8];
     /* Two rank initial pushes */
    temp = (((B->WhitePawns & RankMask[Rank2]) >> 16) & ~(B->All));
     /* Ensure that the 'step-over' square is empty too! */
    temp &= ~(B->All >> 8);
     /* Add them on */
    tsq |= temp;
     /* Mask them to generate only blockers */
    tsq &= TargetMask;
    while (tsq) {
      to = FirstPiece(tsq);
      if (B->WhitePawns & Mask[to+8]) from = to + 8;
      else from = to + 16;
      *movelist++ = from + (to<<6);
      RemoveFirst(tsq);
    }
     
  }

   /* ... or black pawns */
  if (side==BLACK) {
     
     /* Promotions */
    if (ty==7) tsq = ((B->BlackPawns << 8) & RankMask[Rank1]) & ~(B->All);
    else tsq=0;
    while (tsq) {
      to = FirstPiece(tsq);
      from = to - 8;
      tempmove = from + (to<<6);
      for (i=1;i<5;i++) *movelist++ = (tempmove + (i<<12));
      RemoveFirst(tsq);
    }
     /* Pushes */
     /* One rank pushes */
    tsq = (B->BlackPawns << 8) & ~(B->All);
    tsq &= ~RankMask[Rank1];
     /* Two rank initial pushes */
    temp = (((B->BlackPawns & RankMask[Rank7]) << 16) & ~(B->All));
     /* Ensure that the 'step-over' square is empty too! */
    temp &= ~(B->All << 8);
     /* Add them on */
    tsq |= temp;
     /* Mask them to generate only blockers */
    tsq &= TargetMask;
    while (tsq) {
      to = FirstPiece(tsq);
      if (B->BlackPawns & Mask[to-8]) from = to - 8;
      else from = to - 16;
      *movelist++ = from + (to<<6);
      RemoveFirst(tsq);
    }
  }

   /* Now Generate Knight Moves */
  if (side==WHITE) tsq = B->WhiteKnights;
  else             tsq = B->BlackKnights;
  while (tsq) {
    from = FirstPiece(tsq);
    if (side==WHITE) temp = KnightMoves[from] & PieceTargetMask;
    else             temp = KnightMoves[from] & PieceTargetMask;
    while (temp) {
      to = FirstPiece(temp);
      *movelist++ = from + (to<<6);
      RemoveFirst(temp);
    }
    RemoveFirst(tsq);
  }

   /* Now Generate Rook Moves */
  if (side==WHITE) tsq = B->WhiteRooks;
  else tsq = B->BlackRooks;
  while (tsq) {
    from = FirstPiece(tsq);
    mask = (B->All >> (Rank(from)<<3)) & FullRank;
    temp = MovesRank[from][mask];
    mask = (B->R90 >> (File(from)<<3)) & FullRank;
    temp |= MovesFile[from][mask];
    if (side==WHITE) temp &= PieceTargetMask;
    else             temp &= PieceTargetMask;
    while (temp) {
      to = FirstPiece(temp);
      *movelist++ = from + (to<<6); 
      RemoveFirst(temp);
    }
    RemoveFirst(tsq);
  }

   /* Now Generate Bishop Moves */
  if (side==WHITE) tsq = B->WhiteBishops;
  else tsq = B->BlackBishops;
  while (tsq) {
    from = FirstPiece(tsq);
    mask = ((B->R45 >> DiagShifts_a1h8[from]) & DiagonalMask_a1h8[from]);
    temp = Movesa1h8[from][mask];
    mask = ((B->L45 >> DiagShifts_a8h1[from]) & DiagonalMask_a8h1[from]);
    temp |= Movesa8h1[from][mask];
    if (side==WHITE) temp &= PieceTargetMask;
    else             temp &= PieceTargetMask;
    while (temp) {
      to = FirstPiece(temp);
      *movelist++ = from + (to<<6); 
      RemoveFirst(temp);
    }
    RemoveFirst(tsq);
  }

   /* Now Generate Queen Moves */
  if (side==WHITE) tsq = B->WhiteQueens;
  else tsq = B->BlackQueens;
  while (tsq) {
    from = FirstPiece(tsq);
    mask = ((B->R45 >> DiagShifts_a1h8[from]) & DiagonalMask_a1h8[from]);
    temp = Movesa1h8[from][mask];
    mask = ((B->L45 >> DiagShifts_a8h1[from]) & DiagonalMask_a8h1[from]);
    temp |= Movesa8h1[from][mask];
    mask = (B->All & RankMask[(Rank(from))]) >> (Rank(from)<<3);
    temp |= MovesRank[from][mask];
    mask = (B->R90 & RankMask[(File(from))]) >> (File(from)<<3);
    temp |= MovesFile[from][mask];
    if (side==WHITE) temp &= PieceTargetMask;
    else             temp &= PieceTargetMask;
    while (temp) {
      to = FirstPiece(temp);
      *movelist++ = from + (to<<6); 
      RemoveFirst(temp);
    }
    RemoveFirst(tsq);
  }
   
   /* Return the last element so we know where we got up to in the move 
    * list i.e. how many moves there are to check */
  return movelist;
}

/* Generate all moves which could evade check for the current
 * side.  'piece' holds the piece known to be attacking, if any.  This
 * is useful because you can avoid trying to block unblockable moves 
 * (like those of a pawn or a knight) */
MOVE *GenerateCheckEvasions(const Board *B, const int side, MOVE *movelist, const int piece) {
  int from,to;
  BITBOARD tsq;
   
   /* Generate King Moves */  
  if (side==WHITE) {
    from = B->WhiteKing;
     // Include only semi-legal non-capturing moves
     // (We add in capturing moves below)
    tsq = KingMoves[from] & ~(B->All);
  }
  else {
    from = B->BlackKing;
     // Include only semi-legal non-capturing moves
     // (We add in capturing moves below)
    tsq = KingMoves[from] & ~(B->All);
  }
  while (tsq) {
    to = FirstPiece(tsq);
    *movelist++ = from + (to<<6); 
    RemoveFirst(tsq);
  }

  movelist = GenerateCaptures(B,side,movelist);
   
  if (piece==3 || piece==6 || piece==1) return movelist;
   
  movelist = GenerateBlockingMoves(B,side,from,movelist);
   
  return movelist;
}

/* Generate all pawn promotions from the given position.  This is used in
 * the quiescence search, so we consider only queen promotions.  */
MOVE *GeneratePromotions(const Board *B, const int side, MOVE *movelist) {
  int from,to;
  MOVE tempmove;
  BITBOARD tsq;
   
  switch (side) {
  case WHITE :
    tsq = ((B->WhitePawns >> 8) & FullRank) & ~(B->All);
    while (tsq) {
      to = FirstPiece(tsq);
      from = to + 8;
      tempmove = from + (to<<6);
      *movelist++ = (tempmove + promote_q);
      RemoveFirst(tsq);
    }
    break;    
  case BLACK:  
    tsq = ((B->BlackPawns << 8) & RankMask[Rank1]) & ~(B->All);
    while (tsq) {
      to = FirstPiece(tsq);
      from = to - 8;
      tempmove = from + (to<<6);
      *movelist++ = (tempmove + promote_q);
      RemoveFirst(tsq);
    }
    break;
  }

  return movelist;
}

/* Generate all pseudolegal non-quiescent moves for specified side */
MOVE *GenerateNonQuiescentMoves(const Board *B, const int side, MOVE *movelist) {
  int to,from=-1,i;
  MOVE tempmove;
  BITBOARD tsq=0,temp=0,mask,rtarget,btarget;

  /* Generate moves for white pawns first */
  switch (side) {
  case WHITE :
    /* Promotions */
    tsq = ((B->WhitePawns >> 8) & FullRank) & ~(B->All);
    while (tsq) {
      to = FirstPiece(tsq);
      from = to + 8;
      tempmove = from + (to<<6);
      for (i=1;i<5;i++) *movelist++ = (tempmove + (i<<12));
      RemoveFirst(tsq);
    }
    /* Captures (including En-Passant) */
    tsq = (((B->WhitePawns & ~(FileMask[FileH])) >> 7) | ((B->WhitePawns & ~(FileMask[FileA])) >> 9));
    if (B->ep>=0) tsq &= (B->BlackPieces | Mask[B->ep]);
    else tsq &= B->BlackPieces;
    while (tsq) {
      to = FirstPiece(tsq);
      if (File(to)>0) {
        from = to + 7;
        if (B->WhitePawns & Mask[from]) {	
          tempmove = from + (to<<6);
          if (Rank(to)==0) for (i=1;i<5;i++) *movelist++ = (tempmove + (i<<12));
          else {
            /* Check for en-passant move */
            if (!(B->BlackPieces & Mask[to])) tempmove |= SPECIAL_FLAG;
            /* Store move */
            *movelist++ = tempmove;
          }
        }
      }
      if (File(to)<7) {
        from = to + 9;
        if (B->WhitePawns & Mask[from]) {
          tempmove = from + (to<<6);
          if (Rank(to)==0) for (i=1;i<5;i++) *movelist++ = (tempmove + (i<<12));
          else {
            /* Check for en-passant move */
            if (!(B->BlackPieces & Mask[to])) tempmove |= SPECIAL_FLAG;
            /* Store move */
            *movelist++ = tempmove;
          }
        }
      }
      RemoveFirst(tsq);
    }
    /* Pushes that give check */
    /* One rank pushes */
    tsq = (B->WhitePawns >> 8) & ~(B->All);
    tsq &= ~RankMask[Rank8];
    /* Two rank initial pushes */
    temp = (((B->WhitePawns & RankMask[Rank2]) >> 16) & ~(B->All));
    /* Ensure that the 'step-over' square is empty too! */
    temp &= ~(B->All >> 8);
    tsq |= temp;
    /* Select out only checking moves */
    tsq &= PawnAttacksBlack[B->BlackKing];
    /* Add them on */
    while (tsq) {
      to = FirstPiece(tsq);
      if (B->WhitePawns & Mask[to+8]) from = to + 8;
      else from = to + 16;
      *movelist++ = from + (to<<6);
      RemoveFirst(tsq);
    }
    
    break;
    
    /* ... or black pawns */
  case BLACK:
    /* Promotions */
    tsq = ((B->BlackPawns << 8) & RankMask[Rank1]) & ~(B->All);
    while (tsq) {
      to = FirstPiece(tsq);
      from = to - 8;
      tempmove = from + (to<<6);
      for (i=1;i<5;i++) *movelist++ = (tempmove + (i<<12));
      RemoveFirst(tsq);
    }
    /* Captures (including En-Passant) */
    tsq = (((B->BlackPawns & ~(FileMask[FileH])) << 9) | ((B->BlackPawns & ~(FileMask[FileA])) << 7));
    if (B->ep>=0) tsq &= (B->WhitePieces | Mask[B->ep]);
    else tsq &= B->WhitePieces;
    while (tsq) {
      to = FirstPiece(tsq);
      if (File(to)>0) {
        from = to - 9;
        if (B->BlackPawns & Mask[from]) {	
          tempmove = from + (to<<6);
          if (Rank(to)==7) for (i=1;i<5;i++) *movelist++ = (tempmove + (i<<12));
          else {
            /* Check for en-passant move */
            if (to==B->ep) tempmove |= SPECIAL_FLAG;
            /* Store move */
            *movelist++ = tempmove;
          }
        }
      }
      if (File(to)<7) {
        from = to - 7;
        if (B->BlackPawns & Mask[from]) {
          tempmove = from + (to<<6);
          if (Rank(to)==7) for (i=1;i<5;i++) *movelist++ = (tempmove + (i<<12));
          else {
            /* Check for en-passant move */
            if (to==B->ep) tempmove |= SPECIAL_FLAG;
            /* Store move */
            *movelist++ = tempmove;
          }
        }
      }
      RemoveFirst(tsq);
    }
    /* Pushes that give check */
    /* One rank pushes */
    tsq = (B->BlackPawns << 8) & ~(B->All);
    tsq &= ~RankMask[Rank1];
    /* Two rank initial pushes */
    temp = (((B->BlackPawns & RankMask[Rank7]) << 16) & ~(B->All));
    /* Ensure that the 'step-over' square is empty too! */
    temp &= ~(B->All << 8);
    tsq |= temp;
    /* Select out only checking moves */
    tsq &= PawnAttacksWhite[B->WhiteKing];
    /* Add them on */
    while (tsq) {
      to = FirstPiece(tsq);
      if (B->BlackPawns & Mask[to-8]) from = to - 8;
      else from = to - 16;
      *movelist++ = from + (to<<6);
      RemoveFirst(tsq);
    }
    break;
  }

  /* Now Generate King Moves that capture */  
  switch(side) {
   case WHITE:
    from = B->WhiteKing;
    tsq = KingMoves[from] & B->BlackPieces;
    break;
   case BLACK:
    from = B->BlackKing;
    tsq = KingMoves[from] & B->WhitePieces;
    break;
  }
   /* tsq holds a board of all possible target squares */
  while (tsq) {
    to = FirstPiece(tsq);
    *movelist++ = from + (to<<6); 
    RemoveFirst(tsq);
  }


   /* Now Generate Knight Moves that capture or give check */
  switch (side) {
   case WHITE: tsq = B->WhiteKnights; break;
   case BLACK: tsq = B->BlackKnights; break;
  }
   /* tsq holds a board of all possible knights */
  while (tsq) {
    from = FirstPiece(tsq);
    switch(side) {
     case WHITE: temp = KnightMoves[from] & (B->BlackPieces|(~B->All&KnightMoves[B->BlackKing])); break;
     case BLACK: temp = KnightMoves[from] & (B->WhitePieces|(~B->All&KnightMoves[B->WhiteKing])); break;
    }
     /* temp holds a board of all possible target squares for this knight */
    while (temp) {
      to = FirstPiece(temp);
      *movelist++ = from + (to<<6);
      RemoveFirst(temp);
    }
    RemoveFirst(tsq);
  }

  /* Now Generate Rook Moves that capture or give check */
  switch (side) {
   case WHITE: tsq = B->WhiteRooks; rtarget = RookMoves(B,B->BlackKing); break;
   case BLACK: tsq = B->BlackRooks; rtarget = RookMoves(B,B->WhiteKing); break;
  }

   /* tsq now holds a board of all rooks */
  while (tsq) {
    from = FirstPiece(tsq);
     /* First generate horizontal moves */
    mask = (B->All >> (Rank(from)<<3)) & FullRank;
    temp = MovesRank[from][mask];
     /* Next generate vertical moves */
    mask = (B->R90 >> (File(from)<<3)) & FullRank;
    temp |= MovesFile[from][mask];
    switch(side) {
     case WHITE: temp &= (B->BlackPieces|(~B->All&rtarget)); break;
     case BLACK: temp &= (B->WhitePieces|(~B->All&rtarget)); break;
    }
     /* temp holds a board of all possible target squares */
    while (temp) {
      to = FirstPiece(temp);
      *movelist++ = from + (to<<6); 
      RemoveFirst(temp);
    }
    RemoveFirst(tsq);
  }

   /* Now Generate Bishop Moves that capture or give check 
    * See above for comments, or look at my webpage
    * http://www.ast.cam.ac.uk/~cmf/chess/theory.html */
  switch (side) {
   case WHITE: tsq = B->WhiteBishops; btarget = BishopMoves(B,B->BlackKing); break;
   case BLACK: tsq = B->BlackBishops; btarget = BishopMoves(B,B->WhiteKing); break;
  }
  while (tsq) {
    from = FirstPiece(tsq);
    mask = ((B->R45 >> DiagShifts_a1h8[from]) & DiagonalMask_a1h8[from]);
    temp = Movesa1h8[from][mask];
    mask = ((B->L45 >> DiagShifts_a8h1[from]) & DiagonalMask_a8h1[from]);
    temp |= Movesa8h1[from][mask];
    switch(side) {
     case WHITE: temp &= (B->BlackPieces|(~B->All&btarget)); break;
     case BLACK: temp &= (B->WhitePieces|(~B->All&btarget)); break;
    }
    while (temp) {
      to = FirstPiece(temp);
      *movelist++ = from + (to<<6); 
      RemoveFirst(temp);
    }
    RemoveFirst(tsq);
  }

   /* Now Generate Queen Moves that capture or give check */
  switch (side) {
   case WHITE: tsq = B->WhiteQueens; break;
   case BLACK: tsq = B->BlackQueens; break;
  }
  while (tsq) {
    from = FirstPiece(tsq);
    mask = ((B->R45 >> DiagShifts_a1h8[from]) & DiagonalMask_a1h8[from]);
    temp = Movesa1h8[from][mask];
    mask = ((B->L45 >> DiagShifts_a8h1[from]) & DiagonalMask_a8h1[from]);
    temp |= Movesa8h1[from][mask];
    mask = (B->All & RankMask[(Rank(from))]) >> (Rank(from)<<3);
    temp |= MovesRank[from][mask];
    mask = (B->R90 & RankMask[(File(from))]) >> (File(from)<<3);
    temp |= MovesFile[from][mask];
    switch(side) {
     case WHITE: temp &= (B->BlackPieces|(~B->All&(rtarget|btarget))); break;
     case BLACK: temp &= (B->WhitePieces|(~B->All&(rtarget|btarget))); break;
    }
    while (temp) {
      to = FirstPiece(temp);
      *movelist++ = from + (to<<6); 
      RemoveFirst(temp);
    }
    RemoveFirst(tsq);
  }

  /* Return the last element so we know where we got up to in the move 
   * list i.e. how many moves there are to check */
  return movelist;
}

      /* ----------======###    END OF MOVE GENERATION ROUTINES    ###======----------- */


/* Perform a move on the given Board.  Note - this routine does
 * NOT check to see if this move is legal!!! */
Undo DoMove(Board *B,const MOVE move) {
  int from=MFrom(move),to=MTo(move),promote=IsPromote(move);
  int castle=IsCastle(move),ep = IsEP(move);
  int pf=B->pieces[from],pt=B->pieces[to],side = B->side;
  BITBOARD M = Mask[to], MIM = (Mask[from] | M), CMask;
  Undo U;
  
  /* Set up the undo information */
  U.ep = B->ep;
  U.capt = 0;
  U.castle = B->castle;
  U.R90 = B->R90;
  U.R45 = B->R45;
  U.L45 = B->L45;
  U.Key = B->Key;
  
  /* Alter the necessary global position information */
  B->ep=-1; /* No en-passant square just yet... */
  B->side = Opponent(side);  /* Swap sides */
  B->Key ^= CastleKey[(int)U.castle];

  /* Normal Move */
  if ((castle==0) && (promote==0)) {
    /* Update the bitboards */
    if (side==WHITE) {
#ifdef DEBUG_VERIFY
    if (B->pieces[from] <= 0) {
      fprintf(stderr,"Illegal Move in DoMove() (%d - %d)\n",from,to);
      PrintBoard(*B);
      while (1);
    }
#endif
      B->WhitePieces ^= MIM;
      switch (pf) {
       case (wpawn)  : B->WhitePawns ^= MIM; if (to==from-16) B->ep = from-8; break;
       case (wrook)  : B->WhiteRooks ^= MIM; break;
       case (wknight): B->WhiteKnights ^= MIM; break;
       case (wbishop): B->WhiteBishops ^= MIM; break;
       case (wqueen) : B->WhiteQueens ^= MIM; break;
       case (wking)  : B->WhiteKing = to; break;
      }
      B->pieces[from] = empty;
      B->pieces[to] = pf;
      /* Check for Capture */
      switch (pt) {
       case (bpawn)  : B->BlackPieces ^= M;B->BlackPawns ^= M; B->BPts--;U.capt=bpawn; break;
       case (brook)  : B->BlackPieces ^= M;B->BlackRooks ^= M; B->BPts -= 5;U.capt=brook; break;
       case (bknight): B->BlackPieces ^= M;B->BlackKnights ^= M; B->BPts -= 3;U.capt=bknight; break;
       case (bbishop): B->BlackPieces ^= M;B->BlackBishops ^= M; B->BPts -= 3;U.capt=bbishop; break;
       case (bqueen) : B->BlackPieces ^= M;B->BlackQueens ^= M; B->BPts -= 9;U.capt=bqueen; break;
      }
      /* Check for screwing up castling */
      switch(from) {
       case (a1): B->castle &= 13; break;
       case (h1): B->castle &= 14; break;
       case (e1): B->castle &= 12; break;
      }
      switch(to) {
       case (a8): B->castle &= 7; break;
       case (h8): B->castle &= 11; break;
      }
      /* Check for EP capture */
      if (ep) {
        B->BlackPieces &= InvMask[to+8];
        B->BlackPawns &= InvMask[to+8];
        B->pieces[to+8] = empty;
        B->BPts--;
        U.capt = bpawn;
        B->R90 &= IMR90[to+8];
        B->R45 &= IMR45[to+8];
        B->L45 &= IML45[to+8];
        B->Key ^= RandomTable[to+8][empty+6];
        B->Key ^= RandomTable[to+8][bpawn+6];
      }
    }
    if (side==BLACK) {
#ifdef DEBUG_VERIFY
    if (B->pieces[from] >= 0) {
      fprintf(stderr,"Illegal Move in DoMove() (%d - %d)\n",from,to);
      PrintBoard(*B);
      while (1);
    }
#endif
      B->BlackPieces ^= MIM;
      switch (pf) {
       case (bpawn)  : B->BlackPawns ^= MIM; if (to==from+16) B->ep = from+8; break;
       case (brook)  : B->BlackRooks ^= MIM; break;
       case (bknight): B->BlackKnights ^= MIM; break;
       case (bbishop): B->BlackBishops ^= MIM; break;
       case (bqueen) : B->BlackQueens ^= MIM; break;
       case (bking)  : B->BlackKing = to; break;
      }
      B->pieces[from] = empty;
      B->pieces[to] = pf;
      /* Check for Capture */
      switch (pt) {
       case (wpawn)  : B->WhitePieces ^= M;B->WhitePawns ^= M; B->WPts--;U.capt=wpawn; break;
       case (wrook)  : B->WhitePieces ^= M;B->WhiteRooks ^= M; B->WPts -= 5;U.capt=wrook; break;
       case (wknight): B->WhitePieces ^= M;B->WhiteKnights ^= M; B->WPts -= 3;U.capt=wknight; break;
       case (wbishop): B->WhitePieces ^= M;B->WhiteBishops ^= M; B->WPts -= 3;U.capt=wbishop; break;
       case (wqueen) : B->WhitePieces ^= M;B->WhiteQueens ^= M; B->WPts -= 9;U.capt=wqueen; break;
      }
      /* Check for screwing up castling */
      switch(from) {
       case (a8): B->castle &= 7; break;
       case (h8): B->castle &= 11; break;
       case (e8): B->castle &= 3; break;
      }
      switch(to) {
       case (a1): B->castle &= 13; break;
       case (h1): B->castle &= 14; break;
      }
      /* Check for EP capture */
      if (ep) {
        B->WhitePieces &= InvMask[to-8];
        B->WhitePawns &= InvMask[to-8];
        B->pieces[to-8] = empty;
        B->WPts--;
        U.capt = wpawn;
        B->R90 &= IMR90[to-8];
        B->R45 &= IMR45[to-8];
        B->L45 &= IML45[to-8];
        B->Key ^= RandomTable[to-8][empty+6];
        B->Key ^= RandomTable[to-8][wpawn+6];
      }
    }
    
    /* Update Boards */
    B->All  = B->WhitePieces | B->BlackPieces;
    B->R90 &= IMR90[from];
    B->R90 |= MR90[to];
    B->R45 &= IMR45[from];
    B->R45 |= MR45[to];
    B->L45 &= IML45[from];
    B->L45 |= ML45[to];
    /* Update Hash Key (this slows the movegen down a bit) */
    B->Key ^= RandomTable[from][pf+6];
    B->Key ^= RandomTable[from][empty+6];
    B->Key ^= RandomTable[to][pt+6]; 
    B->Key ^= RandomTable[to][pf+6]; 
    B->Key ^= CastleKey[B->castle]; 
    return U;
  }
  
  /* Castling */
  if (castle) {
    if (side==WHITE) {
      /* Reset castle flags to zero */
      B->castle &= 12;
      /* Queen's Side */
      if (to<from) {
        B->pieces[a1] = B->pieces[e1] = empty;
        B->pieces[c1] = wking;
        B->pieces[d1] = wrook;
        B->WhiteRooks |= Mask[d1];
        B->WhiteRooks &= InvMask[a1];
        B->WhiteKing = c1;
        CMask = 29;  /* 1+4+8+16 */
        B->R90 ^= CastleMaskR90[1];
        B->R45 ^= CastleMaskR45[1];
        B->L45 ^= CastleMaskL45[1];
        /* Update Hash Key (this is slow) */
        B->Key ^= RandomTable[a1][wrook+6];
        B->Key ^= RandomTable[a1][empty+6];
        B->Key ^= RandomTable[c1][wking+6];
        B->Key ^= RandomTable[c1][empty+6];
        B->Key ^= RandomTable[d1][wrook+6];
        B->Key ^= RandomTable[d1][empty+6];
      }
      /* King's Side */
      else {
        B->pieces[h1] = B->pieces[e1] = empty;
        B->pieces[g1] = wking;
        B->pieces[f1] = wrook;
        B->WhiteRooks |= Mask[f1];
        B->WhiteRooks &= InvMask[h1];
        B->WhiteKing = g1;
        CMask = 240;  /* 16+32+64+128 */
        B->R90 ^= CastleMaskR90[0];
        B->R45 ^= CastleMaskR45[0];
        B->L45 ^= CastleMaskL45[0];
        /* Update Hash Key (this is slow) */
        B->Key ^= RandomTable[h1][wrook+6];
        B->Key ^= RandomTable[h1][empty+6];
        B->Key ^= RandomTable[g1][wking+6];
        B->Key ^= RandomTable[g1][empty+6];
        B->Key ^= RandomTable[f1][wrook+6];
        B->Key ^= RandomTable[f1][empty+6];
      }
      /* Cunning trick to reset piece bitboard */
      B->WhitePieces ^= (CMask<<56);
      /* Update Hash Key */
      B->Key ^= RandomTable[e1][wking+6];
      B->Key ^= RandomTable[e1][empty+6];
    }
    if (side==BLACK) {
      /* Reset castle flags to zero */
      B->castle &= 3;
      /* Queen's side */
      if (to<from) {
        B->pieces[a8] = B->pieces[e8] = empty;
        B->pieces[c8] = bking;
        B->pieces[d8] = brook;
        B->BlackRooks |= Mask[d8];
        B->BlackRooks &= InvMask[a8];
        B->BlackKing = c8;
        CMask = 29;  /* 1+4+8+16 */
        B->R90 ^= CastleMaskR90[3];
        B->R45 ^= CastleMaskR45[3];
        B->L45 ^= CastleMaskL45[3];
        /* Update Hash Key (this is slow) */
        B->Key ^= RandomTable[a8][brook+6];
       B->Key ^= RandomTable[a8][empty+6];
        B->Key ^= RandomTable[c8][bking+6];
        B->Key ^= RandomTable[c8][empty+6];
        B->Key ^= RandomTable[d8][brook+6];
        B->Key ^= RandomTable[d8][empty+6];
      }
      /* King's side */
      else {
        B->pieces[h8] = B->pieces[e8] = empty;
        B->pieces[g8] = bking;
        B->pieces[f8] = brook;
        B->BlackRooks |= Mask[f8];
        B->BlackRooks &= InvMask[h8];
        B->BlackKing = g8;
        CMask = 240;  /* 16+32+64+128 */
        B->R90 ^= CastleMaskR90[2];
        B->R45 ^= CastleMaskR45[2];
        B->L45 ^= CastleMaskL45[2];
        /* Update Hash Key (this is slow) */
        B->Key ^= RandomTable[h8][brook+6];
        B->Key ^= RandomTable[h8][empty+6];
        B->Key ^= RandomTable[g8][bking+6];
        B->Key ^= RandomTable[g8][empty+6];
        B->Key ^= RandomTable[f8][brook+6];
        B->Key ^= RandomTable[f8][empty+6];
      }
      /* Cunning trick to reset piece bitboard */
      B->BlackPieces ^= CMask;
      /* Update Hash Key (this is slow) */
      B->Key ^= RandomTable[e8][bking+6];
      B->Key ^= RandomTable[e8][empty+6];
    }
    /* Update Total Board */
    B->All = B->WhitePieces | B->BlackPieces;
    B->Key ^= CastleKey[B->castle]; 
    return U;
  }
  
  /* Promotion */
  if (promote) {
    /* Convert number in 0-4 form to the correct promotion piece */
    promote = PromotePiece[promote];
    B->pieces[from] = 0;
    /* Update the bitboards */
    if (side==WHITE) {
      B->WhitePieces ^= MIM;
      B->WhitePawns &= InvMask[from];
      B->pieces[to] = promote;
      /* Check for Capture */
      if (pt) {
        B->BlackPieces ^= M;
        switch (pt) {
         case (brook)   : B->BlackRooks ^= M; B->BPts -= 5; break;
         case (bknight) : B->BlackKnights ^= M; B->BPts -= 3; break;
         case (bbishop) : B->BlackBishops ^= M; B->BPts -= 3; break;
         case (bqueen)  : B->BlackQueens ^= M; B->BPts -= 9; break;
        }
        U.capt = pt;
      }
      /* Keep track of the promoted piece */
      switch (promote) {
       case (rook)   : B->WhiteRooks |= M; B->WPts += 4; break;
       case (knight) : B->WhiteKnights |= M; B->WPts += 2; break;
       case (bishop) : B->WhiteBishops |= M; B->WPts += 2; break;
       case (queen)  : B->WhiteQueens |= M; B->WPts += 8; break;
      }
      /* Update Hash Key (this is slow) */
      B->Key ^= RandomTable[from][wpawn+6];
      B->Key ^= RandomTable[to][promote+6];
    }
    if (side==BLACK) {
      B->BlackPieces ^= MIM;
      B->BlackPawns &= InvMask[from];
      B->pieces[to] = -promote;
      /* Check for Capture */
      if (pt) {
        B->WhitePieces ^= M;
        switch (pt) {
         case (wrook)   : B->WhiteRooks ^= M; B->WPts -= 5; break;
         case (wknight) : B->WhiteKnights ^= M; B->WPts -= 3; break;
         case (wbishop) : B->WhiteBishops ^= M; B->WPts -= 3; break;
         case (wqueen)  : B->WhiteQueens ^= M; B->WPts -= 9; break;
        }
        U.capt = pt;
      }
      /* Keep track of the promoted piece */
      switch (promote) {
       case (rook)   : B->BlackRooks |= M; B->BPts += 4; break;
       case (knight) : B->BlackKnights |= M; B->BPts += 2; break;
       case (bishop) : B->BlackBishops |= M; B->BPts += 2; break;
       case (queen)  : B->BlackQueens |= M; B->BPts += 8; break;
      }
      /* Update Hash Key (this is slow) */
      B->Key ^= RandomTable[from][bpawn+6];
      B->Key ^= RandomTable[to][6-promote];
    }
    /* Update Boards */
    B->All = B->WhitePieces | B->BlackPieces;
    B->R90 &= IMR90[from];
    B->R90 |= MR90[to];
    B->R45 &= IMR45[from];
    B->R45 |= MR45[to];
    B->L45 &= IML45[from];
    B->L45 |= ML45[to];
    /* Update Hash Key (this is slow) */
    B->Key ^= RandomTable[from][empty+6];
    B->Key ^= RandomTable[to][pt+6]; 
    B->Key ^= CastleKey[B->castle]; 
    return U;
  }
  
  return U;
}

/* Undo a move on the given Board.  Note - this routine 
 * assumes that the undo information is correct */
void UndoMove(Board *B, const MOVE move, const Undo U) {
  int from=MFrom(move),to=MTo(move),promote=IsPromote(move),castle=IsCastle(move),ep = IsEP(move);
  int pf=B->pieces[to], pt=U.capt, side = Opponent(B->side);
  BITBOARD M = Mask[to], MIM = (Mask[from] | M), CMask;

  B->ep=U.ep; /* Restore old en-passant setting */
  B->side = side;  /* Swap sides back */
  B->castle = U.castle; /* Restore old castle permissions setting */
  B->R90 = U.R90;
  B->R45 = U.R45;
  B->L45 = U.L45;
  B->Key = U.Key;
   
   /* Normal Move */
  if ((castle==0) & (promote==0)) {
    /* Update the bitboards */
    if (side==WHITE) {
      B->WhitePieces ^= MIM;
      switch (pf) {
       case (wpawn)  : B->WhitePawns ^= MIM; break;
       case (wrook)  : B->WhiteRooks ^= MIM; break;
       case (wknight): B->WhiteKnights ^= MIM; break;
       case (wbishop): B->WhiteBishops ^= MIM; break;
       case (wqueen) : B->WhiteQueens ^= MIM; break;
       case (wking)  : B->WhiteKing = from; break;
      }
      B->pieces[to] = pt;
      B->pieces[from] = pf;
      /* Check for Capture */
      if (pt && !ep) {
        B->BlackPieces |= M;
        switch (pt) {
         case (bpawn)  : B->BlackPawns |= M; B->BPts++; break;
         case (brook)  : B->BlackRooks |= M; B->BPts += 5; break;
         case (bknight): B->BlackKnights |= M; B->BPts += 3; break;
         case (bbishop): B->BlackBishops |= M; B->BPts += 3; break;
         case (bqueen) : B->BlackQueens |= M; B->BPts += 9; break;
        }
      }
      /* Check for EP capture. */
      if (ep) {
        B->BlackPieces |= Mask[to+8];
        B->BlackPawns |= Mask[to+8];
        B->pieces[to+8] = bpawn;
        B->pieces[to] = empty;
        B->BPts += PieceValue[pawn];
      }
    }
    if (side==BLACK) {
      B->BlackPieces ^= MIM;
      switch (pf) {
       case (bpawn)  : B->BlackPawns ^= MIM; break;
       case (brook)  : B->BlackRooks ^= MIM; break;
       case (bknight): B->BlackKnights ^= MIM; break;
       case (bbishop): B->BlackBishops ^= MIM; break;
       case (bqueen) : B->BlackQueens ^= MIM; break;
       case (bking)  : B->BlackKing = from; break;
      }
      B->pieces[to] = pt;
      B->pieces[from] = pf;
      /* Check for Capture */
      if (pt && !ep) {
        B->WhitePieces |= M;
        switch (pt) {
         case (wpawn)  : B->WhitePawns |= M; B->WPts++; break;
         case (wrook)  : B->WhiteRooks |= M; B->WPts += 5; break;
         case (wknight): B->WhiteKnights |= M; B->WPts += 3; break;
         case (wbishop): B->WhiteBishops |= M; B->WPts += 3; break;
         case (wqueen) : B->WhiteQueens |= M; B->WPts += 9; break;
        }
      }
      /* Check for EP capture. */
      if (ep) {
        B->WhitePieces |= Mask[to-8];
        B->WhitePawns |= Mask[to-8];
        B->pieces[to-8] = wpawn;
        B->pieces[to] = empty;
        B->WPts += PieceValue[pawn];
      }
    }
    B->All = B->WhitePieces | B->BlackPieces;
    return;
  }
  
  /* Castling */
  if (castle) {
    if (side==WHITE) {
      B->WhiteKing = e1;
      if (to<from) {
        B->pieces[a1] = wrook;
        B->pieces[e1] = wking;
        B->pieces[c1] = B->pieces[d1] = empty;
        B->WhiteRooks &= InvMask[d1];
        B->WhiteRooks |= Mask[a1];
        CMask = 29;  /* 1+4+8+16 */
      }
      else {
        B->pieces[e1] = wking;
        B->pieces[h1] = wrook;
        B->pieces[f1] = B->pieces[g1] = empty;
        B->WhiteRooks &= InvMask[f1];
        B->WhiteRooks |= Mask[h1];
        CMask = 240;  /* 16+32+64+128 */
      }
      /* Cunning trick to reset piece bitboard */
      B->WhitePieces ^= (CMask<<56);
    }
    if (side==BLACK) {
      B->BlackKing = e8;
      if (to<from) {
        B->pieces[e8] = bking;
        B->pieces[a8] = brook;
        B->pieces[c8] = B->pieces[d8] = empty;
        B->BlackRooks &= InvMask[d8];
        B->BlackRooks |= Mask[a8];
        CMask = 29;  /* 1+4+8+16 */
      }
      else {
        B->pieces[e8] = bking;
        B->pieces[h8] = brook;
        B->pieces[f8] = B->pieces[g8] = empty;
        B->BlackRooks &= InvMask[f8];
        B->BlackRooks |= Mask[h8];
        CMask = 240;  /* 16+32+64+128 */
      }
      /* Cunning trick to reset piece bitboard */
      B->BlackPieces ^= CMask;
    }
    B->All = B->WhitePieces | B->BlackPieces;
    return;
  }
  
  /* Promotion */
  if (promote) {
    promote = PType(pf);
    B->pieces[to] = pt;
    /* Update the bitboards */
    if (side==WHITE) {
      B->WhitePieces ^= MIM;
      B->WhitePawns |= Mask[from];
      B->pieces[from] = wpawn;
      /* Check for Capture */
      if (pt) {
        B->BlackPieces |= M;
        switch (pt) {
         case (brook)   : B->BlackRooks |= M; B->BPts += 5; break;
         case (bknight) : B->BlackKnights |= M; B->BPts += 3; break;
         case (bbishop) : B->BlackBishops |= M; B->BPts += 3; break;
         case (bqueen)  : B->BlackQueens |= M; B->BPts += 9; break;
        }
      }
      switch (promote) {
       case (rook)   : B->WhiteRooks &= ~M; B->WPts -= 4; break;
       case (knight) : B->WhiteKnights &= ~M; B->WPts -= 2; break;
       case (bishop) : B->WhiteBishops &= ~M; B->WPts -= 2; break;
       case (queen)  : B->WhiteQueens &= ~M; B->WPts -= 8; break;
      }
    }
    if (side==BLACK) {
      B->BlackPieces ^= MIM;
      B->BlackPawns |= Mask[from];
      B->pieces[from] = bpawn;
      /* Check for Capture */
      if (pt) {
        B->WhitePieces |= M;
        switch (pt) {
         case (wrook)   : B->WhiteRooks |= M; B->WPts += 5; break;
         case (wknight) : B->WhiteKnights |= M; B->WPts += 3; break;
         case (wbishop) : B->WhiteBishops |= M; B->WPts += 3; break;
         case (wqueen)  : B->WhiteQueens |= M; B->WPts += 9; break;
        }
      }
      switch (promote) {
       case (rook)   : B->BlackRooks &= ~M; B->BPts -= 4; break;
       case (knight) : B->BlackKnights &= ~M; B->BPts -= 2; break;
       case (bishop) : B->BlackBishops &= ~M; B->BPts -= 2; break;
       case (queen)  : B->BlackQueens &= ~M; B->BPts -= 8; break;
      }
    }
    B->All = B->WhitePieces | B->BlackPieces;
  }
  assert(B->WPts >= 0 && B->WPts <= 103);
  assert(B->BPts >= 0 && B->BPts <= 103);
  return;
}


/* Check if castling is possible in the sense indicated, and if
 * so then do it */
int Check_Castle(Board *B, const int type) {
  int dir=0,side = B->side,inchk;
  int cmask = 1 << ((side*2)+type-1),kpos,to,i;
  MOVE move;
  BITBOARD mask;
  Undo U;
   
   /* This castling move is illegal as either the required rook or the
    * king are flagged as having moved */
  if (!(B->castle & cmask)) return S_NOP;
   
   /* Test to see if we're in check */
  if (InCheck(B,B->side)) return S_NOP;
   
   /* Get the king position */
  if (side==WHITE) kpos = B->WhiteKing;
  else             kpos = B->BlackKing;

   /* Set up the direction in which this castling move will shift the king */
  if (type==OOO) dir = -1;
  else dir = 1;

   /* Check for blockers */
  if (type == OOO) mask = 14;
  else             mask = 96;
  if (side == WHITE) mask <<= 56;
  if (B->All & mask) return S_NOP;
   
   /* Test to see if the intermediate or target squares are attacked */
  for (i=1;i<3;i++) {
    to = kpos + (dir*i);
    move = kpos + (to<<6);
    U = DoMove(B,move);
    inchk = InCheck(B,Opponent(B->side));
    UndoMove(B,move,U);
    if (inchk) return S_NOP;
  }

   /* If we're already on the last move */
  if (mvno==999) {fprintf(stdout,"Error - Already On Last Move!\n");return S_NOP;}
   
   /* Setup the castling move */
  if (side==WHITE && type==OOO) move = e1 + (c1 << 6);
  if (side==WHITE && type==OO)  move = e1 + (g1 << 6); 
  if (side==BLACK && type==OOO) move = e8 + (c8 << 6); 
  if (side==BLACK && type==OO)  move = e8 + (g8 << 6); 
  move |= SPECIAL_FLAG;
   
   /* We're OK! Let's castle! */
  U=DoMove(B,move);
   
   /* Store the move in the move history */
  MoveHistory[mvno] = move;
  UndoHistory[mvno] = U;

  mvno++;
  MoveHistory[mvno] = NO_MOVE;
   
   /* Success! */
  return S_CHKDRW;
}


/* Check to see if the given castling move is legal */
BOOL IsLegalCastle(Board *B,const MOVE m) {
  int to = MTo(m), kpos = (MFrom(m)),i,inchk;
  MOVE move;
  Undo U;
   
   /* Test to see if we're in check */
  if (InCheck(B,B->side)) return FALSE;
   
   /* Test to see if the intermediate or target squares are attacked */
  if (to<kpos) {
    for (i=kpos-1;i>=to;i--) {
      move = kpos + (i<<6);
      U = DoMove(B,move);
      inchk = InCheck(B,Opponent(B->side));
      UndoMove(B,move,U);
      if (inchk) return FALSE;
    }
  }
  else {
    for (i=kpos+1;i<=to;i++) {
      move = kpos + (i<<6);
      U = DoMove(B,move);
      inchk = InCheck(B,Opponent(B->side));
      UndoMove(B,move,U);
      if (inchk) return FALSE;
    }
  }

   /* Success! */
  return TRUE;
}


/* Generates all moves down to a certain ply without any pruning.
 * Counts them and returns the result. */
long int CountMoves(Board *B, const int depth, const int level) {
  MOVE movelist[MAX_MOVES],*m,*lastmove;
  long int nm=0;
  Undo U;

  lastmove = GenerateMoves(B,B->side,movelist);
  m = movelist;
   
  while (m<lastmove) {
     /* Filter out illegal castling moves (due to intermediate check) */
    if (IsCastle(*m) && !IsLegalCastle(B,*m)) {m++;continue;}
     /* Perform the move */
    U=DoMove(B,*m);
     
     /* Ensure that this move didn't leave us in check */
    if (!InCheck(B,Opponent(B->side))) {
       /* Update progress indicators at top ply */
      if (level==0) fprintf(stdout,".");
      if (depth>1) nm += CountMoves(B,depth-1,level+1);
      else nm++;
    }

     /* Take the move back */
    UndoMove(B,*m,U);
    m++;
  }
  return nm;
}

/* Print a Move */
void PrintMove(const MOVE m, const BOOL pad, FILE *out) {
  int from=MFrom(m), to=MTo(m), promote=IsPromote(m), castle=IsCastle(m);
  int fx,fy,tx,ty;

   /* Invalid move */
  if (m==NO_MOVE) return;
   
   /* Castling move */
  if (castle) {
    if (from>to) {fprintf(out,"O-O-O");if (pad) fprintf(out," ");}
    else {fprintf(out,"O-O");if (pad) fprintf(out,"  ");}
  }
   /* Normal move */
  else {
    fx = File(from);
    fy = Rank(from);
    tx = File(to);
    ty = Rank(to);
    fprintf(out,"%c%d%c%d",(char)(fx+97),8-fy,(char)(tx+97),8-ty);
    switch (promote) {
     case (1) : fprintf(out,"q"); break;
     case (2) : fprintf(out,"r"); break;
     case (3) : fprintf(out,"n"); break;
     case (4) : fprintf(out,"b"); break;
    }
    if (pad) fprintf(out," ");
  }
}

/* Generate all queen moves for the specified square */
BITBOARD QueenMoves(const Board *B,int from) {
  BITBOARD Moves,mask;

  mask = ((B->R45 >> DiagShifts_a1h8[from]) & DiagonalMask_a1h8[from]);
  Moves = Movesa1h8[from][mask];
  mask = ((B->L45 >> DiagShifts_a8h1[from]) & DiagonalMask_a8h1[from]);
  Moves |= Movesa8h1[from][mask];
  mask = (B->All & RankMask[(Rank(from))]) >> (Rank(from)<<3);
  Moves |= MovesRank[from][mask];
  mask = (B->R90 & RankMask[(File(from))]) >> (File(from)<<3);
  Moves |= MovesFile[from][mask];

  return Moves;
}

/* Generate all rook moves for the specified square */
BITBOARD RookMoves(const Board *B,int from) {
  BITBOARD Moves,mask;

  mask = (B->All & RankMask[(Rank(from))]) >> (Rank(from)<<3);
  Moves = MovesRank[from][mask];
  mask = (B->R90 & RankMask[(File(from))]) >> (File(from)<<3);
  Moves |= MovesFile[from][mask];

  return Moves;
}

/* Generate all bishop moves for the specified square */
BITBOARD BishopMoves(const Board *B,int from) {
  BITBOARD Moves,mask;

  mask = ((B->R45 >> DiagShifts_a1h8[from]) & DiagonalMask_a1h8[from]);
  Moves = Movesa1h8[from][mask];
  mask = ((B->L45 >> DiagShifts_a8h1[from]) & DiagonalMask_a8h1[from]);
  Moves |= Movesa8h1[from][mask];

  return Moves;
}
