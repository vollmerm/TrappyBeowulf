/**********************************
 *    comp.h                      *
 *    Colin Frayn                 *
 *    March 2001                  *
 **********************************/

/*
   Contains the specific defines for comp.c
*/

#ifndef COMP_H
#define COMP_H

#ifdef BEOSERVER
/* New Structures required in the BeoServer Code */

// Individual Nodes to be searched
typedef struct NODE {
  int ID,parent;    // This node's ID and that of its parent (or -1 for first ply NODEs)
  KeyType Key;      // 64 Bit Hash Key
  short int depth;  // Depth to which this element was searched, or 0
  long int score;   // Score for this position
  BOOL Done;        // Has a search been done yet?
  BOOL Required;    // Currently part of the search tree?
  BOOL Awaiting;    // Awaiting a result?
  BOOL Split;       // Has this node been split?
  short int cost;   // Cost of analysing this node to the specified depth (last time)
  int alpha,beta;   // A-B values for this node.
  int talpha,tbeta; // A-B values for child nodes.  The reason for this is complicated.
  MOVE move;        // Best move from this node
} NODE;

// Main entries for the node table
typedef struct NODETABLE {
  int entries;      // Number of entries, initially 0.
  NODE *list;       // List of entries, initially NULL
} NODETABLE;
#endif // BESERVER

/* Function defines */

MOVE Comp(void);
int Search(Board *,const int,const int,int,int,const int,int,int,MOVE);
int Quiesce(Board *,const int,const int,int,int,int);
int FilterMovelist(MOVE *,MOVE *,Board *,FullMove *,const int,const MOVE,const int);
int FilterMovelistQui(MOVE *,MOVE *,Board *,FullMove *,const int, const int,MOVE);
void PonderPosition(void);

#ifdef BEOSERVER
int RunParallel(Board *, int, const int, int, int);
int SearchParallel(Board *,const int,const int,int,int,const int,int,int,MOVE,int,int);
void DistributeNode(struct NODE *, Board *,const int,const int,int,int,const int,int,int,MOVE);
NODE *FindNode(KeyType,int);
void ReadWorkDone(char *);
#endif // BEOSERVER

#endif /* COMP_H */
