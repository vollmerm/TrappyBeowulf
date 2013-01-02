/**********************************
 *    computil.h                  *
 *    Colin Frayn                 *
 *    March 2001                  *
 **********************************/

/*
   Contains the specific defines for computil.c
*/

#ifndef COMPUTIL_H
#define COMPUTIL_H

/* Function defines */

void SortFrom(FullMove *,const int, const int);
void IncreaseCounts(const FullMove, const int, const int, const int);
void SetupHash(void);
void ResetValues(Board *);
void PrintPV(Board *);
void WritePVToText(Board *);
char *MoveToText(char *, MOVE, Board *);
BOOL ContinueSearch(const int, const int,const int);
BOOL CheckTime(const int, const int);
BOOL IsDrawn(Board *, const int, const int, BOOL);
void PrintThinking(const int, Board *);
void GetTimeLimit(void);
void ModifyTime(const int);
HashElt *HashProbe(Board *);
void HashUpdate(Board *, int, MOVE, int, short int, BOOL, int);
void GenerateHashKey(Board *);
BOOL RepeatedPosition(const Board *, const int, const int);
int GetRepeatedPositions(void);
BOOL DrawByRepetition(int, int, KeyType);
MOVE CheckMove(MOVE);
BOOL CheckResign(const int);
void Pad(long int,int);
void PrintInfo(const int);
int NullOK(Board *,int);
long int LargestPrime(long int n);
int CheckUserInput(void);
void ExpireHash(void);
void ResetHash(void);
void SetStartTime(void);
int GetElapsedTime(void);

#endif /* COMPUTIL_H */
