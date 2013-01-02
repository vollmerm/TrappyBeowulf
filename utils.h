 /**********************************
 *    utils.h                     *
 *    Colin Frayn                 *
 *    March 2001                  *
 **********************************/

/*
   Contains the specific defines for utils.c
*/

#ifndef UTILS_H
#define UTILS_H

int CharToPiece(const char);
int GetSquare(const char,const char);
void RunTestSuite(char [FILENAME_MAX],int);
BOOL ParseEPD(char *, char [100], char [10][20], char [100]);
BOOL CorrectMove(MOVE,char [10][20]);
BOOL SameMove(MOVE,char [20]);
int CheckForDraw(void);
void Result(int);
MOVE CheckOpening(Board *,int *);
void BoardToFEN(Board *, char *);
void GetHint(void);
int Bioskey(void);
void StudyFile(void);
MOVE *ImportPGNGame(FILE *, int *, int *, int *, char *, char *);
int ELOBonus(int);
void SaveOpeningBook(void);
void AnnotateFile(void);

#endif /* UTIL_H */
