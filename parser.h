/**********************************
 *    parser.h                    *
 *    Colin Frayn                 *
 *    March 2001                  *
 **********************************/

/*
   Contains the specific defines for parser.c
*/

#ifndef PARSER_H
#define PARSER_H

int ParseInput(const char *);
int CountWords(const char *);
void GetWords(const char *,const int);
void Error(const int);
MOVE ParseMove(char *,Board,BOOL);
void GotoMove(Board *,const char *,const char *);
void MoveList(const char *,const char *);
void PrintHelp(void);
void EditMode(void);
void SetTimeControl(char *, char *, char *);

#endif /* PARSER_H */
