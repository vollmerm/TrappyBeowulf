/**********************************
 *    main.h                      *
 *    Colin Frayn                 *
 *    March 2001                  *
 **********************************/

/*
   Contains the specific defines for main.c
*/

#ifndef MAIN_H
#define MAIN_H

void SetupPrecalculatedData(void);
void Prompt(void);
void Defaults(void);
void LoadOpeningBook(void);
void LoadPersonalityFile(void);
void FreeOpenings(void);
int ParseCommandLine(int, char **);
void PrintArgs(void);
void GetEnvironmentVars(void);
void GetConfigFile(void);
void CheckParams(void);
int GetGamestage(int);
#ifdef BEOSERVER
int  Benchmark(void);
#endif

#endif /* MAIN_H */
