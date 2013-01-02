/**********************************
 *    eval.h                      *
 *    Colin Frayn                 *
 *    March 2001                  *
 **********************************/

/*
   Contains the specific defines for eval.c
*/

#ifndef EVAL_H
#define EVAL_H

void Analyse(Board);

int Eval(Board *,int,int);
int EvalMain(Board *,int,int);
int LazyEval(Board *);
int EvalPassedPawns(Board *, int, int);
int IsWonGame(Board *);
BOOL IsDrawnMaterial(Board *);
void CheckMatingMaterial(Board *);
BOOL Drawish(Board *);
void CheckAsymmetry(Board *);

#endif /* EVAL_H */
