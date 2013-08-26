#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include "trappy.h"

void swap(int *a, int *b) {
  int t=*a; *a=*b; *b=t;
}
void sort(int arr[], int beg, int end) {
  if (end > beg + 1) {
    int piv = arr[beg], l = beg + 1, r = end;
    while (l < r) {
      if (arr[l] <= piv)
        l++;
      else
        swap(&arr[l], &arr[--r]);
    }
    swap(&arr[--l], &arr[beg]);
    sort(arr, beg, l);
    sort(arr, r, end);
  }
}

int findMedian(const int * Scores, int scoreCount) {
  int *sorted = (int*)malloc(sizeof(int) * scoreCount);
  int ret, i;

	memcpy(sorted,Scores,sizeof(int) * scoreCount);

  sort(sorted,0,scoreCount);
  ret = *(sorted + scoreCount/2);
  free(sorted);
  return ret;
}

int findBiggest(const int * Scores, int scoreCount) {
  int max = Scores[0];
  int i;
  for (i = 1; i < scoreCount; i++) 
    if (Scores[i] > max) max = Scores[i];
  return max;
}

int trapDepth(const int * Scores, int score, int scoreCount) {
  int i;
  for (i = scoreCount - 1; Scores[i] != score; i--);
  return i;
}

float trappiness(int last, const int * Scores, int scoreCount, int ply) {
  int inner, distance;
  if (TRAP_METHOD == 1)
    inner = findMedian(Scores, scoreCount);
  else if (TRAP_METHOD == 2)
    inner = findBiggest(Scores, scoreCount);
  else 
    inner = *(Scores + scoreCount-1);
  distance = trapDepth(Scores, inner, scoreCount);
  if (distance > scoreCount/2) {
    inner += distance - scoreCount/2;
  }
  //printf("Trappiness inner %d last %d\n",inner,last);
  int absLast = abs(last);
  //if (absLast == 0) { absLast = 1; }
  if (ply % 2 == 1) {
    if (inner <= last) 
      return 0;
    if (last < inner && inner < last+absLast) 
      return 0.75*(inner-last)/absLast;
    if (last+absLast <= inner && inner < last+(4*absLast))
      return 0.75+0.25*(inner-last-absLast)/(3*absLast);
    else
      return 1;
  } else {
    if (inner >= last) 
      return 0;
    if (last > inner && inner > last-abs(last))
      return 0.75*(last-inner)/abs(last);
    if (last-abs(last) >= inner && inner > last-4*abs(last))
      return 0.75+0.25*(last-inner-abs(last))/(3*abs(last));
    else
      return 1;
  }
}

float scale(float T, int best) {
#if TRAP_SCALE == 1
  int M = abs(best);
  float min = 0, max = M*2, a = 0, b = M*TRAP_CEILING;
  if (M <= 1) return 1;
  if (T < a) return 0;
  if (T > M*2) return M*TRAP_CEILING;
  return ((b-a)*(T-min))/(max-min);
#else
	return T;
#endif
  //else   
  //  return T;
  /*
  if (T > 0 && T < 0.25*M)
    return (0.2*M)/0.25;
  if (T >= 0.25*M && T < 2*M)
    return 0.2+0.05*(T-M)/1.75;
  else if (T >= 2*M)
    return 0.25*M;
  else {
    perror("Scaling error!");
    exit(1);
  }
  */

}

void writeTrapData(int TrapSet, int TrapsFound) {
  FILE *fp;
  //if (TrapsFound) return; 
  fp = fopen("trapresults.csv","a");
  if (fp == NULL) {
    printf("Could not open trapresults.dat for writing.\n");
    return;
  }
  fprintf(fp,"%d,%d\n", TrapSet, TrapsFound);
  fclose(fp);
}

void WriteBoardDataLite(Board b, MOVE top, MOVE best, MOVE trap, int bestscore, int trapscore, int profit)
{
  FILE *fp = fopen("litedata.dat","a");
  char FEN[FILENAME_MAX];
  BoardToFEN(&b, FEN);
  PrintBoardFile(b, fp);
  fprintf(fp, "Top move ");
  PrintMove(top, TRUE, fp);
  fprintf(fp, "\nBest move %d ", bestscore);
  PrintMove(best, TRUE, fp);
  fprintf(fp, "\nTrap move %d ", trapscore);
  PrintMove(trap, TRUE, fp);
  fprintf(fp, "\nProfit %d\nMax Depth %d\n\n", profit, MAX_DEPTH);
  fclose(fp);
}

void WriteMarker()
{
  FILE *fp = fopen("litedata.dat","a");
  fprintf(fp, "********* MARKER\n");
  fclose(fp);
}

void WriteBoardData(MOVE trapm, MOVE bestm, Board b, Board c, int best, int adj, 
    int *scores, int scoresCount, int ply) {
  FILE *fp;
  char FEN1[FILENAME_MAX],FEN2[FILENAME_MAX];
  BoardToFEN(&b,FEN1);
  BoardToFEN(&c,FEN2);
  int i;
  fp = fopen("boardout.dat","a");
  if (fp == NULL) {
    printf("Could not open boardout.dat for writing.\n");
    exit(1);
  }
  fprintf(fp, "Best move: ", best);
  PrintMove(bestm, TRUE, fp);
  fprintf(fp, "\n");
  fprintf(fp, "Trap move: ");
  PrintMove(trapm, TRUE, fp);
  fprintf(fp, "\n");
  fprintf(fp, "Guaranteed score: %d \n", best);
  fprintf(fp, "Trap score: %d \n", *(scores + scoresCount));
  fprintf(fp, "Adjusted trap evaluation: %d \n", adj);
  fprintf(fp, "Ply: %d \n", ply);
  fprintf(fp, "Board: \n%s", FEN1);
  PrintBoardFile(b,fp);
  fprintf(fp, "Root Board: \n%s", FEN2);
  PrintBoardFile(c,fp);
  for (i = 0; i<=scoresCount; i++) {
    fprintf(fp, "TScores[%d] = %d\n", i+2, *(scores + i));
  }
  fprintf(fp,"------------------------------------\n");
  fclose(fp);
}
