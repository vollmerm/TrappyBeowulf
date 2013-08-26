#ifndef TRAPPY_H
#define TRAPPY_H

#include "moves.h"
#include "board.h"
#include "common.h"
#include "utils.h"
#include <unistd.h>
#include <fcntl.h>

/* Enable Trappy Minimax */
#define TRAPPY_DEBUG    0      // show weird debug stuff
#define WRITE_TOP_TRAP  1      // show top-level traps
#define TRAPPY          1      // enable trappy minimax
#define MAX_DEPTH       5
#define TRAP_METHOD     1      // Methods: 1. median, 2. best value, 3. last value
#define TRAP_SCALE 	    0
#define NO_AB
//#define NO_ORDER
//#define NO_QUIESCE
#define TRAP_CEILING    0.5    // Trap evaluations scaled to never exceed a percentage
			       // of the best guaranteed evaluation.
#define TRAP_KEY_SIZE   151
#define MAX_TRAP_DEPTH  1
float trappiness(int M, const int * Scores, int scoreCount, int ply);
float scale(float T, int M);
void writeTrapData(int, int);
void WriteBoardData(MOVE trapm, MOVE bestm, Board b, Board c, int best, int adj, 
    int *scores, int scoresCount, int ply);
void WriteBoardDataLite(Board b, MOVE top, MOVE best, MOVE trap, int bestscore, int trapscore, int profit);
void WriteMarker();
#endif
