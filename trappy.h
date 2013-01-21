#ifndef TRAPPY_H
#define TRAPPY_H

/* Enable Trappy Minimax */
#define TRAPPY_DEBUG    0       // show weird debug stuff
#define TRAPPY          1       // enable trappy minimax
#define MAX_DEPTH       7
#define TRAP_METHOD     3       // Methods: 1. median, 2. best value, 3. last value
#define TRAP_SCALE 	1
#define TRAP_CEILING    0.75    // Trap evaluations scaled to never exceed a percentage
								            // of the best guaranteed evaluation.

float trappiness(int M, const int * Scores, int scoreCount, int ply);
float scale(float T, int M);
#endif
