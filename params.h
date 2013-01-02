/**********************************
 *    params.h                    *
 *    Colin Frayn                 *
 *    June 2001                   *
 **********************************/

/*
   Contains the specific parameters for the evaluation function.
   This is used in tactics.c and eval.c.
   Also contains the main algorithm parameters for comp.c.
*/

#include "pers.h"

#ifndef PARAMS_H
#define PARAMS_H

/*
        **================================**
        ||                                ||
    ====||      Algorithm Parameters      ||====
        ||                                ||
        **================================**
*/


/* Move ordering and miscellaneous comp.c stuff.  Feel free to fiddle
 * with these numbers. */

#define GOOD_CAPTURE      (10000)
#define MATEKILLER        (3000)
#define KILLER1           (2000)
#define KILLER2           (1000)
#define ORDINARY_MOVE     (5000)  /* Penalty */

#define MAX_EXTEND        (person.max_extend)
#define MAX_QUI           (person.max_qui)

/* Score for a draw.  Normally zero unless you want the program
 * to play with contempt */
#define DRAW_SCORE        (person.draw_score)

/* Margin for an easy move */
#define EASY_MOVE_MARGIN  (person.easy_move_margin)

/*  -------===   ALGORITHM SWITCHES   ===-------  */

/* Use the various algorithms? 1=ON / 0=OFF */
#define USE_HISTORY       (person.use_history)      /* History heuristic */
#define USE_KILLERS       (person.use_killers)      /* Killer Move Heuristic */
#define USE_HASH          (person.use_hash)         /* Hash Table */
#define USE_IID           (person.use_iid)          /* Internal Iterative Deepening */
#define USE_WINDOW        (person.use_window)       /* Aspiration Window Search */
#define USE_SEE           (person.use_see)          /* Static Exchange Evaluator */
#define USE_DELTA         (person.use_delta)        /* Delta Cuts */
#define USE_RAZORING      (person.use_razoring)     /* Razoring of Uninteresting Branches */
#define USE_EVAL_SC       (person.use_eval_sc)      /* Eval() short-circuit cutoffs */
#define USE_NULL          (person.use_null)         /* Null Move Algorithm */
#define USE_VERIFICATION  (person.use_verification) /* Verification search in NULL move heuristic */

/*  -------===  FRACTIONAL EXTENSION SETTINGS   ===-------  */

#define CHECK_EXTEND      (person.check_extend)
#define ONEREPLY_EXTEND   (person.onereply_extend)
#define CMTHREAT_EXTEND   (person.cmthreat_extend)
#define PAWNPUSH_EXTEND   (person.pawnpush_extend)
#define RECAP_EXTEND      (person.recap_extend)
#define REVCHECK_EXTEND   (person.revcheck_extend)

/*   -------===   MINIMAL WINDOW SEARCH SETTINGS   ===-------  */

/* Search window for the reduced window search (PAWN=100).  The smaller this
 * is the more effective your pruning will be.  However, if this is too
 * small then you're going to get too many re-searches and the overall gain
 * will be much smaller. Good values are 20-40.  Since v2.0, Beowulf has used the
 * procedure of 'adaptive search window' searching.  Start off with a low guess
 * for the window, because in most positions the variation is low.  Then if
 * the search fails high or low at any point, simple increase the window size by
 * the value WINDOW_STEP. */
#define WINDOW            (person.window)

/*  -----===   NULL MOVE   ===----- */

/* Material left on the board (for the side to play) beneath which we avoid NULL
 * move extensions for fear of zugzwang.  In the late endgame, most games are
 * already effectively won or lost anyway, so the tiny extra help from NULL move
 * is not really needed. */
#define AVOID_NULL_MAT    (person.avoid_null_mat)
/* Also avoid NULL move if you have fewer pieces left than ... */
#define AVOID_NULL_PIECES (person.avoid_null_pieces)
/* Ignore zugzwang when searching beneath this depth */
#define IGNORE_ZUGZWANG   (person.ignore_zugzwang)

/*  -----===   RAZORING SETTINGS   ===----- */

/* Avoid razoring (if it's switched on) for the first # moves at any node */
#define AVOID_RAZ_NUM     (person.avoid_raz_num)
/* Scale of the razoring algorithm.  The higher this number the safer it is,
 * but the less it prunes the tree.  20 - 30 is a fairly good range.  Setting
 * it below 15 will be dangerous, and setting it above 50 will be mostly
 * pointless, effectively turning it off. */
#define RAZOR_SCALE       (person.razor_scale)
/* Extra safety margin for the razoring algorithm. */
#define RAZOR_MARGIN      (person.razor_margin)
/* If the position deficit is more than this value then prune by a whole ply,
 * otherwise use fractional ply reduction. */
#define RAZOR_HARSH       (person.razor_harsh)

/*  -------===   DELTA CUT SETTINGS ===------ */

/* Prune quiescence search moves which look like they will not get within this
 * value of alpha after the full capture exchange.  There is a theoretical problem
 * with this method which I'm worried about.  Imagine a capture of queen with rook,
 * followed by recapture with a pawn (and no further captures).  The second capture
 * will almost certainly be delta-cut and therefore the original queen capture will
 * score much higher than it should do because it doesn't see the loss of the rook
 * and instead the stand pat score returned at the next level. */
#define DELTA_LEVEL       (person.delta_level)

/*  ------===   EVAL CUTOFF SETTINGS   ===-------  */

/* Futility level for eval() cutoffs.  Preliminary scores which are not within
 * this value of the A-B window are not evaluated further.  This avoids doing
 * expensive positional evals for positions which are clearly inferior. */
#define EVAL_FUTILITY     (person.eval_futility)

/*  ------===   TIME CONTROL SETTINGS   ===-------  */

#define BASE_INCR         (person.base_incr)    /* Base of search time extension factor */
#define LOSS_SCALE        (person.loss_scale)   /* Scale of time extension algorithm */
#define GAME_LENGTH       (person.game_length)  /* Estimated average game length (full moves) */
#define MIN_LEFT          (person.min_left)     /* Minimum estimated remaining moves */
#define EXTEND_SCALE      (person.extend_scale) /* Average duration between random extensions */


/*
        **================================**
        ||                                ||
    ====||       Scoring Parameters       ||====
        ||                                ||
        **================================**
*/

/* Piece values */

#define PAWN_SCORE          (100)  /* This is hardcoded */
#define ROOK_SCORE          (person.rook_score)
#define KNIGHT_SCORE        (person.knight_score)
#define BISHOP_SCORE        (person.bishop_score)
#define QUEEN_SCORE         (person.queen_score)

/* Pawn Location */

#define DEFENDED_PAWN       (person.defended_pawn)

#define DOUBLED_PAWNS       (person.doubled_pawns)     /* Penalty */
#define DOUBLED_PAWNS_ISO   (person.doubled_pawns_iso) /* Penalty */
#define HOSTILE_BLOCKADE    (person.hostile_blockade)  /* Penalty */
#define PIECE_BLOCKADE      (person.piece_blockade)    /* Penalty */
#define ISOLATED_PAWN       (person.isolated_pawn)     /* Penalty */

#define BACKWARD_PAWN_1     (person.backward_pawn_1)   /* Penalty */
#define BACKWARD_PAWN_2     (person.backward_pawn_2)   /* Penalty */
#define BACKWARD_PAWN_3     (person.backward_pawn_3)   /* Penalty */

#define UNSTOPPABLE_PP      (person.unstoppable_pp)
#define KING_SAFETY         (person.king_safety)
#define KING_TROPISM        (person.king_tropism)
#define DANGEROUS_PP        (person.dangerous_pp)
#define V_DANGEROUS_PP      (person.v_dangerous_pp)
#define PP_STORM            (person.pp_storm)
#define PP_ATTACKED         (person.pp_attacked)      /* Penalty */

#define PAWN_BLOCK          (person.pawn_block)       /* Penalty Factor */
#define NO_CENTRE_PAWNS     (person.no_centre_pawns)  /* Penalty */
#define CENTRE_PAWN_BONUS   (person.centre_pawn_bonus)
#define NO_PAWNS            (person.no_pawns)

/* Piece Location */

#define DOUBLED_ROOKS       (person.doubled_rooks)
#define ROOK_ATTACK         (person.rook_attack)
#define HALF_OPEN_FILE      (person.half_open_file)
#define OPEN_FILE           (person.open_file)
#define ROOK_7TH_RANK       (person.rook_7th_rank)
#define CONNECTED_ROOKS     (person.connected_rooks)
#define ROOK_BEHIND_PP      (person.rook_behind_pp)
#define EARLY_ROOK_PENALTY  (person.early_rook_penalty) /* Penalty */
#define BLOCK_PAWNS         (person.block_pawns)        /* Penalty */
#define ROOK_BLOCKED        (person.rook_blocked)       /* Penalty */
#define ROOK_BOXED_IN       (person.rook_boxed_in)      /* Penalty */
#define ROOK_PINNED         (person.rook_pinned)        /* Penalty */
#define ROOK_OCCUPIED       (person.rook_occupied)      /* Penalty */
#define ROOK_TRAPPED        (person.rook_trapped)       /* Penalty */

#define HALF_OPEN_FILE_Q    (person.half_open_file_q)
#define OPEN_FILE_Q         (person.open_file_q)
#define LONE_QUEEN          (person.lone_queen)
#define QUEEN_ATTACK        (person.queen_attack)
#define QUEEN_ATTACK_DEF    (person.queen_attack_def)
#define QUEEN_7TH_RANK      (person.queen_7th_rank)
#define LATE_QUEEN_BONUS    (person.late_queen_bonus)    /* Bonus Modifier */ 
#define EARLY_QUEEN         (person.early_queen)         /* Penalty */
#define EARLY_QUEEN_PENALTY (person.early_queen_penalty) /* Penalty Modifier */
#define QUEEN_TRAPPED       (person.queen_trapped)       /* Penalty */
#define QUEEN_MOBILE        (person.queen_mobile)        /* Penalty */
#define QUEEN_IMMOBILE      (person.queen_immobile)      /* Penalty */
#define QUEEN_PINNED        (person.queen_pinned)        /* Penalty */
#define QUEEN_OCCUPIED      (person.queen_occupied)      /* Penalty */
#define DEFENDED_Q_ATTACK   (person.defended_q_attack)   /* Penalty */

#define DRIVE_AWAY_SHORT    (person.drive_away_short)    /* Penalty */
#define DRIVE_AWAY_LONG     (person.drive_away_long)     /* Penalty */
#define BISHOP_TIGHT        (person.bishop_tight)        /* Penalty */
#define BISHOP_TRAPPED      (person.bishop_trapped)      /* Penalty */
#define KNIGHT_TRAPPED      (person.knight_trapped)      /* Penalty */
#define UNDEVELOPED         (person.undeveloped)         /* Penalty */
#define OVERSTRETCHED       (person.overstretched)       /* Penalty */
#define EXTRA_MINOR         (person.extra_minor) 
#define BISHOP_PINNED       (person.bishop_pinned)       /* Penalty */
#define KNIGHT_PINNED       (person.knight_pinned)       /* Penalty */

/* King Safety */

#define HALF_OPEN_FILE_K    (person.half_open_file_k)  /* Penalty */
#define OPEN_FILE_K         (person.open_file_k)       /* Penalty */
#define SHIELD_ZERO         (person.shield_zero)       /* Penalty */
#define SHIELD_ONE          (person.shield_one)        /* Penalty */
#define SHIELD_TWO          (person.shield_two)        /* Penalty */
#define SIDE_ATTACK         (person.side_attack)       /* Penalty */
#define BACK_RANK_UNSAFE    (person.back_rank_unsafe)  /* Penalty */

#define NO_KING_THREAT      (person.no_king_threat)

/* Modifiers for pieces which are en-prise and are attacked
 * more than they are defended.  Note that en-prise pieces
 * qualify for both of these penalties simultaneously. 
 * Note also that just because a piece is attacked more than
 * it is defended, this doesn't mean that the exchange would
 * be favourable for the attacker....*/

#define ROOK_ENPRISE        (person.rook_enprise)    /* Penalty */
#define KNIGHT_ENPRISE      (person.knight_enprise)  /* Penalty */
#define BISHOP_ENPRISE      (person.bishop_enprise)  /* Penalty */
#define QUEEN_ENPRISE       (person.queen_enprise)   /* Penalty */

/* General positional scoring */

#define TWO_BISHOPS         (person.two_bishops)
#define REWARD_CASTLE       (person.reward_castle)
#define SPOILT_CASTLE_1     (person.spoilt_castle_1)  /* Penalty */
#define SPOILT_CASTLE_2     (person.spoilt_castle_2)  /* Penalty */
#define SPACE_WON           (person.space_won)
#define SPACE_DEFENDED      (person.space_defended)

#endif /* PARAMS_H */
