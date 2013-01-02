/* Personality Code by Dann Corbit
 * Merged and Edited by Colin Frayn
 * 20th June 2001 */

#ifndef PERSONALITY_DATA_DEFINED
#define PERSONALITY_DATA_DEFINED
/*
  These are the variables used for personality calculations.
  Basically, it is nothing more than the values used for scoring
  and the try-it/don't-try-it flags.
*/
typedef struct tag_pers {
  int avoid_null_mat;
  int avoid_null_pieces;
  int avoid_raz_num;
  int back_rank_unsafe;
  int backward_pawn_1;
  int backward_pawn_2;
  int backward_pawn_3;
  int base_incr;
  int bishop_enprise;
  int bishop_pinned;
  int bishop_score;
  int bishop_tight;
  int bishop_trapped;
  int block_pawns;
  int centre_pawn_bonus;
  int check_extend;
  int cmthreat_extend;
  int connected_rooks;
  int dangerous_pp;
  int defended_pawn;
  int defended_q_attack;
  int delta_level;
  int doubled_pawns;
  int doubled_pawns_iso;
  int doubled_rooks;
  int draw_score;
  int drive_away_long;
  int drive_away_short;
  int early_queen;
  int early_queen_penalty;
  int early_rook_penalty;
  int easy_move_margin;
  int eval_futility;
  int extend_scale;
  int extra_minor;
  int game_length;
  int half_open_file;
  int half_open_file_k;
  int half_open_file_q;
  int hostile_blockade;
  int ignore_zugzwang;
  int isolated_pawn;
  int king_safety;
  int king_tropism;
  int knight_enprise;
  int knight_pinned;
  int knight_score;
  int knight_trapped;
  int late_queen_bonus;
  int lone_queen;
  int loss_scale;
  int max_extend;
  int max_qui;
  int min_left;
  int no_centre_pawns;
  int no_king_threat;
  int no_pawns;
  int onereply_extend;
  int open_file;
  int open_file_k;
  int open_file_q;
  int overstretched;
  int pawn_block;
  int pawnpush_extend;
  int piece_blockade;
  int pp_attacked;
  int pp_storm;
  int queen_7th_rank;
  int queen_attack;
  int queen_attack_def;
  int queen_enprise;
  int queen_immobile;
  int queen_mobile;
  int queen_occupied;
  int queen_pinned;
  int queen_score;
  int queen_trapped;
  int razor_harsh;
  int razor_margin;
  int razor_scale;
  int recap_extend;
  int revcheck_extend;
  int reward_castle;
  int rook_7th_rank;
  int rook_attack;
  int rook_behind_pp;
  int rook_blocked;
  int rook_boxed_in;
  int rook_enprise;
  int rook_occupied;
  int rook_pinned;
  int rook_score;
  int rook_trapped;
  int shield_one;
  int shield_two;
  int shield_zero;
  int side_attack;
  int space_defended;
  int space_won;
  int spoilt_castle_1;
  int spoilt_castle_2;
  int two_bishops;
  int undeveloped;
  int unstoppable_pp;
  int use_delta;
  int use_eval_sc;
  int use_hash;
  int use_history;
  int use_iid;
  int use_killers;
  int use_null;
  int use_razoring;
  int use_see;
  int use_verification;
  int use_window;
  int v_dangerous_pp;
  int window;
} personality;

 /* Enum for the personality entry types */
enum pft {
  person_avoid_null_mat = 1,
  person_avoid_null_pieces,
  person_avoid_raz_num,
  person_back_rank_unsafe,
  person_backward_pawn_1,
  person_backward_pawn_2,
  person_backward_pawn_3,
  person_base_incr,
  person_bishop_enprise,
  person_bishop_pinned,
  person_bishop_score,
  person_bishop_tight,
  person_bishop_trapped,
  person_block_pawns,
  person_centre_pawn_bonus,
  person_check_extend,
  person_cmthreat_extend,
  person_connected_rooks,
  person_dangerous_pp,
  person_defended_pawn,
  person_defended_q_attack,
  person_delta_level,
  person_doubled_pawns,
  person_doubled_pawns_iso,
  person_doubled_rooks,
  person_draw_score,
  person_drive_away_long,
  person_drive_away_short,
  person_early_queen,
  person_early_queen_penalty,
  person_early_rook_penalty,
  person_easy_move_margin,
  person_eval_futility,
  person_extend_scale,
  person_extra_minor,
  person_game_length,
  person_half_open_file,
  person_half_open_file_k,
  person_half_open_file_q,
  person_hostile_blockade,
  person_ignore_zugzwang,
  person_isolated_pawn,
  person_killer1,
  person_killer2,
  person_king_safety,
  person_king_tropism,
  person_knight_enprise,
  person_knight_pinned,
  person_knight_score,
  person_knight_trapped,
  person_late_queen_bonus,
  person_lone_queen,
  person_loss_scale,
  person_max_extend,
  person_max_qui,
  person_min_left,
  person_no_centre_pawns,
  person_no_king_threat,
  person_no_pawns,
  person_onereply_extend,
  person_open_file,
  person_open_file_k,
  person_open_file_q,
  person_overstretched,
  person_pawn_block,
  person_pawnpush_extend,
  person_piece_blockade,
  person_pp_attacked,
  person_pp_storm,
  person_queen_7th_rank,
  person_queen_attack,
  person_queen_attack_def,
  person_queen_enprise,
  person_queen_immobile,
  person_queen_mobile,
  person_queen_occupied,
  person_queen_pinned,
  person_queen_score,
  person_queen_trapped,
  person_razor_harsh,
  person_razor_margin,
  person_razor_scale,
  person_recap_extend,
  person_revcheck_extend,
  person_reward_castle,
  person_rook_7th_rank,
  person_rook_attack,
  person_rook_behind_pp,
  person_rook_blocked,
  person_rook_boxed_in,
  person_rook_enprise,
  person_rook_occupied,
  person_rook_pinned,
  person_rook_score,
  person_rook_trapped,
  person_shield_one,
  person_shield_two,
  person_shield_zero,
  person_side_attack,
  person_space_defended,
  person_space_won,
  person_spoilt_castle_1,
  person_spoilt_castle_2,
  person_two_bishops,
  person_undeveloped,
  person_unstoppable_pp,
  person_use_delta,
  person_use_eval_sc,
  person_use_hash,
  person_use_history,
  person_use_iid,
  person_use_killers,
  person_use_null,
  person_use_razoring,
  person_use_see,
  person_use_verification,
  person_use_window,
  person_v_dangerous_pp,
  person_window,
  person_oops_too_far
};

/* Personality type data entry */
typedef struct tag_phunk {
  const char *p; /* String title of the parameter */
  enum pft e; /* Enum representing the parameter */
  int def; /* Default value */
  int minval; /* Minimum value allowed for the parameter */
  int maxval; /* Maximum value allowed for the parameter */
} phunk;


extern personality person;

extern void init_personality(void );
extern int write_person(FILE *pfile);
extern int read_person(FILE *pfile);
void PersonSetVal(phunk *, int);

#endif /* PERSONALITY_DATA_DEFINED */
