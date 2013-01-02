/* Personality Code by Dann Corbit.
 * Merged and edited by Colin Frayn
 * June 2001 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "pers.h"

personality person;

/* Basic data for the parameters */
/* Name of the parameter, enum for the parameter, default, minimum, maximum: */
const phunk phunk_list[] = {
  {"avoid_null_mat", person_avoid_null_mat, 5, 0, 20},
  {"avoid_null_pieces", person_avoid_null_pieces, 3, 0, 10},
  {"avoid_raz_num", person_avoid_raz_num, 4, 3, 5},
  {"back_rank_unsafe", person_back_rank_unsafe, 5, 3, 7},
  {"backward_pawn_1", person_backward_pawn_1, 4, 2, 8},
  {"backward_pawn_2", person_backward_pawn_2, 8, 4, 16},
  {"backward_pawn_3", person_backward_pawn_3, 18, 9, 36},
  {"base_incr", person_base_incr, 15, 10, 20},
  {"bishop_enprise", person_bishop_enprise, 25, 15, 35},
  {"bishop_pinned", person_bishop_pinned, 10, 5, 25},
  {"bishop_score", person_bishop_score, 325, 275, 375},
  {"bishop_tight", person_bishop_tight, 4, 0, 12},
  {"bishop_trapped", person_bishop_trapped, 100, 20, 200},
  {"block_pawns", person_block_pawns, 3, 1, 5},
  {"centre_pawn_bonus", person_centre_pawn_bonus, 3, 1, 5},
  {"check_extend", person_check_extend, 8, 0, 8},
  {"cmthreat_extend", person_cmthreat_extend, 6, 0, 8},
  {"connected_rooks", person_connected_rooks, 5, 3, 8},
  {"dangerous_pp", person_dangerous_pp, 30, 10, 90},
  {"defended_pawn", person_defended_pawn, 5, 2, 20},
  {"defended_q_attack", person_defended_q_attack, 4, 2, 6},
  {"delta_level", person_delta_level, 500, 50, 10000},
  {"doubled_pawns", person_doubled_pawns, 12, 6, 18},
  {"doubled_pawns_iso", person_doubled_pawns_iso, 10, 5, 15},
  {"doubled_rooks", person_doubled_rooks, 8, 5, 15},
  {"draw_score", person_draw_score, 0, -50, 50},
  {"drive_away_long", person_drive_away_long, 4, 2, 6},
  {"drive_away_short", person_drive_away_short, 7, 3, 10},
  {"early_queen", person_early_queen, 8, 0, 20},
  {"early_queen_penalty", person_early_queen_penalty, 2, 0, 5},
  {"early_rook_penalty", person_early_rook_penalty, 4, 0, 6},
  {"easy_move_margin", person_easy_move_margin, 200, 150, 500},
  {"eval_futility", person_eval_futility, 150, 100, 10000},
  {"extend_scale", person_extend_scale, 3, 1, 10},   
  {"extra_minor", person_extra_minor, 150, 0, 300},   
  {"game_length", person_game_length, 50, 40, 70},
  {"half_open_file", person_half_open_file, 4, 2, 8},
  {"half_open_file_k", person_half_open_file_k, 4, 0, 6},
  {"half_open_file_q", person_half_open_file_q, 3, 0, 5},
  {"hostile_blockade", person_hostile_blockade, 5, 3, 9},
  {"ignore_zugzwang", person_ignore_zugzwang, 56, 24, 72},
  {"isolated_pawn", person_isolated_pawn, 18, 9, 36},
  {"king_safety", person_king_safety, 8, 0, 16},
  {"king_tropism", person_king_tropism, 4, 0, 10},
  {"knight_enprise", person_knight_enprise, 25, 15, 55},
  {"knight_pinned", person_knight_pinned, 10, 5, 25},
  {"knight_score", person_knight_score, 320, 270, 370},
  {"knight_trapped", person_knight_trapped, 20, 10, 40},
  {"late_queen_bonus", person_late_queen_bonus, 3, 0, 6},
  {"lone_queen", person_lone_queen, 40, 20, 80},
  {"loss_scale", person_loss_scale, 40, 25, 150},  
  {"max_extend", person_max_extend, 4, 0, 20},
  {"max_qui", person_max_qui, 8, 4, 50},
  {"min_left", person_min_left, 30, 20, 40},
  {"no_centre_pawns", person_no_centre_pawns, 6, 3, 9},
  {"no_king_threat", person_no_king_threat, 25, 0, 50},
  {"no_pawns", person_no_pawns, 5, 0, 12},
  {"onereply_extend", person_onereply_extend, 8, 4, 8},
  {"open_file", person_open_file, 6, 0, 12},
  {"open_file_k", person_open_file_k, 2, 0, 4},
  {"open_file_q", person_open_file_q, 4, 1, 8},
  {"overstretched", person_overstretched, 12, 5, 25},
  {"pawn_block", person_pawn_block, 2, 0, 10},
  {"pawnpush_extend", person_pawnpush_extend, 4, 0, 8},
  {"piece_blockade", person_piece_blockade, 4, 0, 12},
  {"pp_attacked", person_pp_attacked, 6, 0, 20},
  {"pp_storm", person_pp_storm, 20, 0, 50},
  {"queen_7th_rank", person_queen_7th_rank, 14, 7, 28},
  {"queen_attack", person_queen_attack, 3, 0, 9},
  {"queen_attack_def", person_queen_attack_def, 10, 5, 20},
  {"queen_enprise", person_queen_enprise, 60, 30, 90},
  {"queen_immobile", person_queen_immobile, 5, 0, 20},
  {"queen_mobile", person_queen_mobile, 5, 0, 20},
  {"queen_occupied", person_queen_occupied, 80, 40, 120},
  {"queen_pinned", person_queen_pinned, 24, 12, 48},
  {"queen_score", person_queen_score, 930, 800, 1100},
  {"queen_trapped", person_queen_trapped, 4, 0, 8},
  {"razor_harsh", person_razor_harsh, 100, 50, 200},
  {"razor_margin", person_razor_margin, 20, 0, 100},
  {"razor_scale", person_razor_scale, 25, 10, 50},
  {"recap_extend", person_recap_extend, 4, 2, 8},
  {"revcheck_extend", person_revcheck_extend, 2, 0, 8},
  {"reward_castle", person_reward_castle, 8, 0, 16},
  {"rook_7th_rank", person_rook_7th_rank, 16, 6, 36},
  {"rook_attack", person_rook_attack, 4, 0, 14},
  {"rook_behind_pp", person_rook_behind_pp, 14, 7, 28},
  {"rook_blocked", person_rook_blocked, 3, 0, 15},
  {"rook_boxed_in", person_rook_boxed_in, 8, 0, 100},
  {"rook_enprise", person_rook_enprise, 40, 10, 80},
  {"rook_occupied", person_rook_occupied, 50, 25, 80},
  {"rook_pinned", person_rook_pinned, 12, 6, 24},
  {"rook_score", person_rook_score, 500, 400, 600},
  {"rook_trapped", person_rook_trapped, 30, 10, 60},
  {"shield_one", person_shield_one, 5, 2, 10},
  {"shield_two", person_shield_two, 2, 1, 4},
  {"shield_zero", person_shield_zero, 8, 4, 16},
  {"side_attack", person_side_attack, 8, 4, 16},
  {"space_defended", person_space_defended, 8, 0, 16},
  {"space_won", person_space_won, 4, 0, 8},
  {"spoilt_castle_1", person_spoilt_castle_1, 8, 4, 16},
  {"spoilt_castle_2", person_spoilt_castle_2, 8, 4, 16},
  {"two_bishops", person_two_bishops, 15, 0, 45},
  {"undeveloped", person_undeveloped, 10, 5, 20},
  {"unstoppable_pp", person_unstoppable_pp, 400, 100, 600},
  {"use_delta", person_use_delta, 0, 0, 1},
  {"use_eval_sc", person_use_eval_sc, 1, 0, 1},
  {"use_hash", person_use_hash, 1, 0, 1},
  {"use_history", person_use_history, 1, 0, 1},
  {"use_iid", person_use_iid, 1, 0, 1},
  {"use_killers", person_use_killers, 1, 0, 1},
  {"use_null", person_use_null, 1, 0, 1},
  {"use_razoring", person_use_razoring, 1, 0, 1},
  {"use_see", person_use_see, 1, 0, 1},
  {"use_verification", person_use_verification, 1, 0, 1},
  {"use_window", person_use_window, 1, 0, 1},
  {"v_dangerous_pp", person_v_dangerous_pp, 150, 75, 300},
  {"window", person_window, 29, 10, 80},
  {NULL, person_oops_too_far, 0, 0, 0}
};

/* Setup default values for the personality if no input is specified */
void init_personality(void) {
  int start;
  phunk ph;

  for (start = 0;; start++) {
    ph = phunk_list[start];
    if (ph.p == NULL) break;
    if (ph.def > ph.maxval || ph.def < ph.minval) {
      fprintf(stdout,"Strange Default Value for %s  (%d)\n",ph.p,ph.def);
    }
    PersonSetVal(&ph, ph.def);
  }
}

/* Check that a personality trait is within range */
void check_range(int *val, phunk p) {
  if (*val > p.maxval) {
    printf("\n%s was outside the allowed range of %d to %d (given at %d) and was reset to %d\n",  p.p, p.minval, p.maxval, *val, p.maxval);
    *val = p.maxval;
  }
  if (*val < p.minval) {
    printf("\n%s was outside the allowed range of %d to %d (given at %d) and was reset to %d\n",  p.p, p.minval, p.maxval, *val, p.minval);
    *val = p.minval;
  }
}

/* Read in a personality file */
int read_person(FILE * pfile) {
  char string[4096], param[4096], *ch;
  int  start, pos;
  phunk ph;
  int found_something = 0;

  if (pfile == NULL) {
    puts("Error reading from personality file");
    return 0;
  }
  while (fgets(string, sizeof string, pfile)) {
    int found = 0;

    ch = string;
    while (*ch != ';') ch++;
    while (*(++ch) == ' ');
     // Backwards compatibility
    if (strstr(ch,"person.")) ch += 7;
    strcpy(param,ch);

    pos=0;
    while (param[pos]!=' ' && param[pos]!='\n' && param[pos]!=13) pos++;
    param[pos]=0;

    start = 0;
    for (start = 0;; start++) {
      ph = phunk_list[start];
      if (ph.p == NULL) break;
      found = (strcmp(param, ph.p) == 0);
      if (found) break;
    }
    if (!found) printf("\nERROR reading personality.  Did not understand file entry %s\n", string);
    else {
      int val = atoi(string);
      found_something = 1;
      check_range(&val, ph);
      PersonSetVal(&ph,val);
    }
  }
  return found_something;
}

/* Set the value for a personality file entry */
void PersonSetVal(phunk *ph, int val) {
  switch (ph->e) {
  case person_avoid_null_mat: person.avoid_null_mat = val; break;
  case person_avoid_null_pieces: person.avoid_null_pieces = val; break;
  case person_avoid_raz_num: person.avoid_raz_num = val; break;
  case person_back_rank_unsafe: person.back_rank_unsafe = val; break;
  case person_backward_pawn_1: person.backward_pawn_1 = val; break;
  case person_backward_pawn_2: person.backward_pawn_2 = val; break;
  case person_backward_pawn_3: person.backward_pawn_3 = val; break;
  case person_base_incr: person.base_incr = val; break;
  case person_bishop_enprise: person.bishop_enprise = val; break;
  case person_bishop_pinned: person.bishop_pinned = val; break;
  case person_bishop_score: person.bishop_score = val; break;
  case person_bishop_tight: person.bishop_tight = val; break;
  case person_bishop_trapped: person.bishop_trapped = val; break;
  case person_block_pawns: person.block_pawns = val; break;
  case person_centre_pawn_bonus: person.centre_pawn_bonus = val; break;
  case person_check_extend: person.check_extend = val; break;
  case person_cmthreat_extend: person.cmthreat_extend = val; break;
  case person_connected_rooks: person.connected_rooks = val; break;
  case person_dangerous_pp: person.dangerous_pp = val; break;
  case person_defended_pawn: person.defended_pawn= val; break;
  case person_defended_q_attack: person.defended_q_attack = val; break;
  case person_delta_level: person.delta_level = val; break;
  case person_doubled_pawns: person.doubled_pawns = val; break;
  case person_doubled_pawns_iso: person.doubled_pawns_iso = val; break;
  case person_doubled_rooks: person.doubled_rooks = val; break;
  case person_draw_score: person.draw_score = val; break;
  case person_drive_away_long: person.drive_away_long = val; break;
  case person_drive_away_short: person.drive_away_short = val; break;
  case person_early_queen: person.early_queen = val; break;
  case person_early_queen_penalty: person.early_queen_penalty = val; break;
  case person_early_rook_penalty: person.early_rook_penalty = val; break;
  case person_easy_move_margin: person.easy_move_margin = val; break;
  case person_eval_futility: person.eval_futility = val; break;
  case person_extend_scale: person.extend_scale = val; break;
  case person_extra_minor: person.extra_minor = val; break;
  case person_game_length: person.game_length = val; break;
  case person_half_open_file: person.half_open_file = val; break;
  case person_half_open_file_k: person.half_open_file_k = val; break;
  case person_half_open_file_q: person.half_open_file_q = val; break;
  case person_hostile_blockade: person.hostile_blockade = val; break;
  case person_ignore_zugzwang: person.ignore_zugzwang = val; break;
  case person_isolated_pawn: person.isolated_pawn = val; break;
  case person_king_safety: person.king_safety = val; break;
  case person_king_tropism: person.king_tropism = val; break;
  case person_knight_enprise: person.knight_enprise = val; break;
  case person_knight_pinned: person.knight_pinned = val; break;
  case person_knight_score: person.knight_score = val; break;
  case person_knight_trapped: person.knight_trapped = val; break;
  case person_late_queen_bonus: person.late_queen_bonus = val; break;
  case person_lone_queen: person.lone_queen = val; break;
  case person_loss_scale: person.loss_scale = val; break;
  case person_max_extend: person.max_extend = val; break;
  case person_max_qui: person.max_qui = val; break;
  case person_min_left: person.min_left = val; break;
  case person_no_centre_pawns: person.no_centre_pawns = val; break;
  case person_no_king_threat: person.no_king_threat = val; break;
  case person_no_pawns: person.no_pawns = val; break;
  case person_onereply_extend: person.onereply_extend = val; break;
  case person_open_file: person.open_file = val; break;
  case person_open_file_k: person.open_file_k = val; break;
  case person_open_file_q: person.open_file_q = val; break;
  case person_overstretched: person.overstretched = val; break;
  case person_pawn_block: person.pawn_block = val; break;
  case person_pawnpush_extend: person.pawnpush_extend = val; break;
  case person_piece_blockade: person.piece_blockade = val; break;
  case person_pp_attacked: person.pp_attacked = val; break;
  case person_pp_storm: person.pp_storm = val; break;
  case person_queen_7th_rank: person.queen_7th_rank = val; break;
  case person_queen_attack: person.queen_attack = val; break;
  case person_queen_attack_def: person.queen_attack_def = val; break;
  case person_queen_enprise: person.queen_enprise = val; break;
  case person_queen_immobile: person.queen_immobile = val; break;
  case person_queen_mobile: person.queen_mobile = val; break;
  case person_queen_occupied: person.queen_occupied = val; break;
  case person_queen_pinned: person.queen_pinned = val; break;
  case person_queen_score: person.queen_score = val; break;
  case person_queen_trapped: person.queen_trapped = val; break;
  case person_razor_harsh: person.razor_harsh = val; break;
  case person_razor_margin: person.razor_margin = val; break;
  case person_razor_scale: person.razor_scale = val; break;
  case person_recap_extend: person.recap_extend = val; break;
  case person_revcheck_extend: person.revcheck_extend = val; break;
  case person_reward_castle: person.reward_castle = val; break;
  case person_rook_7th_rank: person.rook_7th_rank = val; break;
  case person_rook_attack: person.rook_attack = val; break;
  case person_rook_behind_pp: person.rook_behind_pp = val; break;
  case person_rook_blocked: person.rook_blocked = val; break;
  case person_rook_boxed_in: person.rook_boxed_in = val; break;
  case person_rook_enprise: person.rook_enprise = val; break;
  case person_rook_occupied: person.rook_occupied = val; break;
  case person_rook_pinned: person.rook_pinned = val; break;
  case person_rook_score: person.rook_score = val; break;
  case person_rook_trapped: person.rook_trapped = val; break;
  case person_shield_one: person.shield_one = val; break;
  case person_shield_two: person.shield_two = val; break;
  case person_shield_zero: person.shield_zero = val; break;
  case person_side_attack: person.side_attack = val; break;
  case person_space_defended: person.space_defended = val; break;
  case person_space_won: person.space_won = val; break;
  case person_spoilt_castle_1: person.spoilt_castle_1 = val; break;
  case person_spoilt_castle_2: person.spoilt_castle_2 = val; break;
  case person_two_bishops: person.two_bishops = val; break;
  case person_undeveloped: person.undeveloped = val; break;
  case person_unstoppable_pp: person.unstoppable_pp = val; break;
  case person_use_delta: person.use_delta = val; break;
  case person_use_eval_sc: person.use_eval_sc = val; break;
  case person_use_hash: person.use_hash = val; break;
  case person_use_history: person.use_history = val; break;
  case person_use_iid: person.use_iid = val; break;
  case person_use_killers: person.use_killers = val; break;
  case person_use_null: person.use_null = val; break;
  case person_use_razoring: person.use_razoring = val; break;
  case person_use_see: person.use_see = val; break;
  case person_use_verification: person.use_verification = val; break;
  case person_use_window: person.use_window = val; break;
  case person_v_dangerous_pp: person.v_dangerous_pp = val; break;
  case person_window: person.window = val; break;
  default: fprintf(stderr,"Could Not Set Parameter %s\n",ph->p); break;
  }
}

