/*
 * Copyright (c) 2000-2001
 *      The Regents of the University of California.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 */

#ifndef FLOWROW_SORT_H
#define FLOWROW_SORT_H


#include "stdio.h"
#include "banshee.h"
#include "termhash.h"
#include "flow-var.h"

struct flowrow_field
{
  char *label;
  gen_e expr;
};

typedef struct flowrow_field *flowrow_field;

DECLARE_LIST(flowrow_map,flowrow_field)

extern region flowrow_region;

void flowrow_inclusion(fresh_fn_ptr fresh, get_stamp_fn_ptr get_stamp, 
		       incl_fn_ptr field_incl,gen_e zero_elem, gen_e e1, 
		       gen_e e2) deletes;

gen_e flowrow_row(get_stamp_fn_ptr get_stamp,flowrow_map fields, gen_e rest) deletes;

gen_e flowrow_extract_field(const char *name, gen_e e);
gen_e flowrow_extract_rest(gen_e e);
flowrow_map flowrow_extract_fields(gen_e e);

stamp flowrow_get_stamp(gen_e e);

#ifndef NONSPEC
gen_e flowrow_zero(void);
gen_e flowrow_one(void);
gen_e flowrow_abs(void);
gen_e flowrow_wild(void);
gen_e flowrow_fresh(const char *name);
gen_e flowrow_fresh_small(const char *name);
gen_e flowrow_fresh_large(const char *name);
#else
sort_kind flowrow_base_sort(gen_e e);
gen_e flowrow_zero(sort_kind base_sort);
gen_e flowrow_one(sort_kind base_sort);
gen_e flowrow_abs(sort_kind base_sort);
gen_e flowrow_wild(sort_kind base_sort);
gen_e flowrow_fresh(sort_kind base_sort);
gen_e flowrow_fresh_small(sort_kind base_sort);
gen_e flowrow_fresh_large(sort_kind base_sort);
#endif

bool flowrow_is_zero(gen_e e);
bool flowrow_is_one(gen_e e);
bool flowrow_is_abs(gen_e e);
bool flowrow_is_wild(gen_e e);
bool flowrow_is_var(gen_e e);
bool flowrow_is_row(gen_e e);
bool flowrow_is_alias(gen_e e);


void flowrow_init(void);
void flowrow_reset(void) deletes;

typedef void (* field_print_fn_ptr) (FILE *f,gen_e e) deletes;

void flowrow_print(FILE *f,get_stamp_fn_ptr get_stamp,
		   field_print_fn_ptr field_print,gen_e e) deletes;
void flowrow_print_stats(FILE *f);

extern struct flowrow_stats flowrow_stats;

struct flowrow_stats
{
  int fresh;
  int fresh_small;
  int fresh_large;

  int rows_disjoint_wild;
  int rows_equal;
  int rows_zero_one_wild;
  int rows_l_inductive;
  int rows_r_inductive;
  int rows_disjoint_r1_minimal;
  int rows_disjoint_r1_var_r2_minimal;
  int rows_disjoint_r1_var_r2_maximal;
  int rows_disjoint_r1_var_r2_closed;
  int rows_disjoint_r1_var_r2_var_lt;
  int rows_disjoint_r1_var_r2_var_gt;
  int rows_equal_domains;
  int rows_nonempty_intersection;
  int rows_fresh;
  int rows_fresh_large;
};

#endif /* FLOWROW_H */







