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

#ifndef SETST_SORT_H
#define SETST_SORT_H

#include "banshee.h"
#include "termhash.h"
#include "setst-var.h"

extern region setst_region;
extern term_hash setst_hash;

struct setst_term /* extends gen_e */
{
#ifdef NONSPEC
  sort_kind sort;
#endif
  int type;
  stamp st;
};

typedef struct setst_term *setst_term;

stamp setst_get_stamp(gen_e e);
void setst_inclusion(con_match_fn_ptr,gen_e, gen_e);

gen_e setst_zero(void);
gen_e setst_one(void);
gen_e setst_fresh(const char *name);
gen_e setst_fresh_large(const char *name);
gen_e setst_fresh_small(const char *name);
gen_e setst_constant(const char *name) deletes;
gen_e setst_union(gen_e_list exprs) deletes;
gen_e setst_inter(gen_e_list exprs) deletes;
bool setst_is_zero(gen_e e);
bool setst_is_one(gen_e e);
bool setst_is_var(gen_e e);
bool setst_is_union(gen_e e);
bool setst_is_inter(gen_e e);
bool setst_is_constant(gen_e e);

char *setst_get_constant_name(gen_e e);
gen_e_list setst_get_union(gen_e e);
gen_e_list setst_get_inter(gen_e e);

gen_e_list setst_tlb(gen_e e,incl_fn_ptr setst_incl) deletes;
void setst_set_proj_cache(gen_e e, gen_e elem);
gen_e_list setst_get_proj_cache(gen_e e);


void setst_init(void);
void setst_reset(void) deletes;
void setst_print_stats(FILE *f);

extern struct setst_stats setst_stats;

struct setst_stats
{
  int fresh;
  int fresh_large;
  int fresh_small;

  int distinct_constructors;
  int hashed_constructors;
  int distinct_constants;
  int hashed_constants;
  int distinct_unions;
  int filtered_unions;
  int hashed_unions;
  int distinct_intersections;
  int filtered_intersections;
  int hashed_intersections;

  int redundant_var;
  int redundant_source;
  int redundant_sink;
  
  int added_var;
  int added_source;
  int added_sink;

  int incycle_vars;
  int unchanged_vars;
  int no_sinks;
  
  int cycles_searched;
  int cycles_collapsed;
  int cycles_length;
};


#endif /* SETST_SORT_H */

